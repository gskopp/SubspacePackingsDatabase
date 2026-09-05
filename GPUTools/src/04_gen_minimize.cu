// 04_gen_minimize.cu — batched-restart potential minimization on the GPU.
//
// B independent random d×n frames descend the p-frame potential simultaneously.
// Each block owns one seed's frame in shared memory; one gradient step per
// iteration; per-seed adaptive step size; converged seeds masked. Returns the
// lowest-coherence frame to the host.
//
// Objective per seed: Phi in C^{d×n}, unit columns, potential = Σ_{i<j} |<v_i,v_j>|^p.
// Gradient wrt column i: Σ_{j≠i} p|g_ij|^{p-2} conj(g_ij) v_j  (g_ij=<v_i,v_j>).
// Step, renormalize columns, repeat.

#include "04_gen_minimize.cuh"
#include <cuComplex.h>
#include <cuda_runtime.h>
#include <curand_kernel.h>
#include <cstdio>
#include <vector>
#include <cmath>

#define CUDA_CHECK(x) do{ cudaError_t e=(x); if(e!=cudaSuccess){ \
  fprintf(stderr,"CUDA %s:%d %s\n",__FILE__,__LINE__,cudaGetErrorString(e)); \
  return {Mat(d,n),1e9,false}; } }while(0)

namespace {

__device__ __forceinline__ cuDoubleComplex cadd(cuDoubleComplex a,cuDoubleComplex b){return cuCadd(a,b);}
__device__ __forceinline__ cuDoubleComplex cmul(cuDoubleComplex a,cuDoubleComplex b){return cuCmul(a,b);}

// one block per seed; frame Phi (d*n) in shared memory, column-major (d rows).
// blockDim.x threads cooperate over columns.
extern "C" __global__
void minimize_kernel(cuDoubleComplex* Phi_out, int d, int n, int B, double p,
                     int max_iter, double tol, double welch, unsigned long long seed,
                     double* coh_out){
  int b=blockIdx.x; if(b>=B) return;
  extern __shared__ cuDoubleComplex Phi[];   // d*n for this seed
  int tid=threadIdx.x, nt=blockDim.x;

  // init random + unit-normalize columns
  curandStatePhilox4_32_10_t st;
  for(int idx=tid; idx<d*n; idx+=nt){
    curand_init(seed,(unsigned long long)b*d*n+idx,0,&st);
    Phi[idx]=make_cuDoubleComplex(curand_normal_double(&st),curand_normal_double(&st));
  }
  __syncthreads();
  for(int c=tid;c<n;c+=nt){
    double s=0; for(int r=0;r<d;r++){cuDoubleComplex z=Phi[c*d+r]; s+=cuCreal(z)*cuCreal(z)+cuCimag(z)*cuCimag(z);}
    double inv=rsqrt(s); for(int r=0;r<d;r++){Phi[c*d+r]=make_cuDoubleComplex(cuCreal(Phi[c*d+r])*inv,cuCimag(Phi[c*d+r])*inv);}
  }
  __syncthreads();

  __shared__ double step; __shared__ double prevpot;
  if(tid==0){ step=0.1; prevpot=1e300; }
  __syncthreads();

  // gradient buffer in shared (reuse a second region): allocate after Phi
  cuDoubleComplex* grad = Phi + d*n;         // caller sizes shared = 2*d*n

  for(int it=0; it<max_iter; ++it){
    // zero grad
    for(int idx=tid; idx<d*n; idx+=nt) grad[idx]=make_cuDoubleComplex(0,0);
    __syncthreads();
    // grad column i = Σ_{j≠i} p |g_ij|^{p-2} conj(g_ij) v_j
    for(int i=tid; i<n; i+=nt){
      for(int j=0;j<n;j++){ if(j==i) continue;
        cuDoubleComplex g=make_cuDoubleComplex(0,0);
        for(int r=0;r<d;r++) g=cadd(g,cmul(cuConj(Phi[i*d+r]),Phi[j*d+r]));
        double a=cuCabs(g); if(a<1e-300) continue;
        double coef=p*pow(a,p-2.0);
        cuDoubleComplex cg=cuConj(g);
        for(int r=0;r<d;r++){ cuDoubleComplex t=cmul(cg,Phi[j*d+r]);
          grad[i*d+r]=make_cuDoubleComplex(cuCreal(grad[i*d+r])+coef*cuCreal(t),
                                           cuCimag(grad[i*d+r])+coef*cuCimag(t)); }
      }
    }
    __syncthreads();
    // trial step + renormalize + potential, done by thread 0's view via reduction
    // (simple: each thread updates its columns, then compute potential cooperatively)
    for(int idx=tid; idx<d*n; idx+=nt)
      Phi[idx]=make_cuDoubleComplex(cuCreal(Phi[idx])-step*cuCreal(grad[idx]),
                                    cuCimag(Phi[idx])-step*cuCimag(grad[idx]));
    __syncthreads();
    for(int c=tid;c<n;c+=nt){
      double s=0; for(int r=0;r<d;r++){cuDoubleComplex z=Phi[c*d+r]; s+=cuCreal(z)*cuCreal(z)+cuCimag(z)*cuCimag(z);}
      double inv=rsqrt(s); for(int r=0;r<d;r++) Phi[c*d+r]=make_cuDoubleComplex(cuCreal(Phi[c*d+r])*inv,cuCimag(Phi[c*d+r])*inv);
    }
    __syncthreads();
    // potential (Σ_{i<j}|g_ij|^p) via block reduction
    __shared__ double pot[256]; double loc=0;
    for(int i=tid;i<n;i+=nt) for(int j=i+1;j<n;j++){
      cuDoubleComplex g=make_cuDoubleComplex(0,0);
      for(int r=0;r<d;r++) g=cadd(g,cmul(cuConj(Phi[i*d+r]),Phi[j*d+r]));
      loc+=pow(cuCabs(g),p);
    }
    pot[tid]=loc; __syncthreads();
    for(int s=nt/2;s>0;s>>=1){ if(tid<s) pot[tid]+=pot[tid+s]; __syncthreads(); }
    if(tid==0){ double cur=2.0*pot[0];
      if(cur<prevpot){ if(prevpot-cur<tol){ prevpot=cur; } prevpot=cur; step*=1.05; }
      else step*=0.5; }
    __syncthreads();
    if(step<1e-14) break;
  }

  // final coherence (max_{i<j}|g_ij|) and write frame out
  __shared__ double mx[256]; double loc=0;
  for(int i=tid;i<n;i+=nt) for(int j=i+1;j<n;j++){
    cuDoubleComplex g=make_cuDoubleComplex(0,0);
    for(int r=0;r<d;r++) g=cadd(g,cmul(cuConj(Phi[i*d+r]),Phi[j*d+r]));
    double a=cuCabs(g); if(a>loc) loc=a;
  }
  mx[tid]=loc; __syncthreads();
  for(int s=nt/2;s>0;s>>=1){ if(tid<s) mx[tid]=fmax(mx[tid],mx[tid+s]); __syncthreads(); }
  if(tid==0) coh_out[b]=mx[0];
  for(int idx=tid; idx<d*n; idx+=nt) Phi_out[(size_t)b*d*n+idx]=Phi[idx];
}

} // anon

namespace etf {

MinimizeResult minimize_generate(int d,int n,int batch,double p,int max_iter,
                                 double tol,unsigned long long seed){
  int B=batch;
  cuDoubleComplex* dPhi; double* dCoh;
  CUDA_CHECK(cudaMalloc(&dPhi,(size_t)B*d*n*sizeof(cuDoubleComplex)));
  CUDA_CHECK(cudaMalloc(&dCoh,(size_t)B*sizeof(double)));
  double w=welch(d,n);
  size_t shmem=(size_t)2*d*n*sizeof(cuDoubleComplex);   // Phi + grad
  minimize_kernel<<<B,256,shmem>>>(dPhi,d,n,B,p,max_iter,tol,w,seed,dCoh);
  CUDA_CHECK(cudaGetLastError());

  std::vector<double> coh(B);
  CUDA_CHECK(cudaMemcpy(coh.data(),dCoh,B*sizeof(double),cudaMemcpyDeviceToHost));
  int best=0; for(int b=1;b<B;b++) if(coh[b]<coh[best]) best=b;

  std::vector<cuDoubleComplex> hPhi((size_t)d*n);
  CUDA_CHECK(cudaMemcpy(hPhi.data(),dPhi+(size_t)best*d*n,(size_t)d*n*sizeof(cuDoubleComplex),cudaMemcpyDeviceToHost));
  Mat Phi(d,n);
  for(int c=0;c<n;c++) for(int r=0;r<d;r++)
    Phi(r,c)=cd(cuCreal(hPhi[c*d+r]),cuCimag(hPhi[c*d+r]));   // column-major shared -> Mat

  cudaFree(dPhi); cudaFree(dCoh);
  return { Phi, coh[best], (coh[best]-w)<1e-9 };
}

} // namespace etf
