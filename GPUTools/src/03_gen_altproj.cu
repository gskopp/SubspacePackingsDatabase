// 03_gen_altproj.cu — implementation. Kernels identical in spirit to the standalone
// altproj.cu; wrapped to return a host Gram matrix for the pipeline.
#include "03_gen_altproj.cuh"
#include <cstdio>
#include <cstdlib>
#include <vector>
#include <cuComplex.h>
#include <cuda_runtime.h>
#include <cusolverDn.h>
#include <curand_kernel.h>

#define CUDA_CHECK(x)   do{ cudaError_t e=(x); if(e!=cudaSuccess){ \
  fprintf(stderr,"CUDA %s:%d %s\n",__FILE__,__LINE__,cudaGetErrorString(e)); exit(1);} }while(0)
#define CUSOLVER_CHECK(x) do{ cusolverStatus_t s=(x); if(s!=CUSOLVER_STATUS_SUCCESS){ \
  fprintf(stderr,"cuSOLVER %s:%d status %d\n",__FILE__,__LINE__,(int)s); exit(1);} }while(0)

namespace {

__device__ __forceinline__ double zabs(cuDoubleComplex z){ return cuCabs(z); }

__global__ void init_gram(cuDoubleComplex* G,int n,int d,int B,unsigned long long seed){
  int b=blockIdx.x; if(b>=B) return;
  extern __shared__ cuDoubleComplex X[];
  curandStatePhilox4_32_10_t st;
  for(int idx=threadIdx.x; idx<n*d; idx+=blockDim.x){
    curand_init(seed,(unsigned long long)b*n*d+idx,0,&st);
    X[idx]=make_cuDoubleComplex(curand_normal_double(&st),curand_normal_double(&st));
  }
  __syncthreads();
  for(int i=threadIdx.x;i<n;i+=blockDim.x){
    double s=0; for(int j=0;j<d;j++){cuDoubleComplex z=X[i*d+j]; s+=cuCreal(z)*cuCreal(z)+cuCimag(z)*cuCimag(z);}
    double inv=1.0/sqrt(s);
    for(int j=0;j<d;j++) X[i*d+j]=make_cuDoubleComplex(cuCreal(X[i*d+j])*inv,cuCimag(X[i*d+j])*inv);
  }
  __syncthreads();
  cuDoubleComplex* Gb=G+(size_t)b*n*n;
  for(int col=threadIdx.x;col<n;col+=blockDim.x)
    for(int row=0;row<n;row++){
      cuDoubleComplex acc=make_cuDoubleComplex(0,0);
      for(int k=0;k<d;k++) acc=cuCadd(acc,cuCmul(X[row*d+k],cuConj(X[col*d+k])));
      Gb[col*n+row]=acc; // column-major
    }
}

__global__ void clip_offdiag(cuDoubleComplex* G,int n,int B,double mu,const int* done){
  int b=blockIdx.x; if(b>=B||done[b]) return;
  cuDoubleComplex* Gb=G+(size_t)b*n*n;
  for(int idx=threadIdx.x;idx<n*n;idx+=blockDim.x){
    int col=idx/n,row=idx%n;
    if(row==col) Gb[idx]=make_cuDoubleComplex(1.0,0.0);
    else { cuDoubleComplex z=Gb[idx]; double a=zabs(z); double f=mu/fmax(a,mu);
           Gb[idx]=make_cuDoubleComplex(cuCreal(z)*f,cuCimag(z)*f); }
  }
}

__global__ void trace_clip(const double* W,double* Wc,int n,int d,int B,const int* done){
  int b=blockIdx.x; if(b>=B||done[b]) return;
  if(threadIdx.x!=0) return;
  const double* Wb=W+(size_t)b*n; double* Wo=Wc+(size_t)b*n;
  for(int i=0;i<n;i++) Wo[i]=0.0;
  double first[64]; int cntPos=0;
  for(int i=0;i<n;i++) if(Wb[i]>0.0) cntPos++;
  for(int i=0;i<d;i++) first[i]=Wb[n-d+i];         // ascending: top-d are last d
  double s=0; for(int i=0;i<d;i++) s+=first[i];
  if(s>(double)n){
    while(s>(double)n){
      double shift=(s-(double)n)/(double)cntPos;
      for(int i=0;i<d;i++){double v=first[i]-shift; first[i]=v>0?v:0;}
      s=0; for(int i=0;i<d;i++) s+=first[i];
    }
  } else { double add=((double)n-s)/(double)d; for(int i=0;i<d;i++) first[i]+=add; }
  for(int i=0;i<d;i++) Wo[n-d+i]=first[i];
}

__global__ void reconstruct(cuDoubleComplex* G,const cuDoubleComplex* V,
                            const double* Wc,int n,int B,const int* done){
  int b=blockIdx.x; if(b>=B||done[b]) return;
  cuDoubleComplex* Gb=G+(size_t)b*n*n; const cuDoubleComplex* Vb=V+(size_t)b*n*n;
  const double* Wb=Wc+(size_t)b*n;
  for(int idx=threadIdx.x;idx<n*n;idx+=blockDim.x){
    int col=idx/n,row=idx%n; cuDoubleComplex acc=make_cuDoubleComplex(0,0);
    for(int k=0;k<n;k++){ double w=Wb[k]; if(w==0.0) continue;
      cuDoubleComplex t=cuCmul(Vb[k*n+row],cuConj(Vb[k*n+col]));
      acc=cuCadd(acc,make_cuDoubleComplex(w*cuCreal(t),w*cuCimag(t))); }
    Gb[idx]=acc;
  }
}

__global__ void coherence_err(const cuDoubleComplex* G,int n,int B,double mu,
                              double bound,int* done,double* errOut){
  int b=blockIdx.x; if(b>=B) return;
  const cuDoubleComplex* Gb=G+(size_t)b*n*n;
  __shared__ double smax[256]; double local=0;
  for(int idx=threadIdx.x;idx<n*n;idx+=blockDim.x){int col=idx/n,row=idx%n;
    if(row<col){double a=zabs(Gb[idx]); if(a>local) local=a;}}
  smax[threadIdx.x]=local; __syncthreads();
  for(int s=blockDim.x/2;s>0;s>>=1){ if(threadIdx.x<s) smax[threadIdx.x]=fmax(smax[threadIdx.x],smax[threadIdx.x+s]); __syncthreads(); }
  if(threadIdx.x==0){ double err=fabs(smax[0]-mu)/mu; errOut[b]=err; if(err<bound) done[b]=1; }
}

} // anon namespace

namespace etf {

AltProjResult altproj_generate(int d,int n,int batch,int maxit,double bound,
                               unsigned long long seed){
  int B=batch;
  double mu=welch(d,n);
  size_t Gsz=(size_t)B*n*n;
  cuDoubleComplex *dG,*dV; double *dW,*dWc,*dErr; int* dDone;
  CUDA_CHECK(cudaMalloc(&dG,Gsz*sizeof(cuDoubleComplex)));
  CUDA_CHECK(cudaMalloc(&dV,Gsz*sizeof(cuDoubleComplex)));
  CUDA_CHECK(cudaMalloc(&dW,(size_t)B*n*sizeof(double)));
  CUDA_CHECK(cudaMalloc(&dWc,(size_t)B*n*sizeof(double)));
  CUDA_CHECK(cudaMalloc(&dErr,(size_t)B*sizeof(double)));
  CUDA_CHECK(cudaMalloc(&dDone,(size_t)B*sizeof(int)));
  CUDA_CHECK(cudaMemset(dDone,0,B*sizeof(int)));

  init_gram<<<B,128,(size_t)n*d*sizeof(cuDoubleComplex)>>>(dG,n,d,B,seed);
  CUDA_CHECK(cudaGetLastError());

  cusolverDnHandle_t sh; CUSOLVER_CHECK(cusolverDnCreate(&sh));
  syevjInfo_t params; CUSOLVER_CHECK(cusolverDnCreateSyevjInfo(&params));
  CUSOLVER_CHECK(cusolverDnXsyevjSetTolerance(params,1e-15));
  CUSOLVER_CHECK(cusolverDnXsyevjSetMaxSweeps(params,30));
  int lwork=0;
  CUSOLVER_CHECK(cusolverDnZheevjBatched_bufferSize(sh,CUSOLVER_EIG_MODE_VECTOR,
    CUBLAS_FILL_MODE_LOWER,n,dV,n,dW,&lwork,params,B));
  cuDoubleComplex* work; CUDA_CHECK(cudaMalloc(&work,(size_t)lwork*sizeof(cuDoubleComplex)));
  int* devInfo; CUDA_CHECK(cudaMalloc(&devInfo,B*sizeof(int)));

  std::vector<int> hDone(B); int ran=0;
  for(int t=1;t<=maxit;t++){
    ran=t;
    clip_offdiag<<<B,256>>>(dG,n,B,mu,dDone);
    CUDA_CHECK(cudaMemcpy(dV,dG,Gsz*sizeof(cuDoubleComplex),cudaMemcpyDeviceToDevice));
    CUSOLVER_CHECK(cusolverDnZheevjBatched(sh,CUSOLVER_EIG_MODE_VECTOR,
      CUBLAS_FILL_MODE_LOWER,n,dV,n,dW,work,lwork,devInfo,params,B));
    trace_clip<<<B,32>>>(dW,dWc,n,d,B,dDone);
    reconstruct<<<B,256>>>(dG,dV,dWc,n,B,dDone);
    if(t%100==0){
      coherence_err<<<B,256>>>(dG,n,B,mu,bound,dDone,dErr);
      CUDA_CHECK(cudaMemcpy(hDone.data(),dDone,B*sizeof(int),cudaMemcpyDeviceToHost));
      int nd=0; for(int b=0;b<B;b++) nd+=hDone[b];
      if(nd==B) break;
    }
  }
  coherence_err<<<B,256>>>(dG,n,B,mu,bound,dDone,dErr);
  std::vector<double> hErr(B);
  CUDA_CHECK(cudaMemcpy(hErr.data(),dErr,B*sizeof(double),cudaMemcpyDeviceToHost));
  CUDA_CHECK(cudaMemcpy(hDone.data(),dDone,B*sizeof(int),cudaMemcpyDeviceToHost));
  int best=0; for(int b=1;b<B;b++) if(hErr[b]<hErr[best]) best=b;
  int nconv=0; for(int b=0;b<B;b++) nconv+=hDone[b];

  std::vector<cuDoubleComplex> hG((size_t)n*n);
  CUDA_CHECK(cudaMemcpy(hG.data(),dG+(size_t)best*n*n,(size_t)n*n*sizeof(cuDoubleComplex),
             cudaMemcpyDeviceToHost));
  Mat G(n,n);
  for(int col=0;col<n;col++) for(int row=0;row<n;row++)     // col-major -> row-major
    G(row,col)=cd(cuCreal(hG[(size_t)col*n+row]),cuCimag(hG[(size_t)col*n+row]));

  cusolverDnDestroySyevjInfo(params); cusolverDnDestroy(sh);
  cudaFree(dG);cudaFree(dV);cudaFree(dW);cudaFree(dWc);cudaFree(dErr);
  cudaFree(dDone);cudaFree(work);cudaFree(devInfo);

  return {G, hErr[best], ran, nconv};
}

} // namespace etf
