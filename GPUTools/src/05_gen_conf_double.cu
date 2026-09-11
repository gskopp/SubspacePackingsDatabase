// 05_gen_conf_double.cu — doubled-conference-graph (d,2d) generator.
// GPU assembly of eq. (4) of arXiv:2410.17379 (Theorem 8), returns host Gram.
// Kernel math is unchanged from the previously-verified paley assembly; only
// names and comments are updated to reflect the true construction.
#include "05_gen_conf_double.cuh"
#include <cuComplex.h>
#include <cuda_runtime.h>
#include <stdexcept>
#include <cmath>

namespace {
__device__ int legendre_dev(long long a,int p){
  a%=p; if(a<0)a+=p; if(a==0)return 0;
  long long r=1,b=a,e=(p-1)/2;
  while(e){ if(e&1) r=(r*b)%p; b=(b*b)%p; e>>=1; }
  return (r==1)?1:-1;
}

// One thread per Gram entry. Block layout: 2x2 array of q x q blocks.
//   Aab = Paley-graph adjacency (a-b a nonzero quadratic residue)   -> A
//   Bab = complement adjacency (a != b and not adjacent)            -> B = J-I-A
// Blocks (eq. 4):  [ A-B          epsI + betaA + conj(beta)B ]
//                  [ epsI+conj(beta)A + betaB     B-A        ]
// with beta = eps*x + i*y, so betaA+conj(beta)B = eps*x(A+B) + i*y(A-B).
__global__ void assemble(cuDoubleComplex* G,int q,int eps){
  int v=q,n=2*v;
  int idx=blockIdx.x*blockDim.x+threadIdx.x; if(idx>=n*n) return;
  int i=idx/n,j=idx%n;
  double gamma=1.0/sqrt((double)(2*v-1));
  double x=(-1.0+sqrt((double)(2*v-1)))/(double)(v-1);
  double y=sqrt(1.0-x*x);
  int bi=i/v,bj=j/v,a=i%v,b=j%v;
  double Aab=(a!=b && legendre_dev(a-b,q)==1)?1.0:0.0;
  double Bab=(a!=b)?(1.0-Aab):0.0;
  cuDoubleComplex s;
  if(bi==0&&bj==0)      s=make_cuDoubleComplex(Aab-Bab,0.0);
  else if(bi==1&&bj==1) s=make_cuDoubleComplex(Bab-Aab,0.0);
  else if(bi==0&&bj==1){ double e=(a==b)?eps:0.0;
    s=make_cuDoubleComplex(e+eps*x*(Aab+Bab), y*(Aab-Bab)); }
  else { double e=(a==b)?eps:0.0;
    s=make_cuDoubleComplex(e+eps*x*(Aab+Bab), y*(Bab-Aab)); }
  double re=gamma*cuCreal(s)+((i==j)?1.0:0.0);   // G = I + gamma*S
  double im=gamma*cuCimag(s);
  G[idx]=make_cuDoubleComplex(re,im);
}
}

namespace etf {

bool conf_double_valid(int q){
  if(q<5||q%4!=1) return false;                 // need q = 1 mod 4, q >= 5
  for(int i=2;(long long)i*i<=q;i++) if(q%i==0) return false;  // prime
  return true;
}

Mat conf_double_gram(int q,int eps){
  if(!conf_double_valid(q))
    throw std::runtime_error("conf_double_gram: q must be prime = 1 mod 4");
  int n=2*q; size_t nn=(size_t)n*n;
  cuDoubleComplex* dG=nullptr;
  if(cudaMalloc(&dG,nn*sizeof(cuDoubleComplex))!=cudaSuccess)
    throw std::runtime_error("conf_double_gram: cudaMalloc");
  int tpb=256,blk=(int)((nn+tpb-1)/tpb);
  assemble<<<blk,tpb>>>(dG,q,eps);
  if(cudaGetLastError()!=cudaSuccess){ cudaFree(dG); throw std::runtime_error("conf_double assemble"); }
  Mat G(n,n);
  cudaMemcpy(G.a.data(),dG,nn*sizeof(cuDoubleComplex),cudaMemcpyDeviceToHost);
  cudaFree(dG);
  return G;
}

} // namespace etf
