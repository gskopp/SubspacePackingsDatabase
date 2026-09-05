// 06_convert_gpu.cu — GPU triple-product tensor build.
// Used for the conf_double method (bulk sweeps, large n=2q): the n^3 build is heavy
// and regular, so it runs as a device kernel. Result is copied back to host as
// an etf::Tensor3 (the tensor must land host-side for LUT/export regardless).
//
// Only tp_from_gm_gpu is defined here; the cheap conversions and the CPU tensor
// build stay in 06_convert_cpu.cpp. Link this TU only when the GPU path is wanted.
#include "06_convert.hpp"
#include <cuComplex.h>
#include <cuda_runtime.h>
#include <cstdio>
#include <stdexcept>

namespace {
__global__ void tp_kernel(const cuDoubleComplex* G, cuDoubleComplex* T, int n){
  long long total=(long long)n*n*n;
  for(long long idx=blockIdx.x*blockDim.x+threadIdx.x; idx<total;
      idx+=(long long)blockDim.x*gridDim.x){
    int k=idx%n, j=(idx/n)%n, i=idx/((long long)n*n);
    cuDoubleComplex gij=G[(long long)i*n+j];
    cuDoubleComplex gjk=G[(long long)j*n+k];
    cuDoubleComplex gki=G[(long long)k*n+i];
    T[idx]=cuCmul(cuCmul(gij,gjk),gki);
  }
}
}

namespace etf {

Tensor3 tp_from_gm_gpu(const Mat& G){
  int n=G.rows;
  size_t nn=(size_t)n*n, nnn=(size_t)n*n*n;
  // host G -> device (row-major, cuDoubleComplex is layout-compatible with cd)
  cuDoubleComplex *dG=nullptr,*dT=nullptr;
  if(cudaMalloc(&dG,nn*sizeof(cuDoubleComplex))!=cudaSuccess)
    throw std::runtime_error("cudaMalloc dG");
  if(cudaMalloc(&dT,nnn*sizeof(cuDoubleComplex))!=cudaSuccess){
    cudaFree(dG); throw std::runtime_error("cudaMalloc dT (n^3 too large?)"); }
  cudaMemcpy(dG,G.a.data(),nn*sizeof(cuDoubleComplex),cudaMemcpyHostToDevice);

  int tpb=256, grid=2048;
  tp_kernel<<<grid,tpb>>>(dG,dT,n);
  if(cudaGetLastError()!=cudaSuccess){ cudaFree(dG);cudaFree(dT);
    throw std::runtime_error("tp_kernel launch"); }

  Tensor3 T(n);
  cudaMemcpy(T.a.data(),dT,nnn*sizeof(cuDoubleComplex),cudaMemcpyDeviceToHost);
  cudaFree(dG); cudaFree(dT);
  return T;
}

} // namespace etf
