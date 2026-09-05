// 06_convert_cpu.cpp — CPU conversions + host triple-product tensor build.
// Used for altproj/minimize (one-off frames materialized to disk).
#include "06_convert.hpp"
#include <cmath>
#include <stdexcept>

extern "C" {
  void zgesdd_(const char*,const int*,const int*,void*,const int*,double*,
               void*,const int*,void*,const int*,void*,const int*,double*,int*,int*);
}

namespace etf {

Mat normalize_so(const Mat& Phi){
  Mat R=Phi;
  for(int j=0;j<R.cols;++j){ double s=0; for(int i=0;i<R.rows;++i) s+=std::norm(R(i,j));
    double inv=1.0/std::sqrt(s); for(int i=0;i<R.rows;++i) R(i,j)*=inv; }
  return R;
}

Mat gm_from_so(const Mat& Phi){
  int n=Phi.cols; Mat G(n,n);
  for(int i=0;i<n;++i) for(int j=0;j<n;++j){ cd a(0,0);
    for(int r=0;r<Phi.rows;++r) a+=std::conj(Phi(r,i))*Phi(r,j); G(i,j)=a; }
  return G;
}

Mat so_from_gm(const Mat& G,int r){
  int n=G.rows;
  std::vector<cd> A((size_t)n*n);
  for(int i=0;i<n;++i) for(int j=0;j<n;++j) A[(size_t)j*n+i]=G(i,j);
  std::vector<double> S(n); std::vector<cd> U((size_t)n*n),VT((size_t)n*n);
  int lwork=-1,info=0; cd wkopt; std::vector<double> rwork((size_t)5*n*n+7*n);
  std::vector<int> iwork(8*n); const char jobz='A';
  zgesdd_(&jobz,&n,&n,A.data(),&n,S.data(),U.data(),&n,VT.data(),&n,&wkopt,&lwork,rwork.data(),iwork.data(),&info);
  lwork=(int)wkopt.real(); std::vector<cd> work(lwork);
  zgesdd_(&jobz,&n,&n,A.data(),&n,S.data(),U.data(),&n,VT.data(),&n,work.data(),&lwork,rwork.data(),iwork.data(),&info);
  if(info!=0) throw std::runtime_error("zgesdd failed");
  Mat Phi(r,n);
  for(int a=0;a<r;++a){ double sr=std::sqrt(S[a]);
    for(int c=0;c<n;++c) Phi(a,c)=sr*std::conj(U[(size_t)a*n+c]); }
  return Phi;
}

std::vector<double> gos_from_so(const Mat& Phi){
  std::vector<double> g; g.reserve((size_t)2*Phi.rows*Phi.cols);
  for(int j=0;j<Phi.cols;++j) for(int i=0;i<Phi.rows;++i) g.push_back(Phi(i,j).real());
  for(int j=0;j<Phi.cols;++j) for(int i=0;i<Phi.rows;++i) g.push_back(Phi(i,j).imag());
  return g;
}

Mat so_from_gos(const std::vector<double>& g,int d,int n){
  Mat Phi(d,n); size_t half=(size_t)d*n,p=0;
  for(int j=0;j<n;++j) for(int i=0;i<d;++i){ Phi(i,j)=cd(g[p],g[half+p]); ++p; }
  return Phi;
}

static int find_or_add(Vec& u,cd v,double tol){
  for(size_t k=0;k<u.size();++k) if(std::abs(u[k]-v)<tol) return (int)k;
  u.push_back(v); return (int)u.size()-1;
}
LUT array_to_lut(const Tensor3& T,double tol){
  LUT l; l.rank=3; l.n=T.n; l.idx.resize(T.a.size());
  for(size_t i=0;i<T.a.size();++i) l.idx[i]=find_or_add(l.distinct,T.a[i],tol);
  return l;
}
LUT array_to_lut(const Mat& M,double tol){
  LUT l; l.rank=2; l.n=M.rows; l.idx.resize(M.a.size());
  for(size_t i=0;i<M.a.size();++i) l.idx[i]=find_or_add(l.distinct,M.a[i],tol);
  return l;
}
Mat tp_slice_from_tp(const Tensor3& T,int i){
  int n=T.n; Mat S(n,n);
  for(int j=0;j<n;++j) for(int k=0;k<n;++k) S(j,k)=T(i,j,k);
  return S;
}

// The n^3 tensor build on the host: T[i,j,k] = G_ij G_jk G_ki.
Tensor3 tp_from_gm_cpu(const Mat& G){
  int n=G.rows; Tensor3 T(n);
  for(int i=0;i<n;++i) for(int j=0;j<n;++j) for(int k=0;k<n;++k)
    T(i,j,k)=G(i,j)*G(j,k)*G(k,i);
  return T;
}

// Dispatcher: the single host->device seam for the n^3 tensor build. main.cpp
// (and any caller) routes through tp_from_gm(G, use_gpu); use_gpu=true requires
// 06_convert_gpu.cu in the link (the CMake target includes it). CPU-only helpers
// that must never touch the GPU call tp_from_gm_cpu directly (07_invariants,
// 10_validate).
Tensor3 tp_from_gm(const Mat& G, bool use_gpu){
  return use_gpu ? tp_from_gm_gpu(G) : tp_from_gm_cpu(G);
}

} // namespace etf
