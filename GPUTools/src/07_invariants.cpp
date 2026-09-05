// 07_invariants.cpp
#include "07_invariants.hpp"
#include <cmath>

namespace etf {

Vec distinct_tp(const Tensor3& TP, double tol) {
  Vec out;
  for (cd v : TP.a) {
    bool found=false;
    for (cd u : out) if (std::abs(u-v)<tol){found=true;break;}
    if(!found) out.push_back(v);
  }
  return out;
}
Vec distinct_tp(const Mat& Phi, double tol) { return distinct_tp(tp_from_gm_cpu(gm_from_so(Phi)), tol); }
int number_tp(const Tensor3& TP, double tol) { return (int)distinct_tp(TP,tol).size(); }

cd moment(const Tensor3& TP, int m) {
  cd s(0,0);
  for (cd v : TP.a) s += std::pow(v, m);
  return s;
}

cd moment_nd(const Tensor3& TP, int m) {
  int n = TP.n; cd s(0,0);
  for (int i=0;i<n;i++) for (int j=0;j<n;j++) for (int k=0;k<n;k++) {
    if (i==j||j==k||k==i) continue;         // drop degenerate index tuples
    s += std::pow(TP(i,j,k), m);
  }
  return s;
}

std::vector<cd> moments(const Tensor3& TP, int M) {
  std::vector<cd> out; out.reserve(M);
  for (int m=1;m<=M;m++) out.push_back(moment(TP,m));
  return out;
}

} // namespace etf
