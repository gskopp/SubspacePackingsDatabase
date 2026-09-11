// 08_equiv.cpp — coset-decomposition equivalence test.
// Frames are d x n synthesis operators; permutations act on the n columns
// (frame vectors). Two frames are equivalent iff some column permutation makes
// their triple-product tensors match up to the ETF's phase freedom, which the
// order-invariant hash of triple-product magnitudes/values captures.
#include "09_equiv.hpp"
#include <vector>
#include <algorithm>
#include <numeric>
#include <cmath>
#include <unordered_set>
#ifdef _OPENMP
#include <omp.h>
#endif

namespace etf {

// ---- triple products of a column-permuted frame ----
// Column j of Phi is frame vector v_j. Hermitian inner product <v_a,v_b>.
static cd ip(const Mat& Phi, int a, int b) {
  cd s(0,0); for (int r=0;r<Phi.rows;++r) s += std::conj(Phi(r,a))*Phi(r,b);
  return s;
}

// Order-invariant hash of the full triple-product multiset under a permutation.
// Encode each complex value at fixed tolerance, sum modulo a large prime.
static long long encode(cd z) {
  const double scale = 1e12;
  long long re = llround(z.real()*scale);
  long long im = llround(z.imag()*scale);
  return re*37 + im;
}
static long long hash_tp(const Mat& Phi, const std::vector<int>& perm) {
  const long long prime = 1000000007LL;
  int n = Phi.cols;
  long long acc = 0;
  for (int i=0;i<n;i++) for (int j=0;j<n;j++) for (int k=0;k<n;k++) {
    int a=perm[i], b=perm[j], c=perm[k];
    cd t = ip(Phi,a,b)*ip(Phi,b,c)*ip(Phi,c,a);
    acc = ( acc + (encode(t)%prime + prime)%prime ) % prime;
  }
  return acc;
}

std::pair<int,int> find_optimal_kl(int n) {
  if (n<=1) return {0,0};
  double sqrtNFact = std::sqrt((double)std::tgamma((double)n+1.0)); // sqrt(n!)
  int kMax = std::max(1, n-2);
  // shrink kMax while k! too large (stability, mirrors WL)
  auto fact=[&](int k){ double f=1; for(int i=2;i<=k;i++) f*=i; return f; };
  while (kMax>1 && fact(kMax)>2*sqrtNFact) kMax--;
  int bestK=0,bestL=0; double minDiff=1e300;
  for (int k=0;k<=kMax;k++) for (int l=0;l<=n-k;l++) {
    if (k==0&&l==0) continue;
    double hSize = (l==0||l==1)? fact(k) : l*fact(k);
    if (hSize==0) continue;
    double diff=std::fabs(hSize-sqrtNFact);
    if (diff<minDiff){minDiff=diff;bestK=k;bestL=l;}
  }
  return {bestK,bestL};
}

// Build subgroup H (as explicit permutations) per GenerateSubgroupH's generators,
// then close under composition. For the modest |H| this targets, brute closure ok.
static std::vector<std::vector<int>> generate_H(int n, int k, int l) {
  std::vector<std::vector<int>> gens;
  auto ident=[&]{ std::vector<int> p(n); std::iota(p.begin(),p.end(),0); return p; };
  auto cycle=[&](int lo,int hi){ auto p=ident(); // cycle lo..hi (0-based inclusive)
    for(int i=lo;i<=hi;i++) p[i]= (i==hi)? lo : i+1; return p; };
  if (k==0) { if(l>=2) gens.push_back(cycle(0,l-1)); }
  else if (k==1) { if(l>=2) gens.push_back(cycle(1,l)); }
  else { auto t=ident(); std::swap(t[0],t[1]); gens.push_back(t);
         gens.push_back(cycle(0,k-1));
         if(l>=2) gens.push_back(cycle(k,k+l-1)); }
  // closure
  auto compose=[&](const std::vector<int>&a,const std::vector<int>&b){
    std::vector<int> c(n); for(int i=0;i<n;i++) c[i]=a[b[i]]; return c; };
  auto key=[&](const std::vector<int>&p){ std::string s; s.reserve(n);
    for(int x:p) s.push_back((char)('0'+x)); return s; };
  std::vector<std::vector<int>> H{ident()};
  std::unordered_set<std::string> seen{ key(ident()) };
  bool grew=true;
  while(grew){ grew=false; size_t sz=H.size();
    for(size_t i=0;i<sz;i++) for(auto&g:gens){ auto c=compose(H[i],g);
      auto kk=key(c); if(!seen.count(kk)){seen.insert(kk);H.push_back(c);grew=true;} } }
  return H;
}

// Transversal T of S_n / H: coset representatives. We enumerate S_n and pick one
// rep per coset by canonicalizing g*H. For modest n this is acceptable; it mirrors
// GenerateTransversalH's role without reproducing its interleave construction.
static std::vector<std::vector<int>> generate_T(int n,
        const std::vector<std::vector<int>>& H) {
  std::vector<int> base(n); std::iota(base.begin(),base.end(),0);
  auto compose=[&](const std::vector<int>&a,const std::vector<int>&b){
    std::vector<int> c(n); for(int i=0;i<n;i++) c[i]=a[b[i]]; return c; };
  auto key=[&](const std::vector<int>&p){ std::string s; for(int x:p) s.push_back((char)('0'+x)); return s; };
  std::vector<std::vector<int>> T; std::unordered_set<std::string> covered;
  std::vector<int> g=base;
  do {
    // canonical coset key = min over h in H of key(g*h)
    std::string ck; bool first=true;
    for(auto&h:H){ auto gh=compose(g,h); auto kk=key(gh); if(first||kk<ck){ck=kk;first=false;} }
    if(!covered.count(ck)){ covered.insert(ck); T.push_back(g); }
  } while(std::next_permutation(g.begin(),g.end()));
  return T;
}

bool compare_etfs(const Mat& A, const Mat& B, double tol, bool verbose) {
  int n=A.cols;
  if (B.cols!=n || A.rows!=B.rows) return false;
  auto kl=find_optimal_kl(n);
  auto H=generate_H(n,kl.first,kl.second);
  auto T=generate_T(n,H);

  // hashes of A under H
  std::vector<long long> hashA(H.size());
  #pragma omp parallel for schedule(dynamic)
  for (int i=0;i<(int)H.size();++i) hashA[i]=hash_tp(A,H[i]);
  std::unordered_set<long long> setA(hashA.begin(),hashA.end());

  // hashes of B under T; any collision => equivalent
  std::vector<char> hit(T.size(),0);
  #pragma omp parallel for schedule(dynamic)
  for (int i=0;i<(int)T.size();++i) {
    long long hb=hash_tp(B,T[i]);
    if (setA.count(hb)) hit[i]=1;
  }
  for (char c:hit) if(c) return true;
  return false;
}

} // namespace etf
