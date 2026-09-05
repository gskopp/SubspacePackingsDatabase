// 11_hash.cpp
#include "11_hash.hpp"
#include "07_invariants.hpp"
#include <cmath>

namespace etf {

long long encode_complex(cd z, double tol){
  // scale = 1/tol; Round[Re*scale]*37 + Round[Im*scale].
  // llroundl on long double keeps the 10^20 scaling honest.
  long double scale = (long double)1.0 / (long double)tol;
  long long re = (long long)llroundl((long double)z.real() * scale);
  long long im = (long long)llroundl((long double)z.imag() * scale);
  return re * 37 + im;
}

long long hash_tp_tensor(const Tensor3& TP, double tol){
  // Total[...] then Mod prime. Accumulate modulo prime as we go to avoid
  // overflow on large n^3 (each encoded term can be large).
  long long acc = 0;
  for (cd v : TP.a){
    long long e = encode_complex(v, tol) % HASH_PRIME;
    acc = ((acc + e) % HASH_PRIME + HASH_PRIME) % HASH_PRIME;
  }
  return acc;
}

int number_tp_hashed(const Tensor3& TP, double tol){
  // Reuse the existing distinct-value dedup (07_invariants) so trip counting
  // has exactly one definition across the codebase. numberTP = #distinct.
  return number_tp(TP, tol);
}

} // namespace etf
