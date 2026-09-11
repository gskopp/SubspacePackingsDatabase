// 11_hash.hpp — order-invariant triple-product hashing.
// Direct port of EncodeComplex / HashfunctionH from frameEquiv.wl (Gene Kopp).
//
//   EncodeComplex[z, tol]  =  Round[Re z / tol]*37 + Round[Im z / tol]
//   HashfunctionH[Tri,tol] =  Mod[ Total[ EncodeComplex /@ Flatten[Tri] ], prime ]
//
// with tol = 10^-20 and prime = 104729 in the reference. The hash is a
// permutation-order-invariant fingerprint of the multiset of triple products:
// summing the per-entry encodings is symmetric under any reindexing, so two
// frames related by a column permutation produce the same hash.
//
// This is the same fingerprint used inside 09_equiv.cpp's compare_etfs; it is
// factored out here so the generator can compute a frame's invariant hash and
// its distinct-triple-product count through one shared, reference-faithful path.
#pragma once
#include "01_toolbox.hpp"
#include "06_convert.hpp"

namespace etf {

// Reference constants from frameEquiv.wl.
inline constexpr double      HASH_TOL   = 1e-20;   // EncodeComplex tolerance
inline constexpr long long   HASH_PRIME = 104729;  // HashfunctionH modulus

// EncodeComplex[z]: integer encoding of a single complex value at fixed tol.
// Uses long double for the Re*scale rounding so the 10^20 scale doesn't lose
// bits before llround (double mantissa is ~15-16 digits; the scaled magnitudes
// here are small because triple products of a unit frame lie in the unit disk).
long long encode_complex(cd z, double tol = HASH_TOL);

// HashfunctionH over an explicit tensor: Mod[Total[encode /@ entries], prime].
long long hash_tp_tensor(const Tensor3& TP, double tol = HASH_TOL);

// Distinct triple products (including degenerate), matching numberTP semantics:
// dedup every entry of the full n^3 tensor at the given tolerance. Returned as
// the count only (the list itself is available via etf::distinct_tp).
int number_tp_hashed(const Tensor3& TP, double tol = 1e-10);

} // namespace etf
