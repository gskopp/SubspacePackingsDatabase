// 07_invariants.hpp — frame invariants. Port of frameInvariants.wl.
#pragma once
#include "06_convert.hpp"

namespace etf {

// Distinct triple products (including degenerate), up to tolerance.
Vec distinct_tp(const Tensor3& TP, double tol = 1e-10);
Vec distinct_tp(const Mat& Phi, double tol = 1e-10);   // from synthesis operator
int number_tp(const Tensor3& TP, double tol = 1e-10);

// m-th moment = sum over all i,j,k of (triple product)^m.
cd moment(const Tensor3& TP, int m);
// nondiagonal moment: degenerate (repeated-index) entries zeroed.
cd moment_nd(const Tensor3& TP, int m);

// convenience: moments 1..M
std::vector<cd> moments(const Tensor3& TP, int M);

} // namespace etf
