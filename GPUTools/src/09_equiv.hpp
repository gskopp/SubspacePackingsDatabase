// 08_equiv.hpp — projective unitary permutation equivalence of ETFs.
// Port of Functionsfile.wl ComPareETFs: small-step/big-step (subgroup H +
// transversal T of S_n) decomposition, triple-product hashing, collision match.
// Parallelism: OpenMP over coset elements (independent tasks) — CPU, not GPU.
#pragma once
#include "01_toolbox.hpp"

namespace etf {

// True if the two frames are projective-unitary-permutation equivalent.
// tol: numerical tolerance for triple-product hashing.
bool compare_etfs(const Mat& A, const Mat& B, double tol = 1e-14, bool verbose = false);

// Balances |H| against sqrt(n!) as in FindOptimalKL. Returns {k,l}.
std::pair<int,int> find_optimal_kl(int n);

} // namespace etf
