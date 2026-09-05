// 03_altproj.cuh — batched alternating-projection generator (library interface).
// Port of troppAltProj.wl, batched across seeds. GPU stage only: returns the
// coarse (machine-precision) Gram matrix of the best-converged seed as a host
// etf::Mat. Frame extraction (so_from_gm) and exactification happen downstream.
#pragma once
#include "01_toolbox.hpp"

namespace etf {

struct AltProjResult {
  Mat  gram;            // n x n best-seed Gram matrix (host)
  double coh_error;     // |max offdiag - welch|/welch for the best seed
  int  iterations;      // iterations run
  int  converged_seeds; // how many of the batch reached the bound
};

// d, n: ETF parameters. batch: number of parallel random seeds. maxit: cap.
// bound: relative coherence tolerance for convergence (default 5e-15).
AltProjResult altproj_generate(int d, int n, int batch = 256,
                               int maxit = 30000, double bound = 5e-15,
                               unsigned long long seed = 1234ULL);

} // namespace etf
