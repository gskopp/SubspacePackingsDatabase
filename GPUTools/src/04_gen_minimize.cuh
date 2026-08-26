// 04_gen_minimize.cuh — GPU potential-minimization generator (batched restarts).
// Replaces the CPU version. The method's whole strategy is "many random starts,
// keep the best" — that batches naturally onto the GPU: hundreds of independent
// frames descend the frame potential in lockstep, best-coherence seed returned.
// Approximate (double); Mathematica exactifies/validates afterward for non-conf-double frames.
#pragma once
#include "01_toolbox.hpp"

namespace etf {

struct MinimizeResult {
  Mat    frame;        // best synthesis operator (d x n, unit columns)
  double coherence;    // its coherence
  bool   is_etf;       // coherence - welch < tol
};

// batch independent random starts on the GPU; descend the p-frame potential;
// return the lowest-coherence frame. p defaults to 4 (the standard choice).
MinimizeResult minimize_generate(int d, int n, int batch = 256, double p = 4.0,
                                 int max_iter = 2000, double tol = 1e-12,
                                 unsigned long long seed = 2718ULL);

} // namespace etf
