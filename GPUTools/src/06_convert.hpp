// 06_convert.hpp — representation conversions, shared interface.
// The triple-product tensor build tp_from_gm() has TWO implementations that
// satisfy this same declaration:
//     06_convert_cpu.cpp  — host triple loop      (altproj, minimize: one-off frames)
//     06_convert_gpu.cu   — n^3 device kernel      (conf_double: bulk sweeps, large n)
// main.cpp links the one matching the chosen generator, or links both and calls
// tp_from_gm_cpu / tp_from_gm_gpu explicitly (both symbols declared here).
//
// Everything else in convert (SO<->GM, SVD extraction, GoS, LUT) is cheap,
// once-per-frame, and CPU-only — it lives in 06_convert_cpu.cpp and is shared.
#pragma once
#include "01_toolbox.hpp"

namespace etf {

// dense triple-product tensor, flat n^3 row-major
struct Tensor3 {
  int n = 0;
  Vec a;
  Tensor3() = default;
  explicit Tensor3(int n_) : n(n_), a((size_t)n_*n_*n_) {}
  cd&       operator()(int i,int j,int k)       { return a[((size_t)i*n+j)*n+k]; }
  const cd& operator()(int i,int j,int k) const { return a[((size_t)i*n+j)*n+k]; }
};

struct LUT {
  Vec distinct;
  std::vector<int> idx;
  int rank = 0, n = 0;
};

// --- cheap conversions, CPU-only, in 06_convert_cpu.cpp ---
Mat     normalize_so(const Mat& Phi);
Mat     gm_from_so(const Mat& Phi);
Mat     so_from_gm(const Mat& G, int r);           // LAPACK SVD to rank r
std::vector<double> gos_from_so(const Mat& Phi);
Mat     so_from_gos(const std::vector<double>& g, int d, int n);
LUT     array_to_lut(const Tensor3& T, double tol = 1e-10);
LUT     array_to_lut(const Mat& M, double tol = 1e-10);
Mat     tp_slice_from_tp(const Tensor3& T, int i = 0);

// --- the n^3 tensor build: two implementations, same result ---
Tensor3 tp_from_gm_cpu(const Mat& G);              // 06_convert_cpu.cpp
Tensor3 tp_from_gm_gpu(const Mat& G);              // 06_convert_gpu.cu

// Dispatcher: main.cpp sets use_gpu based on --method (true for conf_double).
Tensor3 tp_from_gm(const Mat& G, bool use_gpu);

} // namespace etf
