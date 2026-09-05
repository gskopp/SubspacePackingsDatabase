// 01_toolbox.hpp — foundation types + basic frame quantities.
// Port of toolbox.wl. Everything downstream links against this.
#pragma once
#include <complex>
#include <vector>
#include <string>
#include <cstddef>

namespace etf {

using cd  = std::complex<double>;
using Vec = std::vector<cd>;

// Row-major dense complex matrix. Frames are stored as SYNTHESIS operators:
// d rows (ambient dim) x n cols (frame vectors), matching convertFrameData.wl's
// SO convention (columns are unit frame vectors).
struct Mat {
  int rows = 0, cols = 0;
  Vec a;                                   // size rows*cols, row-major
  Mat() = default;
  Mat(int r, int c) : rows(r), cols(c), a((size_t)r*c) {}
  cd&       operator()(int i, int j)       { return a[(size_t)i*cols + j]; }
  const cd& operator()(int i, int j) const { return a[(size_t)i*cols + j]; }
};

// Welch bound: coherence lower bound, attained iff Phi is an ETF.
inline double welch(int d, int n) {
  return std::sqrt(double(n - d) / (double(d) * double(n - 1)));
}

// Coherence of a synthesis operator = max off-diagonal |<v_i,v_j>| over unit cols.
double coherence(const Mat& Phi);

// p-frame potential (unit-normalized columns).
double p_frame_potential(const Mat& Phi, double p);

// Parse "dxn" and the triple-product count from a filename like etf_4x8_21a.gos
struct Dims { int d, n; };
Dims  extract_dims(const std::string& filename);      // {d,n}
int   extract_trip(const std::string& filename);      // number of triple products
std::string replace_ext(const std::string& filename, const std::string& ext);

} // namespace etf
