// 01_toolbox.cpp
#include "01_toolbox.hpp"
#include <algorithm>
#include <regex>
#include <cmath>

namespace etf {

static Vec normalize_cols(const Mat& Phi) {
  // return column norms
  Vec norms(Phi.cols, cd(0,0));
  std::vector<double> s(Phi.cols, 0.0);
  for (int j = 0; j < Phi.cols; ++j) {
    double acc = 0;
    for (int i = 0; i < Phi.rows; ++i) acc += std::norm(Phi(i,j));
    s[j] = std::sqrt(acc);
  }
  Vec out(s.size());
  for (size_t j = 0; j < s.size(); ++j) out[j] = s[j];
  return out;
}

double coherence(const Mat& Phi) {
  std::vector<double> nrm(Phi.cols);
  for (int j = 0; j < Phi.cols; ++j) {
    double acc = 0; for (int i = 0; i < Phi.rows; ++i) acc += std::norm(Phi(i,j));
    nrm[j] = std::sqrt(acc);
  }
  double mx = 0.0;
  for (int j = 0; j < Phi.cols; ++j)
    for (int k = j + 1; k < Phi.cols; ++k) {
      cd ip(0,0);
      for (int i = 0; i < Phi.rows; ++i) ip += std::conj(Phi(i,j)) * Phi(i,k);
      double v = std::abs(ip) / (nrm[j]*nrm[k]);
      if (v > mx) mx = v;
    }
  return mx;
}

double p_frame_potential(const Mat& Phi, double p) {
  std::vector<double> nrm(Phi.cols);
  for (int j = 0; j < Phi.cols; ++j) {
    double acc = 0; for (int i = 0; i < Phi.rows; ++i) acc += std::norm(Phi(i,j));
    nrm[j] = std::sqrt(acc);
  }
  double tot = 0.0;
  for (int j = 0; j < Phi.cols; ++j)
    for (int k = j + 1; k < Phi.cols; ++k) {
      cd ip(0,0);
      for (int i = 0; i < Phi.rows; ++i) ip += std::conj(Phi(i,j)) * Phi(i,k);
      tot += std::pow(std::abs(ip) / (nrm[j]*nrm[k]), p);
    }
  return 2.0 * tot;
}

Dims extract_dims(const std::string& f) {
  std::smatch m;
  std::regex re("(\\d+)x(\\d+)");
  if (std::regex_search(f, m, re)) return {std::stoi(m[1]), std::stoi(m[2])};
  return {0,0};
}

int extract_trip(const std::string& f) {
  std::smatch m;
  std::regex re("(\\d+)x(\\d+)_(\\d+)");
  if (std::regex_search(f, m, re)) return std::stoi(m[3]);
  return -1;
}

std::string replace_ext(const std::string& f, const std::string& ext) {
  auto slash = f.find_last_of("/\\");
  auto dot   = f.find_last_of('.');
  if (dot == std::string::npos || (slash != std::string::npos && dot < slash))
    return f + "." + ext;
  return f.substr(0, dot) + "." + ext;
}

} // namespace etf
