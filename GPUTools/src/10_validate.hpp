// 09_validate.hpp — ETF validity checks. Port of validatePackings.wl.
#pragma once
#include "06_convert.hpp"
#include <string>
#include <vector>

namespace etf {

struct ValidationReport {
  std::string file;
  bool coherence_ok = false;   // |offdiag| == welch
  bool hermitian_ok = false;
  bool projection_ok = false;  // G^2 == (n/d) G
  bool trip_count_ok = false;  // #distinct TP matches filename
  bool passed() const { return coherence_ok && hermitian_ok && projection_ok && trip_count_ok; }
};

ValidationReport validate_frame(const Mat& Phi, int d, int n,
                                int expected_trip = -1, double tol = 1e-9);

// Validate a file by path (dispatches on extension; reconstructs Gram matrix).
ValidationReport validate_file(const std::string& path, double tol = 1e-9);

// Glob a pattern, validate all matching etf files, return reports.
std::vector<ValidationReport> validate_pattern(const std::string& pattern, double tol = 1e-9);

} // namespace etf
