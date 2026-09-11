// 02_io.hpp — the four label-doc format writers + one shared export_all.
// Every generator routes through export_all, so all methods produce
// byte-identical file structure.
//
// LABEL POLICY (this revision):
//   * trip is COMPUTED from the frame's triple-product tensor (numberTP), never
//     hardcoded. See 11_hash.hpp / 07_invariants.hpp.
//   * alpha defaults to "a". Per the database conventions doc, the lowercase
//     letter disambiguates equivalence classes among frames sharing (d,n,trip)
//     and is assigned by equivalence-checking against the database in insertion
//     order -- it is NOT an intrinsic property of a single frame. This pipeline
//     emits "a" and leaves database-order assignment / dedup to the caller
//     (compare_etfs in 09_equiv).
#pragma once
#include "01_toolbox.hpp"
#include "06_convert.hpp"
#include <string>
#include <vector>

namespace etf {

// individual writers (numeric doubles, per the label doc)
void export_gos(const Mat& Phi, const std::string& path, int digits = 17);
void export_tp (const Tensor3& TP, const std::string& path);
void export_exa(const Mat& TPslice, const std::string& path);
void export_inv(const std::string& path, int d, int n, int trip, int variant,
                const char* tags, double coherence, double welch_bound,
                const Vec& distinct_tp,
                const std::vector<cd>& moments,
                const std::vector<cd>& moments_nd);

Mat     import_gos(const std::string& path, int d = -1, int n = -1);

// label per doc: etf_{d}x{n}_{trip}{alpha}.
std::string make_label(int d, int n, int trip, const std::string& alpha);

// ONE place that writes all four files from a frame + its tensor + invariants.
// Returns the stem (path without extension).
//
// trip is computed inside export_all from `tp`; the caller does not supply it.
// `alpha` defaults to "a". `tags` is free-form metadata written into the .inv
// body only (it never affects the filename); pass "" unless there is a genuine
// note to record.
struct ExportInputs {
  Mat        frame;        // for .gos
  Tensor3    tp;           // for .tp/.exa/.inv
  int        d, n;
  int        variant;      // e.g. eps sign, or 0
  std::string alpha = "a"; // provisional letter; default a
  std::string tags  = "";  // free-form .inv note; "" by default
  std::string outdir;
};
std::string export_all(const ExportInputs& in);

} // namespace etf
