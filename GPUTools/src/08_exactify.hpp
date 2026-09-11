// 08_exactify.hpp — round-trip to Mathematica for non-conf-double frames.
//
// C++ generates an approximate frame and writes it as a numeric .gos handoff.
// Mathematica (exactify_validate.wl via wolframscript) exactifies the frame,
// validates it as an ETF, and writes the frame BACK as a numeric high-precision
// .gos. C++ then reads that returned frame and builds/exports everything.
//
// POLICY: the C++ driver is the authority. main.cpp REQUIRES this round-trip for
// altproj/minimize (compiled default ETF_REQUIRE_WOLFRAM=1) and fails hard if it
// cannot run — it never silently exports an uncertified frame. The status enum
// distinguishes "wolframscript not on PATH" (environment) from "Mathematica ran
// and REJECTED the frame" (math), so the exit message is honest.
#pragma once
#include "01_toolbox.hpp"
#include <string>

namespace etf {

enum class ExactifyStatus {
  Ok,               // validated; returned .gos written
  NoWolframscript,  // wolframscript not found on PATH
  NoScript,         // the .wl script could not be located
  ValidationFailed, // Mathematica ran but rejected the frame (rc==2)
  RunError,         // wolframscript ran but errored (other nonzero rc)
  NoOutput          // ran, rc==0, but no returned frame appeared
};

struct ExactifyResult {
  ExactifyStatus status = ExactifyStatus::RunError;
  std::string    returned_path;   // valid only when status==Ok
  int            rc = -1;         // raw wolframscript return code
  bool ok() const { return status == ExactifyStatus::Ok; }
};

// True if `wolframscript` is resolvable on PATH.
bool wolframscript_available();

// Full round trip. `script` resolved next to the running binary first, then CWD.
// Never throws; encodes every outcome in the returned status.
ExactifyResult exactify_validate_frame_ex(const std::string& in_gos, int d, int n,
                                          const std::string& script = "exactify_validate.wl");

// Back-compat wrapper: path on success, "" otherwise.
std::string exactify_validate_frame(const std::string& in_gos, int d, int n,
                                    const std::string& script = "exactify_validate.wl");

} // namespace etf
