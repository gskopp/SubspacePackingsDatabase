// main.cpp — unified ETF apparatus.
//
//   etf generate --altproj      -d D -n N [--batch B] [--maxit M] [--allow-approx]
//   etf generate --minimize     -d D -n N [--batch B]             [--allow-approx]
//   etf generate --conf-double  -q Q
//   etf validate PATTERN
//   etf equiv    F1.gos F2.gos
//
// altproj/minimize REQUIRE the Mathematica round-trip (exactify + validate via
// wolframscript) and fail hard if it is unavailable; --allow-approx opts out and
// exports an UNCERTIFIED numerical frame. conf-double is closed-form: no round-trip.
//
// conf-double : doubled conference graph (Thm 8 of arXiv:2410.17379) applied to
//               the Paley graph on q vertices -> q x 2q ETF (the paper's 2.Gq).
//               Guaranteed by construction, so no Mathematica round trip. GPU
//               assembly -> convert (GPU) -> export. Emits ONE frame (eps=+1).
// Others      : generate (GPU, approx) -> write numeric frame -> Mathematica
//               exactifies & VALIDATES -> C++ reads it, builds tensor, exports.
//
// LABELS: trip is computed from the tensor; alpha defaults to "a". See 02_io.hpp.
#include "01_toolbox.hpp"
#include "02_io.hpp"
#include "03_gen_altproj.cuh"
#include "04_gen_minimize.cuh"
#include "05_gen_conf_double.cuh"
#include "06_convert.hpp"
#include "07_invariants.hpp"
#include "08_exactify.hpp"
#include "09_equiv.hpp"
#include "10_validate.hpp"
#include "11_hash.hpp"
#include <cstdio>
#include <cstring>
#include <string>
#include <filesystem>

using namespace etf;
enum class Method { Altproj, Minimize, ConfDouble };

static int  argi(int c,char**v,const char*f,int d){ for(int i=1;i<c-1;i++) if(!strcmp(v[i],f)) return atoi(v[i+1]); return d; }
static bool flag(int c,char**v,const char*f){ for(int i=1;i<c;i++) if(!strcmp(v[i],f)) return true; return false; }

// export a frame's full file set (tensor built here). use_gpu picks convert path.
// alpha defaults to "a"; trip is computed inside export_all.
static void export_from_frame(const Mat& Phi,int d,int n,int variant,
                              const std::string& tags,bool use_gpu_convert,
                              const std::string& outdir){
  Mat G = gm_from_so(normalize_so(Phi));
  Tensor3 TP = tp_from_gm(G, use_gpu_convert);   // single host->device seam (06_convert)
  ExportInputs in;
  in.frame = Phi; in.tp = TP; in.d = d; in.n = n;
  in.variant = variant; in.alpha = "a"; in.tags = tags; in.outdir = outdir;
  std::string stem = export_all(in);
  // report the computed trip so the operator sees it (also in the .inv)
  int trip = number_tp(TP);
  printf("wrote %s.{gos,tp,exa,inv}  (trip=%d, hash=%lld)\n",
         stem.c_str(), trip, hash_tp_tensor(TP));
}

int main(int argc,char**argv){
  if(argc<2){ printf("usage: etf <generate|validate|equiv> ...\n"); return 1; }
  std::string cmd=argv[1];
  std::string outdir="out"; std::filesystem::create_directories(outdir);

  if(cmd=="generate"){
    Method m = flag(argc,argv,"--minimize")     ? Method::Minimize
             : (flag(argc,argv,"--conf-double") ||
                flag(argc,argv,"--paley"))       ? Method::ConfDouble  // accept old flag
             :                                     Method::Altproj;

    // ---- conf-double: guaranteed, no validation, all C++, single eps=+1 ----
    if(m==Method::ConfDouble){
      int q=argi(argc,argv,"-q",0);
      if(!conf_double_valid(q)){ printf("conf-double needs -q prime = 1 mod 4 (>=5)\n"); return 1; }
      int d=q,n=2*q;
      int eps=+1;                                  // single variant only
      Mat G=conf_double_gram(q,eps);
      Mat Phi=so_from_gm(G,d);
      export_from_frame(Phi,d,n,eps,/*tags*/"",/*gpu convert*/true,outdir);
      return 0;
    }

    // ---- altproj / minimize: approximate on GPU, round-trip to Mathematica ----
    int d=argi(argc,argv,"-d",0), n=argi(argc,argv,"-n",0);
    if(!d||!n){ printf("need -d and -n\n"); return 1; }
    Mat Phi;
    if(m==Method::Minimize){
      auto r=minimize_generate(d,n,argi(argc,argv,"--batch",256));
      printf("minimize: coherence %.3e welch %.3e\n",r.coherence,welch(d,n));
      Phi=r.frame;
    } else {
      auto r=altproj_generate(d,n,argi(argc,argv,"--batch",256),argi(argc,argv,"--maxit",30000));
      printf("altproj: %d converged, err %.3e\n",r.converged_seeds,r.coh_error);
      Phi=so_from_gm(r.gram,d);
    }

    // ---- MANDATORY Mathematica round-trip (the .cpp driver is the authority) ----
    // For altproj/minimize the exe REQUIRES wolframscript to exactify and validate
    // the frame; it never silently exports an uncertified frame. The compiled-in
    // default (ETF_REQUIRE_WOLFRAM=1, set in CMakeLists) enforces this;
    // --allow-approx is the explicit, per-run opt-out.
#ifndef ETF_REQUIRE_WOLFRAM
#define ETF_REQUIRE_WOLFRAM 1
#endif
    bool allow_approx = flag(argc,argv,"--allow-approx");

    std::string handoff = outdir + "/_handoff_" + std::to_string(d) + "x" + std::to_string(n) + ".gos";
    export_gos(Phi, handoff);

    ExactifyResult ex = exactify_validate_frame_ex(handoff, d, n);
    if(ex.ok()){
      Mat Phi2 = import_gos(ex.returned_path, d, n);
      export_from_frame(Phi2,d,n,0,/*tags*/"",/*gpu convert*/false,outdir);
      return 0;
    }

    switch(ex.status){
      case ExactifyStatus::NoWolframscript:
      case ExactifyStatus::NoScript:
        if(ETF_REQUIRE_WOLFRAM && !allow_approx){
          printf("ERROR: Mathematica round-trip is required but unavailable.\n"
                 "       %s\n"
                 "       Nothing exported. Re-run with --allow-approx to export the\n"
                 "       UNCERTIFIED numerical frame instead (not recommended).\n",
                 ex.status==ExactifyStatus::NoWolframscript
                   ? "wolframscript is not on PATH."
                   : "the .wl validation script could not be found.");
          return 2;
        }
        printf("WARNING: exporting UNCERTIFIED approximate frame (--allow-approx).\n");
        export_from_frame(Phi,d,n,0,/*tags*/"UNCERTIFIED approx",/*gpu convert*/false,outdir);
        return 0;

      case ExactifyStatus::ValidationFailed:
        // Mathematica ran and REJECTED it: genuine math failure, never overridable.
        printf("ERROR: Mathematica validated the frame and REJECTED it "
               "(not an ETF). Nothing exported.\n");
        return 3;

      case ExactifyStatus::RunError:
      case ExactifyStatus::NoOutput:
      default:
        printf("ERROR: Mathematica round-trip failed (rc=%d). Nothing exported.\n",
               ex.rc);
        return 4;
    }
  }

  if(cmd=="validate"){
    auto reps=validate_pattern(argc>2?argv[2]:"*");
    for(auto&r:reps) printf("%s: %s\n",r.file.c_str(),r.passed()?"PASS":"FAIL");
    return 0;
  }
  if(cmd=="equiv"){
    if(argc<4){ printf("usage: etf equiv F1.gos F2.gos\n"); return 1; }
    Dims a=extract_dims(argv[2]),b=extract_dims(argv[3]);
    Mat A=import_gos(argv[2],a.d,a.n),B=import_gos(argv[3],b.d,b.n);
    printf("%s\n", compare_etfs(A,B)?"EQUIVALENT":"NOT equivalent");
    return 0;
  }
  printf("unknown command: %s\n",cmd.c_str());
  return 1;
}
