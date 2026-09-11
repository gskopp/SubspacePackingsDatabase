// 08_exactify.cpp — cross-platform (POSIX + Windows/MSVC).
#include "08_exactify.hpp"
#include <cstdio>
#include <cstdlib>
#include <string>

#if defined(_WIN32)
  #include <windows.h>
#else
  #include <sys/stat.h>
  #include <sys/wait.h>
  #include <unistd.h>
  #include <limits.h>
#endif

namespace etf {

static bool exists(const std::string& p){
#if defined(_WIN32)
  DWORD a = GetFileAttributesA(p.c_str());
  return a != INVALID_FILE_ATTRIBUTES;
#else
  struct stat s; return stat(p.c_str(),&s)==0;
#endif
}

// directory containing the running binary, so .wl files placed next to the
// binary are found regardless of the current working directory.
static std::string exe_dir(){
#if defined(_WIN32)
  char buf[MAX_PATH];
  DWORD len = GetModuleFileNameA(nullptr, buf, MAX_PATH);
  if(len==0 || len==MAX_PATH) return ".";
  std::string p(buf, len);
  auto slash = p.find_last_of("/\\");
  return (slash==std::string::npos) ? "." : p.substr(0,slash);
#else
  char buf[PATH_MAX];
  ssize_t len = readlink("/proc/self/exe", buf, sizeof(buf)-1);
  if(len<=0) return ".";
  buf[len]=0;
  std::string p(buf);
  auto slash=p.find_last_of('/');
  return (slash==std::string::npos) ? "." : p.substr(0,slash);
#endif
}

bool wolframscript_available(){
  // `where` on Windows, `command -v` on POSIX; both exit 0 iff resolvable.
#if defined(_WIN32)
  int rc = std::system("where wolframscript >NUL 2>NUL");
#else
  int rc = std::system("command -v wolframscript >/dev/null 2>&1");
#endif
  return rc == 0;
}

// Normalize std::system()'s raw return into the child's exit code.
static int child_exit_code(int raw){
#if defined(_WIN32)
  return raw;                 // on Windows system() already returns the exit code
#else
  if(raw != -1 && WIFEXITED(raw)) return WEXITSTATUS(raw);
  return raw;
#endif
}

ExactifyResult exactify_validate_frame_ex(const std::string& in_gos,int d,int n,
                                          const std::string& script){
  ExactifyResult res;

  // 1) locate the .wl script: next to the binary first, then CWD.
  std::string dir = exe_dir();
  std::string script_path = dir + "/" + script;
  if(!exists(script_path)) script_path = script;
  if(!exists(script_path)){
    fprintf(stderr,"[exactify] cannot find %s (looked in %s and CWD).\n"
                   "           Place the .wl files next to the binary.\n",
            script.c_str(), dir.c_str());
    res.status = ExactifyStatus::NoScript;
    return res;
  }

  // 2) ensure wolframscript is present so "missing Mathematica" is reported
  //    distinctly from a genuine validation failure.
  if(!wolframscript_available()){
    fprintf(stderr,"[exactify] wolframscript not found on PATH.\n"
                   "           Install Wolfram Engine/Mathematica, or pass --allow-approx\n"
                   "           to export the UNCERTIFIED numerical frame (not recommended).\n");
    res.status = ExactifyStatus::NoWolframscript;
    return res;
  }

  // 3) run the round trip. Quote all paths so spaces in directories are safe.
  std::string out = in_gos + ".ret";
  std::string cmd = "wolframscript -file \"" + script_path + "\" \"" + in_gos +
                    "\" \"" + out + "\" " + std::to_string(d) + " " + std::to_string(n);
  int raw  = std::system(cmd.c_str());
  res.rc   = raw;
  int code = child_exit_code(raw);

  if(code == 2){   // .wl exits 2 on validation failure (cohOK/projOK false)
    fprintf(stderr,"[exactify] Mathematica REJECTED the frame for %s "
                   "(failed ETF validation).\n", in_gos.c_str());
    res.status = ExactifyStatus::ValidationFailed;
    return res;
  }
  if(code != 0){
    fprintf(stderr,"[exactify] wolframscript rc=%d for %s (run error).\n",
            code, in_gos.c_str());
    res.status = ExactifyStatus::RunError;
    return res;
  }
  if(!exists(out)){
    fprintf(stderr,"[exactify] rc=0 but no returned frame for %s.\n", in_gos.c_str());
    res.status = ExactifyStatus::NoOutput;
    return res;
  }
  res.status = ExactifyStatus::Ok;
  res.returned_path = out;
  return res;
}

std::string exactify_validate_frame(const std::string& in_gos,int d,int n,
                                    const std::string& script){
  ExactifyResult r = exactify_validate_frame_ex(in_gos,d,n,script);
  return r.ok() ? r.returned_path : std::string();
}

} // namespace etf
