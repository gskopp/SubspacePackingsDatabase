// etfcheck.cpp — standalone ETF verifier.
//
// Reads every .gos frame file in its own folder (or a folder given as argv[1]),
// checks whether each is a genuine equiangular tight frame, and prints a PASS/FAIL
// line per file to the console with the reason on failure.
//
// Independent of the generation apparatus: no CUDA, no LAPACK, one translation
// unit. Verification needs only the Gram matrix, which is cheap to form directly.
//
// Checks (all must hold within tolerance):
//   1. unit-norm columns
//   2. equiangular : every off-diagonal |<v_i,v_j>| equals the Welch bound
//   3. tight       : G^2 = (n/d) G   (equivalently Phi Phi^* = (n/d) I)
//   4. (if filename encodes trip) the distinct-triple-product count matches
//
// Filename convention (label doc): etf_{d}x{n}_{trip}{alpha}.gos
// d,n are read from the filename; if absent, inferred from the vector length is
// impossible (d and n both unknown), so such files are reported as UNREADABLE.
//
// Build:  g++ -O2 -std=c++17 etfcheck.cpp -o etfcheck
// Run:    ./etfcheck            (checks .gos files in the current folder)
//         ./etfcheck path/to/dir

#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <complex>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <filesystem>
#include <regex>

namespace fs = std::filesystem;
using cd = std::complex<double>;

struct Dims { int d=0, n=0, trip=-1; bool ok=false; };

// parse etf_{d}x{n}_{trip}{alpha}.gos  (trip/alpha optional)
static Dims parse_name(const std::string& fname){
  Dims r;
  std::smatch m;
  std::regex re("(\\d+)x(\\d+)(?:_(\\d+))?");
  if(std::regex_search(fname, m, re)){
    r.d = std::stoi(m[1]); r.n = std::stoi(m[2]);
    if(m[3].matched) r.trip = std::stoi(m[3]);
    r.ok = true;
  }
  return r;
}

// read a .gos file: flat list of reals, first d*n are Re (column-major), next d*n Im
static bool read_gos(const std::string& path, int d, int n, std::vector<cd>& Phi){
  std::ifstream f(path);
  if(!f) return false;
  std::vector<double> g; double x;
  // tolerate commas/brackets/whitespace
  std::string tok;
  while(f >> tok){
    // strip non-numeric edges
    for(char& c : tok) if(c=='['||c==']'||c==',') c=' ';
    std::stringstream ss(tok);
    while(ss >> x) g.push_back(x);
  }
  size_t need = (size_t)2*d*n;
  if(g.size() < need) return false;
  Phi.assign((size_t)d*n, cd(0,0));
  size_t half=(size_t)d*n;
  for(size_t p=0;p<half;p++) Phi[p]=cd(g[p], g[half+p]);
  return true;
}

// column-major access: column c, row r  (d rows, n cols)
static inline cd at(const std::vector<cd>& Phi,int d,int r,int c){ return Phi[(size_t)c*d+r]; }

struct Result { bool pass; std::string reason; };

static Result check(const std::vector<cd>& Phi,int d,int n,int trip_expected){
  const double tol = 1e-6;
  double welch = std::sqrt(double(n-d)/(double(d)*double(n-1)));

  // 1. unit columns
  for(int c=0;c<n;c++){
    double s=0; for(int r=0;r<d;r++) s+=std::norm(at(Phi,d,r,c));
    if(std::abs(std::sqrt(s)-1.0) > tol)
      return {false, "column "+std::to_string(c)+" not unit norm ("+std::to_string(std::sqrt(s))+")"};
  }

  // build Gram G[i][j] = <v_i,v_j> = sum_r conj(Phi(r,i)) Phi(r,j)
  std::vector<cd> G((size_t)n*n);
  for(int i=0;i<n;i++) for(int j=0;j<n;j++){
    cd acc(0,0); for(int r=0;r<d;r++) acc+=std::conj(at(Phi,d,r,i))*at(Phi,d,r,j);
    G[(size_t)i*n+j]=acc;
  }

  // 2. equiangular: all off-diagonal |G_ij| == welch
  double maxdev=0;
  for(int i=0;i<n;i++) for(int j=0;j<n;j++) if(i!=j){
    double a=std::abs(G[(size_t)i*n+j]);
    maxdev=std::max(maxdev, std::abs(a-welch));
  }
  if(maxdev > tol)
    return {false, "not equiangular (max |offdiag-welch| = "+std::to_string(maxdev)+")"};

  // 3. tight: G^2 = (n/d) G
  double s=(double)n/d, projdev=0;
  for(int i=0;i<n;i++) for(int j=0;j<n;j++){
    cd acc(0,0); for(int k=0;k<n;k++) acc+=G[(size_t)i*n+k]*G[(size_t)k*n+j];
    projdev=std::max(projdev, std::abs(acc - s*G[(size_t)i*n+j]));
  }
  if(projdev > tol*10)
    return {false, "not tight (max |G^2-(n/d)G| = "+std::to_string(projdev)+")"};

  // 4. trip count, if the filename claimed one
  if(trip_expected>=0){
    std::vector<cd> distinct;
    for(int i=0;i<n;i++) for(int j=0;j<n;j++) for(int k=0;k<n;k++){
      cd t=G[(size_t)i*n+j]*G[(size_t)j*n+k]*G[(size_t)k*n+i];
      bool found=false; for(const cd& u:distinct) if(std::abs(u-t)<1e-6){found=true;break;}
      if(!found) distinct.push_back(t);
    }
    if((int)distinct.size()!=trip_expected)
      return {false, "trip mismatch: filename says "+std::to_string(trip_expected)
                     +", computed "+std::to_string(distinct.size())};
  }

  return {true, ""};
}

int main(int argc,char** argv){
  std::string dir = (argc>1) ? argv[1] : ".";
  if(!fs::exists(dir) || !fs::is_directory(dir)){
    fprintf(stderr,"not a directory: %s\n", dir.c_str());
    return 1;
  }

  std::vector<std::string> files;
  for(auto& e : fs::directory_iterator(dir)){
    if(e.path().extension()==".gos"){
      std::string fn=e.path().filename().string();
      if(fn.rfind("_handoff",0)==0) continue;   // skip transient handoff files
      files.push_back(e.path().string());
    }
  }
  std::sort(files.begin(), files.end());

  if(files.empty()){ printf("no .gos frame files in %s\n", dir.c_str()); return 0; }

  int pass=0, fail=0, bad=0;
  for(const std::string& path : files){
    std::string name = fs::path(path).filename().string();
    Dims dm = parse_name(name);
    if(!dm.ok){ printf("  UNREADABLE  %s  (no dims in filename)\n", name.c_str()); bad++; continue; }
    std::vector<cd> Phi;
    if(!read_gos(path, dm.d, dm.n, Phi)){
      printf("  UNREADABLE  %s  (need %d reals, file short)\n", name.c_str(), 2*dm.d*dm.n); bad++; continue;
    }
    Result r = check(Phi, dm.d, dm.n, dm.trip);
    if(r.pass){ printf("  PASS        %s\n", name.c_str()); pass++; }
    else       { printf("  FAIL        %s  -- %s\n", name.c_str(), r.reason.c_str()); fail++; }
  }

  printf("\n%d passed, %d failed, %d unreadable  (%zu files in %s)\n",
         pass, fail, bad, files.size(), dir.c_str());
  return (fail||bad) ? 1 : 0;
}
