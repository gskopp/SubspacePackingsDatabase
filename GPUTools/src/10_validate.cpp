// 09_validate.cpp
#include "10_validate.hpp"
#include "02_io.hpp"
#include "07_invariants.hpp"
#include <cmath>
#include <filesystem>
#include <regex>

namespace fs = std::filesystem;

namespace etf {

ValidationReport validate_frame(const Mat& Phi, int d, int n,
                                int expected_trip, double tol) {
  ValidationReport r;
  Mat G = gm_from_so(normalize_so(Phi));
  double w = welch(d,n);

  // coherence: all off-diagonal |G_ij| == w
  bool coh=true;
  for (int i=0;i<n && coh;i++) for(int j=0;j<n;j++)
    if(i!=j && std::abs(std::abs(G(i,j))-w)>tol){coh=false;break;}
  r.coherence_ok=coh;

  // hermitian
  bool herm=true;
  for(int i=0;i<n&&herm;i++) for(int j=0;j<n;j++)
    if(std::abs(G(i,j)-std::conj(G(j,i)))>tol){herm=false;break;}
  r.hermitian_ok=herm;

  // projection G^2 == (n/d) G
  bool proj=true; double s=(double)n/d;
  for(int i=0;i<n&&proj;i++) for(int j=0;j<n;j++){
    cd acc(0,0); for(int k=0;k<n;k++) acc+=G(i,k)*G(k,j);
    if(std::abs(acc - s*G(i,j))>tol*10){proj=false;break;}
  }
  r.projection_ok=proj;

  // distinct triple product count
  if (expected_trip<0) r.trip_count_ok=true;
  else r.trip_count_ok = (number_tp(tp_from_gm_cpu(G), tol) == expected_trip);

  return r;
}

ValidationReport validate_file(const std::string& path, double tol) {
  Dims dm = extract_dims(path);
  int trip = extract_trip(path);
  std::string ext = fs::path(path).extension().string();
  Mat Phi;
  if (ext==".gos") Phi = import_gos(path, dm.d, dm.n);
  else {
    // .tp/.exa carry triple products, not the frame; reconstructing a frame from
    // a numeric LUT is the fragile reverse path. Validate via the sibling .gos.
    std::string gos = path.substr(0, path.find_last_of('.')) + ".gos";
    Phi = import_gos(gos, dm.d, dm.n);
  }
  ValidationReport r = validate_frame(Phi, dm.d, dm.n, trip, tol);
  r.file = path;
  return r;
}

std::vector<ValidationReport> validate_pattern(const std::string& pattern, double tol) {
  std::vector<ValidationReport> out;
  fs::path p(pattern);
  fs::path dir = p.has_parent_path()? p.parent_path() : fs::current_path();
  std::string pat = p.filename().string();
  // translate simple glob (* -> .*) to regex, restrict to etf*.{gos,tp,exa}
  std::string re; for(char c:pat){ if(c=='*') re+=".*"; else if(c=='.') re+="\\."; else re+=c; }
  std::regex rx(re);
  for (auto& e : fs::directory_iterator(dir)) {
    std::string fn = e.path().filename().string();
    if (!std::regex_match(fn, rx)) continue;
    std::string ext=e.path().extension().string();
    if (fn.rfind("etf",0)==0 && (ext==".gos"||ext==".tp"||ext==".exa"))
      out.push_back(validate_file(e.path().string(), tol));
  }
  return out;
}

} // namespace etf
