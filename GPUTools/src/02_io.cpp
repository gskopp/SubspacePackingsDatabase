// 02_io.cpp
#include "02_io.hpp"
#include "07_invariants.hpp"
#include <fstream>
#include <sstream>
#include <iomanip>
#include <stdexcept>

namespace etf {

static void write_lut(const std::string& path, const LUT& lut){
  std::ofstream f(path); if(!f) throw std::runtime_error("open "+path);
  f<<std::setprecision(17)<<"[";
  for(size_t k=0;k<lut.distinct.size();++k){ if(k)f<<",";
    f<<"["<<lut.distinct[k].real()<<","<<lut.distinct[k].imag()<<"]"; }
  f<<"]\n[";
  for(size_t i=0;i<lut.idx.size();++i){ if(i)f<<","; f<<lut.idx[i]; }
  f<<"]\n";
}

void export_gos(const Mat& Phi,const std::string& path,int digits){
  std::ofstream f(path); if(!f) throw std::runtime_error("open "+path);
  f<<std::setprecision(digits);
  for(double v:gos_from_so(Phi)) f<<v<<"\n";
}
void export_tp(const Tensor3& TP,const std::string& path){ write_lut(path, array_to_lut(TP)); }
void export_exa(const Mat& S,const std::string& path){ write_lut(path, array_to_lut(S)); }

void export_inv(const std::string& path,int d,int n,int trip,int variant,
                const char* tags,double coherence,double welch_bound,
                const Vec& dtp,const std::vector<cd>& mom,const std::vector<cd>& mom_nd){
  std::ofstream f(path); if(!f) throw std::runtime_error("open "+path);
  f<<std::setprecision(17);
  f<<"d "<<d<<"\nn "<<n<<"\ntrip "<<trip<<"\nvariant "<<variant<<"\n";
  f<<"tags "<<(tags&&*tags?tags:"-")<<"\n";
  f<<"coherence "<<coherence<<"\nwelch "<<welch_bound<<"\n";
  for(size_t m=0;m<mom.size();++m)   f<<"mu"<<m+1<<" "<<mom[m].real()<<" "<<mom[m].imag()<<"\n";
  for(size_t m=0;m<mom_nd.size();++m) f<<"mu"<<m+1<<"_nd "<<mom_nd[m].real()<<" "<<mom_nd[m].imag()<<"\n";
  f<<"# distinct triple products ("<<dtp.size()<<")\n";
  for(cd z:dtp) f<<"tp "<<z.real()<<" "<<z.imag()<<"\n";
}

Mat import_gos(const std::string& path,int d,int n){
  std::ifstream f(path); if(!f) throw std::runtime_error("open "+path);
  std::vector<double> g; double x; while(f>>x) g.push_back(x);
  if(d<0||n<0){ Dims dm=extract_dims(path); d=dm.d; n=dm.n; }
  return so_from_gos(g,d,n);
}

std::string make_label(int d,int n,int trip,const std::string& alpha){
  std::ostringstream o; o<<"etf_"<<d<<"x"<<n<<"_"<<trip<<alpha; return o.str();
}

std::string export_all(const ExportInputs& in){
  // trip is COMPUTED here, not supplied. numberTP = # distinct triple products
  // over the whole tensor, degenerate entries included (matches frameInvariants.wl).
  int trip = number_tp(in.tp);

  std::string label = make_label(in.d,in.n,trip,in.alpha);   // etf_dxn_tripa
  std::string stem  = in.outdir + "/" + label;

  export_gos(in.frame, stem+".gos");
  export_tp (in.tp,    stem+".tp");
  export_exa(tp_slice_from_tp(in.tp,0), stem+".exa");

  Vec dtp = distinct_tp(in.tp);
  std::vector<cd> mom, mom_nd;
  for(int m=1;m<=6;m++){ mom.push_back(moment(in.tp,m)); mom_nd.push_back(moment_nd(in.tp,m)); }
  double w = welch(in.d,in.n);
  export_inv(stem+".inv", in.d,in.n,trip,in.variant,in.tags.c_str(), w, w, dtp, mom, mom_nd);
  return stem;
}

} // namespace etf
