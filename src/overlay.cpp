#include <cpp11.hpp>
#include <polymer/overlay.hpp>

#include <string>
#include <vector>

using namespace cpp11;

// tri and edge are 0-based, row by row (3 and 2 per row); es_* are the
// edge_sources rows with 0-based edge and feature. Returns tri_set
// (0-based, -1 uncovered), set_offset, set_feature (0-based) and the
// self-check count.
[[cpp11::register]]
list overlay_label_cpp(integers tri, integers edge, integers es_edge,
                       integers es_feature, logicals es_forward,
                       std::string rule) {
  polymer::overlay::FillRule fr;
  if (rule == "nonzero") {
    fr = polymer::overlay::FillRule::NonZero;
  } else if (rule == "evenodd") {
    fr = polymer::overlay::FillRule::EvenOdd;
  } else if (rule == "positive") {
    fr = polymer::overlay::FillRule::Positive;
  } else {
    stop("unknown fill rule '%s'", rule.c_str());
  }
  R_xlen_t ns = es_edge.size();
  if (es_feature.size() != ns || es_forward.size() != ns) {
    stop("edge_sources columns differ in length");
  }
  std::vector<int32_t> t(tri.begin(), tri.end());
  std::vector<int32_t> e(edge.begin(), edge.end());
  std::vector<int32_t> se(es_edge.begin(), es_edge.end());
  std::vector<int32_t> sf(es_feature.begin(), es_feature.end());
  std::vector<int8_t> fw((size_t)ns);
  for (R_xlen_t i = 0; i < ns; i++) fw[(size_t)i] = es_forward[i] == TRUE;

  polymer::overlay::Mesh mesh;
  mesh.tri = t.data();
  mesh.n_tri = (int64_t)t.size() / 3;
  mesh.edge = e.data();
  mesh.n_edge = (int64_t)e.size() / 2;
  polymer::overlay::EdgeSources src;
  src.edge = se.data();
  src.feature = sf.data();
  src.forward = fw.data();
  src.n = (int64_t)ns;

  polymer::overlay::Labels lab;
  try {
    lab = polymer::overlay::label(mesh, src, fr);
  } catch (const std::exception& ex) {
    stop("overlay labelling failed: %s", ex.what());
  }

  writable::integers tri_set(lab.tri_set.begin(), lab.tri_set.end());
  writable::doubles set_offset(lab.set_offset.size());
  for (size_t i = 0; i < lab.set_offset.size(); i++) set_offset[i] = (double)lab.set_offset[i];
  writable::integers set_feature(lab.set_feature.begin(), lab.set_feature.end());
  return writable::list({
    "tri_set"_nm = tri_set,
    "set_offset"_nm = set_offset,
    "set_feature"_nm = set_feature,
    "inconsistent_edges"_nm = (double)lab.inconsistent_edges,
    "n_state"_nm = (double)lab.n_state
  });
}
