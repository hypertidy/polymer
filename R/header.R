#' Using the header from another package
#'
#' The labelling that [polymer()] uses to find which input features cover
#' each triangle is a dependency-free, header-only C++17 file,
#' `polymer/overlay.hpp` (MIT). Another package can use it directly, for
#' example to label a mesh of its own from a constrained triangulation with
#' its `edge_sources`, without going through R.
#'
#' @section Setting up:
#' In the other package's `DESCRIPTION`, link to polymer (and to 'cpp11' if
#' that is how it talks to R; the header itself needs only the C++ standard
#' library):
#'
#' ```
#' LinkingTo: cpp11, polymer
#' ```
#'
#' The header needs C++17, which is not the default in every R version that
#' polymer supports, so set it in `src/Makevars` (and `src/Makevars.win`):
#'
#' ```
#' CXX_STD = CXX17
#' ```
#'
#' Then `#include <polymer/overlay.hpp>`. Everything is in the
#' `polymer::overlay` namespace.
#'
#' @section What it takes:
#' `polymer::overlay::label()` takes borrowed, 0-based views of
#' * a `Mesh`: counter-clockwise triangles (`tri`, three vertex ids each)
#'   and the constraint edges (`edge`, two vertex ids each);
#' * `EdgeSources`: one row for each constraint edge and input segment
#'   lying on it, with the segment's `feature` and whether it runs the same
#'   way as the edge (`forward`), as from `cdtr::cdt_triangulate(edge_sources
#'   = TRUE)`;
#' * a `FillRule`: `NonZero`, `EvenOdd` or `Positive`.
#'
#' It returns `Labels`: `tri_set`, the covering set of each triangle (`-1`
#' when uncovered), the sets in compressed form (`set_offset`,
#' `set_feature`), and `inconsistent_edges`, a self-check count where a
#' non-zero value means the noding is broken. No coordinates are used. The
#' header's own comments give the details.
#'
#' @section A minimal example:
#' One counter-clockwise triangle whose three edges belong to feature 0, all
#' running the same way as the edge:
#'
#' ```
#' #include <cpp11.hpp>
#' #include <polymer/overlay.hpp>
#'
#' [[cpp11::register]]
#' cpp11::writable::integers label_one() {
#'   using namespace polymer::overlay;
#'   std::vector<int32_t> tri = {0, 1, 2};
#'   std::vector<int32_t> edge = {0, 1,  1, 2,  2, 0};
#'   std::vector<int32_t> es_edge = {0, 1, 2};
#'   std::vector<int32_t> es_feature = {0, 0, 0};
#'   std::vector<int8_t> es_forward = {1, 1, 1};
#'
#'   Mesh m;
#'   m.tri = tri.data();   m.n_tri = 1;
#'   m.edge = edge.data(); m.n_edge = 3;
#'   EdgeSources s;
#'   s.edge = es_edge.data();       s.feature = es_feature.data();
#'   s.forward = es_forward.data(); s.n = 3;
#'
#'   Labels l = label(m, s, FillRule::NonZero);
#'   // l.tri_set is {0}: the triangle is covered by set 0, whose only
#'   // feature is 0; l.inconsistent_edges is 0
#'   return cpp11::writable::integers(l.tri_set.begin(), l.tri_set.end());
#' }
#' ```
#'
#' `label()` throws `std::exception` on malformed input, so catch it at the
#' R boundary (cpp11 does this for exceptions that escape a registered
#' function).
#'
#' @name overlay-header
#' @aliases polymer-header
#' @seealso [polymer()], which calls the header on the output of
#'   `cdtr::cdt_triangulate()`.
NULL
