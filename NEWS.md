# polymer (development version)

* Triangles are now labelled by propagating each feature's winding number
  across the mesh, using the `edge_sources` table from 'cdtr' (which input
  segments lie on each mesh edge, and which way they run). This uses no
  coordinates, so sliver triangles can no longer be mislabelled, and it is
  about ten times faster than testing centroids against every ring. Edges
  not on the walk are checked for agreement, and `polymer()` warns when they
  disagree, which means the noding is broken. Requires 'cdtr' with
  `edge_sources` (development version, 0.1.0.9000).
* The labelling core is a dependency-free C++17 header,
  `inst/include/polymer/overlay.hpp` (MIT), usable from other packages with
  `LinkingTo: polymer`. It implements mesh spec v0.2 Annex A.
* New `rule` argument to `polymer()`: `"nonzero"` (the new default),
  `"evenodd"` (the 0.3.0 behaviour) or `"positive"`. Rings are oriented
  first, shells counter-clockwise and holes clockwise, the first ring of each
  polygon being its shell. Under `"nonzero"` a duplicated part stays covered
  where `"evenodd"` cancelled it.
* New `label` argument to `polymer()`: `"pip"` keeps the centroid
  point-in-polygon labelling of 0.3.0 (now honouring `rule`) for comparison.
  It will be removed in a later release.
* Input segments shared by several rings are no longer merged before
  triangulation, so every ring's edges reach the mesh with their direction.

# polymer 0.3.0

polymer is rebuilt on 'wkpool' and 'cdtr'. This release breaks the old
interface.

* polymer is now MIT licensed (was GPL-3), matching 'cdtr' and 'wk', so the
  overlay core can be reused freely.

* `fuse()` no longer fails when a piece is very small relative to the size
  of its coordinates. Ring areas, used to tell outer rings from holes, are
  now computed relative to each ring's first vertex (#18).
* New vignette "Identity and n-ary overlay with polymer" works through
  r-spatial/sf#1959 (Esri Identity) and r-spatial/sf#2242 (n-ary
  intersection of one layer), shows Intersect, Union, Identity, Erase and
  Symmetrical Difference as filters on one mesh, and sets out where 'sf' prepares the inputs
  and where the workflow stays on polymer's own tables.
* New article "Overlay robustness: the history of issue 7" records issue 7
  from 2020 to 2026, reruns its examples against GEOS 3.12.1 and 3.15.0,
  and documents where polymer itself still fails.
* `polymer()` takes any number of layers as anything 'wk' can read, not only
  'sf' data frames. Vertices are shared on exact coordinate equality with
  'wkpool', and all edges are triangulated once with 'cdtr', which inserts
  crossings as new vertices.
* Triangles are now labelled per feature rather than per ring, so holes are
  subtracted instead of being counted as an extra overlap.
* The polymer object holds `mesh` (a 'wkpool' triangle pool), `index`
  (triangle, layer, feature), `layers` (attributes of each input) and `crs`.
  The old `input`, `primitives` and `geometry_map` elements are gone.
* New `fuse()` dissolves the mesh into overlay pieces, one per distinct set
  of covering features, as an exact partition with shared vertices.
* `layer_n()` now returns the dissolved pieces covered at least `n` times,
  with geometry as `wk::wkb()`, instead of one 'sf' polygon per triangle.
* 'RTriangle' (non-commercial licence), 'silicate', 'gibble', 'sf' and the
  tidyverse packages are no longer dependencies; 'sf' is in Suggests. The
  `%>%` re-export is removed.
* The `soil` and `field` data now carry a current 'sf' crs object.

* Now use wk and geos for input handling, and output of `layer_n()`. 
