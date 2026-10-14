# polymer 0.3.0

polymer is rebuilt on 'wkpool' and 'cdtr'. This release breaks the old
interface.

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
