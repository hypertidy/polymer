# Build a polymer mesh from polygon layers

Combine one or more polygon layers into a single triangle mesh, where
every input edge from every layer is kept as a chain of mesh edges, and
record which input features cover each triangle.

## Usage

``` r
polymer(..., engine = "cdtr")
```

## Arguments

- ...:

  Polygon layers. Each can be anything the 'wk' package can read (an
  'sf' data frame or `sfc`, a
  [`wk::wkb()`](https://paleolimbot.github.io/wk/reference/wkb.html) or
  [`wk::wkt()`](https://paleolimbot.github.io/wk/reference/wkt.html)
  vector, and so on). Data frames keep their non-geometry columns as
  attributes.

- engine:

  The triangulation engine. Only `"cdtr"` is available.

## Value

A `polymer` object, a list with

- `mesh`: a `wkpool_tri` (see
  [`wkpool::new_wkpool_tri()`](https://rdrr.io/pkg/wkpool/man/new_wkpool_tri.html))
  holding the triangles on a shared vertex pool. The vertex table has an
  `origin` column: `"input"` for input vertices and `"crossing"` for
  vertices inserted where input edges cross.

- `index`: a data frame with one row per triangle and covering feature:
  `triangle` (row in `mesh`), `layer` (position of the layer in `...`)
  and `feature` (row within that layer).

- `layers`: a list with one data frame of attributes per layer (no
  geometry).

- `crs`: the coordinate reference system shared by the inputs.

## Details

All the inputs' edges are triangulated together in one constrained
Delaunay triangulation. Where edges from different features cross, the
crossing is inserted as a new vertex, so the mesh is a partition of the
plane that respects every input boundary. Vertices are shared on exact
coordinate equality before triangulation.

Each triangle is then labelled with the features that contain it: a
triangle is inside a feature when its centroid is inside an odd number
of the feature's rings, so holes are subtracted. Triangles covered by no
feature (gaps enclosed by the inputs) are dropped.

The result is the mesh and its index, not a set of polygons. Use
[`fuse()`](https://hypertidy.github.io/polymer/reference/fuse.md) or
[`layer_n()`](https://hypertidy.github.io/polymer/reference/fuse.md) to
get polygons back.

## Examples

``` r
pool <- polymer(A, B, C)
pool
#> polymer mesh:
#> Layers:    3
#> Features:  6
#> Triangles: 42
#> Vertices:  26 (10 at crossings)
#> Overlaps:  15 triangles covered more than once, at most 3 deep
head(pool$index)
#>   triangle layer feature
#> 1        1     1       3
#> 2        1     2       1
#> 3        2     1       1
#> 4        3     1       1
#> 5        4     1       2
#> 6        5     1       4

## the mesh is a 'wkpool' triangle pool
pool$mesh
#> <wkpool_tri[42 triangles, 26 vertices] CRS=NA>
#>  [1] <triangle: 12,17,19> <triangle: 4,16,1>   <triangle: 16,11,1> 
#>  [4] <triangle: 24,15,2>  <triangle: 10,6,17>  <triangle: 23,18,22>
#>  [7] <triangle: 19,13,4>  <triangle: 9,6,10>   <triangle: 3,17,6>  
#> [10] <triangle: 1,11,2>   <triangle: 19,4,12>  <triangle: 12,4,8>  
#> [13] <triangle: 13,16,4>  <triangle: 19,21,13> <triangle: 24,2,11> 
#> [16] <triangle: 7,9,10>   <triangle: 3,14,22>  <triangle: 2,15,5>  
#> [19] <triangle: 3,6,14>   <triangle: 6,15,14>  <triangle: 6,5,15>  
#> [22] <triangle: 7,12,8>   <triangle: 20,10,17> <triangle: 20,17,12>
#> [25] <triangle: 21,19,23> <triangle: 3,18,17>  <triangle: 16,13,21>
#> [28] <triangle: 18,19,17> <triangle: 20,7,10>  <triangle: 20,12,7> 
#> [31] <triangle: 25,16,21> <triangle: 18,23,19> <triangle: 15,22,14>
#> [34] <triangle: 3,22,18>  <triangle: 25,21,26> <triangle: 26,24,11>
#> [37] <triangle: 24,22,15> <triangle: 22,24,23> <triangle: 25,11,16>
#> [40] <triangle: 26,11,25> <triangle: 26,23,24> <triangle: 23,26,21>
```
