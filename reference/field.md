# Field and soil polygons

Two polygon layers used to show overlay: `soil` has 12 polygons with a
made-up soil type, `field` has two rectangular fields that cross them.
The example comes from a question on the GIS Stack Exchange
(<https://gis.stackexchange.com/a/230161>), using the Luxembourg
boundaries shipped with the 'raster' package.

## Format

'sf' data frames in longitude and latitude (WGS84):

- `soil`: 12 rows, `soil` (character) and `geometry`.

- `field`: 2 rows, `field` (character) and `geoms`.

## Examples

``` r
polymer(soil, field)
#> polymer mesh:
#> Layers:    2
#> Features:  14
#> Triangles: 4166
#> Vertices:  2306 (21 at crossings)
#> Overlaps:  2224 triangles covered more than once, at most 2 deep
```
