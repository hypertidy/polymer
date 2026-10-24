# Simple overlapping layers

`A`, `B` and `C` are three partially overlapping polygon layers with
different attributes: `A` is four squares in a 2 by 2 grid over the unit
square, `B` is a single triangle and `C` a single thin rectangle.

## Format

'sf' data frames with no coordinate reference system:

- `A`: 4 rows, `layer`, `a` and `geometry`.

- `B`: 1 row, `b` and `geometry`.

- `C`: 1 row, `c` and `geometry`.

## Examples

``` r
polymer(A, B, C)
#> polymer mesh:
#> Layers:    3
#> Features:  6
#> Triangles: 42
#> Vertices:  26 (10 at crossings)
#> Overlaps:  15 triangles covered more than once, at most 3 deep
```
