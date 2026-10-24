# Print a polymer

Print a short summary of a polymer mesh: the number of layers, input
features, triangles, crossing vertices and the deepest overlap.

## Usage

``` r
# S3 method for class 'polymer'
print(x, ...)
```

## Arguments

- x:

  A polymer.

- ...:

  Ignored.

## Value

`x`, invisibly.

## Examples

``` r
print(polymer(A, B, C))
#> polymer mesh:
#> Layers:    3
#> Features:  6
#> Triangles: 42
#> Vertices:  26 (10 at crossings)
#> Overlaps:  15 triangles covered more than once, at most 3 deep
```
