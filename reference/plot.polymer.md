# Plot a polymer

Draw the triangles of the mesh. With `show_intersection = TRUE` the
triangles covered by two or more input features are filled.

## Usage

``` r
# S3 method for class 'polymer'
plot(x, ..., show_intersection = FALSE)
```

## Arguments

- x:

  A polymer.

- ...:

  Arguments passed to
  [`graphics::polygon()`](https://rdrr.io/r/graphics/polygon.html), such
  as `col` and `border`.

- show_intersection:

  Logical, fill the triangles covered more than once (default `FALSE`).

## Value

`x`, invisibly.

## Examples

``` r
plot(polymer(A, B, C))

plot(polymer(A, B, C), show_intersection = TRUE, col = "firebrick")
```
