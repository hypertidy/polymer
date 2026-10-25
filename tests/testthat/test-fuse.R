test_that("fuse() matches the A, B, C overlaps", {
  f <- fuse(polymer(A, B, C))
  expect_named(f, c("n", "origins", "geometry"))
  expect_s3_class(f$geometry, "wk_wkb")
  expect_equal(nrow(f), 10L)
  by_n <- tapply(piece_area(f$geometry), f$n, sum)
  ## areas by overlap count from sf::st_intersection() on the same layers
  expect_equal(as.vector(by_n), c(0.816042513, 0.176139974, 0.007817513), tolerance = 1e-8)
  expect_equal(f$origins[[which(f$n == 3L)]],
               data.frame(layer = 1:3, feature = c(1L, 1L, 1L)))
})

test_that("layer_n() keeps pieces covered at least n times", {
  x <- polymer(A, B, C)
  expect_equal(layer_n(x, 3)$n, 3L)
  expect_true(all(layer_n(x)$n >= 2L))
  expect_equal(nrow(layer_n(x, 1)), nrow(fuse(x)))
  expect_equal(nrow(layer_n(x, 4)), 0L)
})

test_that("the polymer issue 7 stress input is an exact partition", {
  ## 100 overlapping buffered lines in one layer; sf::st_intersection() on
  ## this input is slow and its pieces overlap each other
  for (seed in 1:3) {
    b <- buffered_lines(seed)
    f <- expect_partition(polymer(b), sum(piece_area(b)), tolerance = 1e-10)
    expect_gt(max(f$n), 3L)
  }
})

test_that("fuse() pieces are valid", {
  skip_if_not_installed("sf")
  f <- fuse(polymer(buffered_lines(1)))
  expect_true(all(sf::st_is_valid(sf::st_as_sfc(f$geometry))))
})

test_that("fuse() checks its input", {
  expect_error(fuse(list()), "must be a polymer")
})

## a ring's area relative to its first vertex, so it does not lose precision
## to the size of the coordinates (rings repeat their first vertex at the end)
local_area <- function(x, y) {
  x <- x - x[1L]
  y <- y - y[1L]
  j <- c(seq_along(x)[-1L], 1L)
  sum(x * y[j] - x[j] * y) / 2
}

test_that("fuse() keeps a piece smaller than the coordinate rounding (#18)", {
  ## one piece of a 500-segment overlay: area 1.4e-13 at coordinates near
  ## (31, 77), where a shoelace sum over absolute coordinates gives exactly 0
  tri <- wkt_poly(paste0(
    "POLYGON ((31.160398626022253 76.585971564176361, ",
    "31.160399417737235 76.585971473120267, ",
    "31.160399423995848 76.585971829381265, ",
    "31.160398626022253 76.585971564176361))"))
  pieces <- fuse(polymer(tri))
  expect_equal(nrow(pieces), 1L)
  expect_equal(pieces$n, 1L)
  co <- wk::wk_coords(pieces$geometry)
  expect_equal(length(unique(co$ring_id)), 1L)
  expect_gt(local_area(co$x, co$y), 0)
})

test_that("fuse() keeps tiny pieces far from the origin (#18)", {
  set.seed(1)
  n <- 40
  x0 <- 1e5 + seq_len(n)
  y0 <- 1e5 + runif(n)
  s <- 1e-6
  tris <- wk::wkt(sprintf(
    "POLYGON ((%.17g %.17g, %.17g %.17g, %.17g %.17g, %.17g %.17g))",
    x0, y0,
    x0 + s * runif(n, 0.5, 1), y0 + s * runif(n, -0.2, 0.2),
    x0 + s * runif(n, -0.2, 0.2), y0 + s * runif(n, 0.5, 1),
    x0, y0))
  pieces <- fuse(polymer(tris))
  expect_equal(nrow(pieces), n)
  expect_true(all(pieces$n == 1L))
  co <- wk::wk_coords(pieces$geometry)
  ## one ring per piece, each counter-clockwise
  expect_equal(length(unique(paste(co$feature_id, co$ring_id))), n)
  a <- vapply(split(co, co$feature_id),
              function(r) local_area(r$x, r$y), numeric(1))
  expect_true(all(a > 0))
})
