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
