test_that("polymer builds a mesh and index from several layers", {
  x <- polymer(A, B, C)
  expect_s3_class(x, "polymer")
  expect_named(x, c("mesh", "index", "layers", "crs", "set_id"))
  expect_s3_class(x$mesh, "wkpool_tri")
  expect_length(x$layers, 3L)
  expect_named(x$index, c("triangle", "layer", "feature"))
  ## every triangle is covered at least once
  expect_setequal(unique(x$index$triangle), seq_along(x$mesh))
  expect_equal(max(tabulate(x$index$triangle)), 3L)
  expect_equal(x$layers[[1]]$a, rep(1, 4))
  expect_true(all(wkpool::pool_vertices(x$mesh)$origin %in% c("input", "crossing")))
  expect_output(print(x), "Triangles: 42")
})

test_that("triangles are counter-clockwise", {
  x <- polymer(A, B, C)
  v <- wkpool::pool_vertices(x$mesh)
  t <- wkpool::pool_triangles(x$mesh)
  i0 <- match(t$.vx0, v$.vx)
  i1 <- match(t$.vx1, v$.vx)
  i2 <- match(t$.vx2, v$.vx)
  cross <- (v$x[i1] - v$x[i0]) * (v$y[i2] - v$y[i0]) -
    (v$x[i2] - v$x[i0]) * (v$y[i1] - v$y[i0])
  expect_true(all(cross > 0))
})

test_that("holes are subtracted, not counted as an overlap", {
  donut <- wkt_poly("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0), (3 3, 3 7, 7 7, 7 3, 3 3))")
  plug <- wkt_poly("POLYGON ((4 4, 6 4, 6 6, 4 6, 4 4))")
  f <- fuse(polymer(donut, plug))
  expect_equal(f$n, c(1L, 1L))
  expect_equal(sort(unname(piece_area(f$geometry))), c(4, 84))
})

test_that("shared edges and a crossing square partition exactly", {
  grid <- wkt_poly("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))", "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))")
  over <- wkt_poly("POLYGON ((0.5 0.5, 1.5 0.5, 1.5 1.5, 0.5 1.5, 0.5 0.5))")
  f <- expect_partition(polymer(grid, over), 3)
  expect_equal(sum(f$n == 2L), 2L)
  expect_equal(unname(piece_area(f$geometry)[f$n == 2L]), c(0.25, 0.25))
})

test_that("pieces touching at a vertex dissolve to valid polygons", {
  ## one feature whose two parts touch at a corner
  bow <- wkt_poly("MULTIPOLYGON (((0 0, 1 0, 1 1, 0 1, 0 0)), ((1 1, 2 1, 2 2, 1 2, 1 1)))")
  ## a polygon whose hole touches its outer ring at one point
  notch <- wkt_poly("POLYGON ((3 0, 6 0, 6 3, 3 3, 3 0), (3 0, 5 1, 4 2, 3 0))")
  f <- fuse(polymer(bow, notch))
  expect_equal(nrow(f), 2L)
  expect_equal(sort(unname(piece_area(f$geometry))), c(2, 7.5))
  skip_if_not_installed("sf")
  expect_true(all(sf::st_is_valid(sf::st_as_sfc(f$geometry))))
})

test_that("inputs are checked", {
  expect_error(polymer(), "at least one layer")
  expect_error(polymer(wk::wkt("LINESTRING (0 0, 1 1)")), "polygon")
  expect_error(polymer(data.frame(a = 1)), "geometry column")
  expect_error(polymer(1:3), "geometry vector")
  a <- wk::wk_set_crs(wkt_poly("POLYGON ((0 0, 1 0, 1 1, 0 0))"), "EPSG:4326")
  b <- wk::wk_set_crs(wkt_poly("POLYGON ((0 0, 1 0, 1 1, 0 0))"), "EPSG:3857")
  expect_error(polymer(a, b), "coordinate reference system")
  expect_equal(polymer(a)$crs, "EPSG:4326")
})

test_that("sf layers keep their attributes and crs", {
  skip_if_not_installed("sf")
  x <- polymer(soil, field)
  expect_equal(nrow(x$layers[[1]]), 12L)
  expect_equal(x$layers[[2]]$field, c("x", "y"))
  expect_true(wk::wk_crs_equal(wk::wk_crs(fuse(x)$geometry), sf::st_crs(soil)))
})

test_that("plot draws the mesh", {
  x <- polymer(A, B, C)
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  })
  expect_identical(plot(x), x)
  expect_identical(plot(x, show_intersection = TRUE, col = "grey"), x)
})
