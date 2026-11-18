## each feature of each layer as its own dissolved geometry, for GEOS
feature_geos <- function(pool) {
  idx <- pool$index
  key <- unique(idx[order(idx$layer, idx$feature), c("layer", "feature")])
  geoms <- lapply(seq_len(nrow(key)), function(k) {
    tri <- idx$triangle[idx$layer == key$layer[k] & idx$feature == key$feature[k]]
    group <- rep(NA_integer_, length(pool$mesh))
    group[tri] <- 1L
    geos::as_geos_geometry(wkpool::tri_dissolve(pool$mesh, group))
  })
  list(key = key, geom = do.call(c, geoms))
}

test_that("relate agrees with GEOS across three layers", {
  skip_if_not_installed("geos")
  pool <- polymer(A, B, C)
  r <- relate(pool, cross_layer = FALSE)
  fg <- feature_geos(pool)
  ia <- match(paste(r$layer_a, r$feature_a), paste(fg$key$layer, fg$key$feature))
  ib <- match(paste(r$layer_b, r$feature_b), paste(fg$key$layer, fg$key$feature))
  expect_equal(r$relate, geos::geos_relate(fg$geom[ia], fg$geom[ib]))

  ## a pair that is not listed is disjoint
  all_pairs <- expand.grid(a = seq_len(nrow(fg$key)), b = seq_len(nrow(fg$key)))
  all_pairs <- all_pairs[all_pairs$a < all_pairs$b, ]
  listed <- paste(ia, ib) %in% paste(all_pairs$a, all_pairs$b)
  expect_true(all(listed))
  unlisted <- all_pairs[!paste(all_pairs$a, all_pairs$b) %in% paste(ia, ib), ]
  expect_true(all(geos::geos_disjoint(fg$geom[unlisted$a], fg$geom[unlisted$b])))
})

test_that("cross_layer drops pairs within a layer", {
  pool <- polymer(A, B, C)
  r <- relate(pool)
  expect_true(all(r$layer_a != r$layer_b))
  expect_true(nrow(relate(pool, cross_layer = FALSE)) > nrow(r))
  expect_named(r, c("layer_a", "feature_a", "layer_b", "feature_b", "relate"))
  expect_true(all(nchar(r$relate) == 9L))
})

test_that("relate reports overlap, touching and containment", {
  a <- wkt_poly("POLYGON ((0 0, 2 0, 2 2, 0 2, 0 0))")
  b <- wkt_poly("POLYGON ((1 1, 3 1, 3 3, 1 3, 1 1))")   # overlaps a
  c <- wkt_poly("POLYGON ((2 0, 4 0, 4 1, 2 1, 2 0))")   # shares an edge with a
  d <- wkt_poly("POLYGON ((0.5 0.5, 1 0.5, 1 1, 0.5 1, 0.5 0.5))")  # inside a
  r <- relate(polymer(a, b, c, d))
  get <- function(la, lb) r$relate[r$layer_a == la & r$layer_b == lb]
  expect_equal(substr(get(1, 2), 1, 1), "2")
  expect_equal(substr(get(1, 3), 1, 1), "F")
  expect_equal(substr(get(1, 3), 5, 5), "1")
  expect_equal(get(1, 4), "212FF1FF2")
})

test_that("a single layer has no cross-layer relations", {
  a <- wkt_poly("POLYGON ((0 0, 1 0, 1 1, 0 0))")
  expect_equal(nrow(relate(polymer(a))), 0L)
  expect_error(relate(1), "polymer")
})

test_that("cdtr's winding numbers agree with the labelling", {
  a <- wkt_poly("POLYGON ((0 0, 2 0, 2 2, 0 2, 0 0))")
  b <- wkt_poly("POLYGON ((1 1, 3 1, 3 3, 1 3, 1 1))")
  c <- wkt_poly("POLYGON ((0.5 -0.5, 1.5 -0.5, 1.5 2.5, 0.5 2.5, 0.5 -0.5))")
  pool <- polymer(a, b, c)

  sq <- list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2)),
             rbind(c(1, 1), c(3, 1), c(3, 3), c(1, 3)),
             rbind(c(0.5, -0.5), c(1.5, -0.5), c(1.5, 2.5), c(0.5, 2.5)),
             rbind(c(-1, -1), c(4, -1), c(4, 4), c(-1, 4)))
  feature <- c(1, 2, 3, 0)
  n <- vapply(sq, nrow, 1L)
  first <- cumsum(c(0L, n[-length(n)]))
  xy <- do.call(rbind, sq)
  s0 <- unlist(lapply(seq_along(n), function(k) first[k] + 1:4))
  s1 <- unlist(lapply(seq_along(n), function(k) first[k] + c(2:4, 1L)))
  r <- cdtr::cdt_triangulate(xy[, 1], xy[, 2], s0, s1, edge_sources = TRUE)
  w <- cdtr::cdt_winding(r, owner = rep(feature, n), outside = c(-0.5, -0.5))

  v <- wkpool::pool_vertices(pool$mesh)
  t <- wkpool::pool_triangles(pool$mesh)
  i0 <- match(t$.vx0, v$.vx)
  i1 <- match(t$.vx1, v$.vx)
  i2 <- match(t$.vx2, v$.vx)
  cx <- (v$x[i0] + v$x[i1] + v$x[i2]) / 3
  cy <- (v$y[i0] + v$y[i1] + v$y[i2]) / 3
  tri <- vapply(seq_along(cx), function(k) cdtr::cdt_locate(r$P, r$T, cx[k], cy[k]), 1L)
  expect_false(anyNA(tri))
  for (f in 1:3) {
    covered <- seq_along(cx) %in% pool$index$triangle[pool$index$layer == f]
    expect_equal(covered, w[tri, f] != 0L)
  }
})
