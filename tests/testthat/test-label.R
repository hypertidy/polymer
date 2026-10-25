## the (triangle, layer, feature) rows of a polymer as keys
index_keys <- function(x) sort(paste(x$index$triangle, x$index$layer, x$index$feature))

test_that("mesh labelling agrees with point in polygon under every rule", {
  g <- buffered_lines(1)
  for (rule in c("nonzero", "evenodd", "positive")) {
    m <- polymer(g, rule = rule)
    p <- polymer(g, rule = rule, label = "pip")
    expect_equal(length(m$mesh), length(p$mesh))
    expect_identical(index_keys(m), index_keys(p))
  }
  m <- polymer(A, B, C)
  expect_identical(index_keys(m), index_keys(polymer(A, B, C, label = "pip")))
})

test_that("the fill rule decides duplicated parts, and ring order sets roles", {
  dup <- wkt_poly("MULTIPOLYGON (((0 0, 1 0, 1 1, 0 1, 0 0)), ((0 0, 1 0, 1 1, 0 1, 0 0)))")
  expect_equal(nrow(polymer(dup)$index), 2L)
  expect_equal(nrow(polymer(dup, rule = "positive")$index), 2L)
  expect_equal(nrow(polymer(dup, rule = "evenodd")$index), 0L)
  ## a hole wound the same way as its shell is still a hole
  donut <- wkt_poly("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0), (3 3, 7 3, 7 7, 3 7, 3 3))")
  for (rule in c("nonzero", "evenodd", "positive")) {
    expect_equal(sum(piece_area(fuse(polymer(donut, rule = rule))$geometry)), 84)
  }
  ## a clockwise shell is a shell
  cw <- wkt_poly("POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))")
  expect_equal(sum(piece_area(fuse(polymer(cw, rule = "positive"))$geometry)), 1)
  expect_error(polymer(donut, rule = "odd"), "should be one of")
})

test_that("empty features and multipolygons keep their rings", {
  g <- wk::wkt(c("POLYGON EMPTY",
                 "MULTIPOLYGON (((0 0, 2 0, 2 2, 0 2, 0 0), (0.5 0.5, 1.5 0.5, 1.5 1.5, 0.5 1.5, 0.5 0.5)), ((5 5, 6 5, 6 6, 5 5)))",
                 "POLYGON ((1 1, 3 1, 3 3, 1 3, 1 1))"))
  x <- polymer(g)
  expect_identical(index_keys(x), index_keys(polymer(g, label = "pip")))
  expect_setequal(x$index$feature, 2:3)
  expect_partition(x, 3 + 0.5 + 4)
})

## two overlapping unit squares (features 0 and 1, counter-clockwise) and
## the arguments overlay_label_cpp() takes, from cdtr
square_pair <- function() {
  x <- c(0, 2, 2, 0, 1, 3, 3, 1)
  y <- c(0, 0, 2, 2, 1, 1, 3, 3)
  s0 <- c(1:4, 5:8)
  s1 <- c(2:4, 1, 6:8, 5)
  r <- cdtr::cdt_triangulate(x, y, s0, s1, edge_sources = TRUE)
  es <- r$edge_sources
  list(tri = as.vector(t(r$T)) - 1L, edge = as.vector(t(r$S)) - 1L,
       es_edge = es$edge - 1L, es_feature = rep(0:1, each = 4)[es$segment],
       es_forward = es$forward, rule = "nonzero")
}

test_that("the header labels sets densely and checks itself", {
  a <- square_pair()
  lab <- do.call(overlay_label_cpp, a)
  expect_equal(lab$inconsistent_edges, 0)
  sets <- lapply(seq_len(length(lab$set_offset) - 1L), function(k) {
    lab$set_feature[seq(lab$set_offset[k] + 1, length.out = lab$set_offset[k + 1] - lab$set_offset[k])]
  })
  expect_setequal(sets, list(0L, 1L, 0:1))
  expect_true(all(lab$tri_set >= 0L))
  expect_equal(sort(unique(lab$tri_set)), 0:2)
  ## numbered by first appearance
  expect_equal(unique(lab$tri_set), seq_along(unique(lab$tri_set)) - 1L)

  ## a missing source row is broken noding, and the self-check sees it
  b <- a
  drop <- -1L
  b$es_edge <- b$es_edge[drop]
  b$es_feature <- b$es_feature[drop]
  b$es_forward <- b$es_forward[drop]
  expect_gt(do.call(overlay_label_cpp, b)$inconsistent_edges, 0)

  ## triangles must be consistently counter-clockwise
  c <- a
  c$tri[1:3] <- c$tri[c(1, 3, 2)]
  expect_error(do.call(overlay_label_cpp, c), "consistently CCW")
  a$rule <- "odd"
  expect_error(do.call(overlay_label_cpp, a), "unknown fill rule")
})
