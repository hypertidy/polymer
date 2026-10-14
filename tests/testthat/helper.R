wkt_poly <- function(...) wk::wkt(c(...))

## shoelace area of each feature: outer rings count positive and holes
## negative, whatever their winding
piece_area <- function(geometry) {
  co <- wk::wk_coords(geometry)
  n <- nrow(co)
  new_ring <- c(TRUE, co$feature_id[-1L] != co$feature_id[-n] |
                  co$part_id[-1L] != co$part_id[-n] |
                  co$ring_id[-1L] != co$ring_id[-n])
  ring <- cumsum(new_ring)
  a <- vapply(split(co[c("x", "y")], ring), function(r) {
    j <- c(seq_len(nrow(r))[-1L], 1L)
    sum(r$x * r$y[j] - r$x[j] * r$y) / 2
  }, numeric(1))
  first <- which(new_ring)
  ## the first ring of each part is its outer ring
  part_key <- paste(co$feature_id[first], co$part_id[first])
  is_outer <- !duplicated(part_key)
  a <- ifelse(is_outer, abs(a), -abs(a))
  as.vector(tapply(a, co$feature_id[first], sum))
}

## the overlap-weighted area of the pieces equals the summed input area
expect_partition <- function(x, inputs_area, tolerance = 1e-12) {
  f <- fuse(x)
  a <- piece_area(f$geometry)
  expect_equal(sum(f$n * a), inputs_area, tolerance = tolerance)
  invisible(f)
}

## the polymer issue 7 stress input: random segments buffered by 1 with
## flat caps, built directly as quadrilaterals
buffered_lines <- function(seed, n = 200) {
  set.seed(seed)
  x <- stats::runif(n, -50, 50)
  y <- stats::runif(n, 0, 100)
  i <- seq(1, n, by = 2)
  dx <- x[i + 1] - x[i]
  dy <- y[i + 1] - y[i]
  len <- sqrt(dx^2 + dy^2)
  nx <- -dy / len
  ny <- dx / len
  wk::wk_polygon(
    wk::xy(as.vector(rbind(x[i] + nx, x[i + 1] + nx, x[i + 1] - nx, x[i] - nx)),
           as.vector(rbind(y[i] + ny, y[i + 1] + ny, y[i + 1] - ny, y[i] - ny))),
    feature_id = rep(seq_along(i), each = 4L)
  )
}
