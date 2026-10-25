#' Fuse a polymer into overlay pieces
#'
#' Dissolve the triangles of a polymer into polygons, one for each distinct
#' set of covering input features. This is the n-ary intersection of all
#' layers (the same partition that `sf::st_intersection()` computes for a
#' single layer), derived from the mesh rather than from pairwise overlay.
#'
#' `layer_n()` keeps the pieces covered by at least `n` features.
#'
#' Pieces are dissolved along mesh edges, so they share vertices exactly
#' where they touch. Outer rings are counter-clockwise and holes clockwise.
#' Every piece is a MULTIPOLYGON, with one polygon for each connected part.
#'
#' @param x A polymer (see [polymer()]).
#' @param n Minimum number of covering features, for `layer_n()`.
#' @param ... Ignored.
#'
#' @return A data frame with one row per piece and columns
#' * `n`: the number of input features covering the piece.
#' * `origins`: a list column, each element a data frame of `layer` and
#'   `feature` (row within its layer) for the features covering the piece.
#' * `geometry`: a `wk::wkb()` vector with the polymer's coordinate reference
#'   system. Convert with `sf::st_as_sf()` if you work with 'sf'.
#' @export
#' @examples
#' pool <- polymer(A, B, C)
#' pieces <- fuse(pool)
#' pieces[c("n", "geometry")]
#'
#' ## which features made the deepest piece
#' pieces$origins[[which.max(pieces$n)]]
#'
#' ## pieces covered two or more times
#' plot(pool)
#' wk::wk_plot(layer_n(pool, 2)$geometry, add = TRUE, col = "grey")
#' wk::wk_plot(layer_n(pool, 3)$geometry, add = TRUE, col = "dodgerblue")
fuse <- function(x, ...) {
  check_polymer(x)
  idx <- x$index
  nt <- length(x$mesh)

  ## one group per distinct set of covering features
  gid <- idx$layer * (max(idx$feature) + 1) + idx$feature
  set_key <- vapply(split(gid, factor(idx$triangle, levels = seq_len(nt))),
                    function(g) paste(g, collapse = " "), character(1))
  group <- match(set_key, unique(set_key))

  geometry <- dissolve_triangles(x$mesh, group, x$crs)

  first <- match(seq_len(max(group)), group)
  rows <- split(idx[c("layer", "feature")], factor(idx$triangle, levels = seq_len(nt)))
  origins <- lapply(rows[first], function(r) {
    rownames(r) <- NULL
    r
  })
  out <- data.frame(n = unname(vapply(origins, nrow, integer(1))))
  out$origins <- unname(origins)
  out$geometry <- geometry
  out
}

#' @rdname fuse
#' @export
#' @examples
#'
#' layer_n(pool, 3)
layer_n <- function(x, n = 2, ...) {
  out <- fuse(x)
  out <- out[out$n >= n, , drop = FALSE]
  rownames(out) <- NULL
  out
}

check_polymer <- function(x) {
  if (!inherits(x, "polymer")) stop("`x` must be a polymer (see polymer())")
  invisible(x)
}

## Dissolve triangles into one MULTIPOLYGON per group.
##
## Triangles are counter-clockwise, so the boundary of a group, walked along
## triangle edges whose twin is not in the group, has the group on its left:
## outer rings come out counter-clockwise and holes clockwise. Rings are
## traced within edge-connected components of a group. A walk that passes
## through a vertex twice (a component touching itself there, such as a hole
## that meets its outer ring at a point) is split at that vertex, so every
## ring is simple and rings meet only at points.
dissolve_triangles <- function(mesh, group, crs = NULL) {
  v <- wkpool::pool_vertices(mesh)
  t <- wkpool::pool_triangles(mesh)
  nv <- nrow(v)
  nt <- nrow(t)
  t0 <- match(t$.vx0, v$.vx)
  t1 <- match(t$.vx1, v$.vx)
  t2 <- match(t$.vx2, v$.vx)

  ## directed edges, three per triangle
  e0 <- c(t0, t1, t2)
  e1 <- c(t1, t2, t0)
  et <- rep(seq_len(nt), 3L)
  twin <- match((e1 - 1) * nv + e0, (e0 - 1) * nv + e1)
  inner <- !is.na(twin) & group[et] == group[et[twin]]

  comp <- edge_components(et[inner], et[twin[inner]], nt)

  ## boundary edges and the next edge round the same component
  b <- which(!inner)
  bu <- e0[b]
  bv <- e1[b]
  bc <- comp[et[b]]
  from_key <- (bc - 1) * nv + bu
  to_key <- (bc - 1) * nv + bv
  nxt <- match(to_key, from_key)
  pinch <- which(to_key %in% from_key[duplicated(from_key)])
  for (i in pinch) {
    cand <- which(from_key == to_key[i])
    vv <- bv[i]
    a_in <- atan2(v$y[bu[i]] - v$y[vv], v$x[bu[i]] - v$x[vv])
    a_out <- atan2(v$y[bv[cand]] - v$y[vv], v$x[bv[cand]] - v$x[vv])
    turn <- (a_in - a_out) %% (2 * pi)
    nxt[i] <- cand[which.min(turn)]
  }

  ## walk the cycles
  nb <- length(b)
  ring <- integer(nb)
  pos <- integer(nb)
  r <- 0L
  for (s in seq_len(nb)) {
    if (ring[s] > 0L) next
    r <- r + 1L
    k <- 0L
    e <- s
    while (ring[e] == 0L) {
      k <- k + 1L
      ring[e] <- r
      pos[e] <- k
      e <- nxt[e]
    }
  }
  o <- order(ring, pos)
  ring <- split_rings(ring[o], bu[o])
  o <- o[order(ring, seq_along(ring))]
  ring <- sort(ring)
  vx <- bu[o]
  owner <- et[b][o]

  ## signed area per ring: positive is an outer ring
  xr <- v$x[vx]
  yr <- v$y[vx]
  nxt_in_ring <- unlist(lapply(split(seq_along(ring), ring),
                               function(i) c(i[-1L], i[1L])), use.names = FALSE)
  ## translate each ring to its first vertex before the shoelace sum: with
  ## absolute coordinates the rounding error (about |x| |y| 1e-16) can exceed
  ## the area of a small ring and give it the wrong sign or exactly zero
  ring_first <- match(ring, ring)
  xs <- xr - xr[ring_first]
  ys <- yr - yr[ring_first]
  area <- rowsum((xs * ys[nxt_in_ring] - xs[nxt_in_ring] * ys) / 2, ring)[, 1L]
  first <- !duplicated(ring)
  ring_comp <- comp[owner[first]]
  ring_group <- group[owner[first]]
  is_outer <- area > 0

  ## each ring belongs to a polygon (an outer ring and its holes)
  poly <- assign_holes(seq_along(area), ring_comp, is_outer, area,
                       owner[first], ring, xr, yr, t0, t1, t2, v)

  ## rings in output order: by group, then polygon, outer ring first
  poly_group <- ring_group[poly]
  ring_rank <- match(seq_along(area), order(poly_group, poly, !is_outer))
  row_order <- order(ring_rank[ring], seq_along(ring))
  row_poly <- poly[ring[row_order]]
  polys <- wk::wk_polygon(wk::xy(xr[row_order], yr[row_order]),
                          feature_id = match(row_poly, unique(row_poly)),
                          ring_id = ring[row_order])
  pg <- ring_group[unique(row_poly)]
  out <- wk::wk_collection(polys, wk::wk_geometry_type("multipolygon"),
                           feature_id = pg)
  ## one geometry per group, in group order
  out <- wk::as_wkb(out)[match(seq_len(max(group)), unique(pg))]
  wk::wk_set_crs(out, crs)
}

## Split rings that visit a vertex more than once. `ring` and `vx` are in
## walk order; the closed sub-walk between the two visits becomes its own
## ring and the rest of the ring keeps one visit. Returns new ring ids.
split_rings <- function(ring, vx) {
  key <- (as.double(ring) - 1) * (max(vx) + 1) + vx
  if (!anyDuplicated(key)) return(ring)
  next_id <- max(ring)
  for (r in unique(ring[duplicated(key)])) {
    idx <- which(ring == r)
    repeat {
      j <- which(duplicated(vx[idx]))[1L]
      if (is.na(j)) break
      i <- match(vx[idx][j], vx[idx])
      next_id <- next_id + 1L
      ring[idx[i:(j - 1L)]] <- next_id
      idx <- idx[-(i:(j - 1L))]
    }
  }
  ring
}

## Connected components of triangles joined by inner edges (label
## propagation with pointer jumping); returns a component id per triangle.
edge_components <- function(a, b, n) {
  lab <- seq_len(n)
  repeat {
    m <- pmin(lab[a], lab[b])
    o <- order(m, decreasing = TRUE)
    new <- lab
    new[a[o]] <- m[o]
    new <- pmin(new, lab)
    new <- new[new]
    if (identical(new, lab)) break
    lab <- new
  }
  match(lab, unique(lab))
}

## Polygon id for each ring. A component with one outer ring takes all its
## holes. Otherwise each hole goes to the smallest outer ring of its
## component that contains a point of the triangle on its inner side.
assign_holes <- function(id, comp, is_outer, area, owner, ring, xr, yr,
                         t0, t1, t2, v) {
  poly <- integer(length(id))
  poly[is_outer] <- id[is_outer]
  for (h in id[!is_outer]) {
    cand <- id[is_outer & comp == comp[h]]
    if (length(cand) == 1L) {
      poly[h] <- cand
      next
    }
    tr <- owner[h]
    px <- (v$x[t0[tr]] + v$x[t1[tr]] + v$x[t2[tr]]) / 3
    py <- (v$y[t0[tr]] + v$y[t1[tr]] + v$y[t2[tr]]) / 3
    inside <- vapply(cand, function(o) {
      i <- ring == o
      polyclip::pointinpolygon(list(x = px, y = py), list(x = xr[i], y = yr[i])) != 0L
    }, logical(1))
    cand <- if (any(inside)) cand[inside] else cand
    poly[h] <- cand[which.min(abs(area[cand]))]
  }
  poly
}
