## One row per input ring, in input order: its feature, whether it is a
## shell (the first ring of its part) and `sign`, -1 when the ring must be
## reversed so that shells are counter-clockwise and holes clockwise.
## Signed areas are taken relative to each ring's first vertex.
ring_table <- function(coords) {
  n <- nrow(coords)
  if (n < 1L) return(data.frame(feature = integer(), shell = logical(), sign = numeric()))
  new_ring <- c(TRUE, coords$feature_id[-1L] != coords$feature_id[-n] |
                  coords$part_id[-1L] != coords$part_id[-n] |
                  coords$ring_id[-1L] != coords$ring_id[-n])
  ring <- cumsum(new_ring)
  first <- which(new_ring)
  x <- coords$x - coords$x[first][ring]
  y <- coords$y - coords$y[first][ring]
  j <- c(seq_len(n)[-1L], 1L)
  ## wrap each ring's last vertex to its own first vertex
  last <- c(first[-1L] - 1L, n)
  j[last] <- first
  area <- as.vector(tapply(x * y[j] - x[j] * y, ring, sum)) / 2
  part_key <- paste(coords$feature_id[first], coords$part_id[first])
  shell <- !duplicated(part_key)
  want <- ifelse(shell, 1, -1)
  data.frame(feature = coords$feature_id[first], shell = shell,
             sign = ifelse(area * want < 0, -1, 1))
}

## Label triangles by propagating winding numbers over the mesh (the
## header inst/include/polymer/overlay.hpp). `tri` is cdt_triangulate()
## output with edge_sources over the distinct segments; `s_feature` is the
## feature of each input segment, `s_unique` the distinct segment it lies
## on and `s_same` whether it runs the same way. Each edge_sources row is
## expanded to every input segment on its distinct segment.
## Returns the (triangle, feature) pairs, the 1-based set id of every
## triangle (NA when uncovered) and the self-check count.
label_mesh <- function(tri, s_feature, s_unique, s_same, rule) {
  es <- tri$edge_sources
  ord <- order(s_unique)
  n_on <- tabulate(s_unique, nbins = max(s_unique))
  start <- cumsum(c(0L, n_on))[es$segment]
  k <- n_on[es$segment]
  orig <- ord[rep(start, k) + sequence(k)]
  row <- rep(seq_len(nrow(es)), k)
  lab <- overlay_label_cpp(as.vector(t(tri$T)) - 1L, as.vector(t(tri$S)) - 1L,
                           es$edge[row] - 1L, s_feature[orig] - 1L,
                           es$forward[row] == s_same[orig], rule)
  covered <- which(lab$tri_set >= 0L)
  set <- lab$tri_set[covered] + 1L
  len <- diff(lab$set_offset)
  n <- len[set]
  first <- lab$set_offset[set]
  pos <- rep(first, n) + sequence(n)
  list(hits = data.frame(triangle = rep(covered, n),
                         feature = lab$set_feature[pos] + 1L),
       set_id = ifelse(lab$tri_set >= 0L, lab$tri_set + 1L, NA_integer_),
       inconsistent_edges = lab$inconsistent_edges)
}

## Label triangles by the features containing their centroids.
##
## `coords` is wk::wk_coords() of all inputs, so feature_id is the global
## feature number, and `rings` its ring_table(). Each ring is tested on its
## own (bounding box first, then point in polygon); a hit counts +1 for a
## shell and -1 for a hole, which is the winding number once rings are
## oriented, and the fill rule decides what is covered.
## Returns a data frame of (triangle, feature) pairs.
label_triangles <- function(cx, cy, coords, rings, rule = "nonzero") {
  n <- nrow(coords)
  new_ring <- c(TRUE, coords$feature_id[-1L] != coords$feature_id[-n] |
                  coords$part_id[-1L] != coords$part_id[-n] |
                  coords$ring_id[-1L] != coords$ring_id[-n])
  ring <- cumsum(new_ring)
  rows <- split(seq_len(n), ring)

  ## centroids sorted on x, so each ring's x range is one slice
  ord <- order(cx)
  sx <- cx[ord]

  tri <- vector("list", length(rows))
  feat <- vector("list", length(rows))
  wt <- vector("list", length(rows))
  for (i in seq_along(rows)) {
    r <- rows[[i]]
    rx <- coords$x[r]
    ry <- coords$y[r]
    k <- length(rx)
    if (k > 1L && rx[1L] == rx[k] && ry[1L] == ry[k]) {
      rx <- rx[-k]
      ry <- ry[-k]
    }
    if (length(rx) < 3L) next
    i1 <- findInterval(min(rx), sx, left.open = TRUE) + 1L
    i2 <- findInterval(max(rx), sx)
    if (i2 < i1) next
    cand <- ord[i1:i2]
    cand <- cand[cy[cand] >= min(ry) & cy[cand] <= max(ry)]
    if (length(cand) < 1L) next
    pip <- polyclip::pointinpolygon(list(x = cx[cand], y = cy[cand]),
                                    list(x = rx, y = ry))
    hit <- cand[pip != 0L]
    tri[[i]] <- hit
    feat[[i]] <- rep(coords$feature_id[r[1L]], length(hit))
    wt[[i]] <- rep(if (rings$shell[i]) 1L else -1L, length(hit))
  }
  tri <- unlist(tri, use.names = FALSE)
  feat <- unlist(feat, use.names = FALSE)
  wt <- unlist(wt, use.names = FALSE)
  if (length(tri) < 1L) return(data.frame(triangle = integer(), feature = integer()))

  ## winding of each (triangle, feature) pair, then the fill rule
  pair <- as.double(tri) * (max(feat) + 1) + feat
  u <- unique(pair)
  grp <- match(pair, u)
  w <- as.vector(tapply(wt, factor(grp, levels = seq_along(u)), sum))
  cov <- switch(rule, nonzero = w != 0L, evenodd = w %% 2L == 1L, positive = w > 0L)
  first <- !duplicated(grp)
  data.frame(triangle = tri[first][cov], feature = feat[first][cov])
}
