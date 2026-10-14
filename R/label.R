## Label triangles by the features containing their centroids.
##
## `coords` is wk::wk_coords() of all inputs, so feature_id is the global
## feature number. Each ring is tested on its own (bounding box first, then
## point in polygon) and a triangle is inside a feature when it is inside an
## odd number of that feature's rings: holes cancel their outer ring.
## Returns a data frame of (triangle, feature) pairs.
label_triangles <- function(cx, cy, coords) {
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
  }
  tri <- unlist(tri, use.names = FALSE)
  feat <- unlist(feat, use.names = FALSE)
  if (length(tri) < 1L) return(data.frame(triangle = integer(), feature = integer()))

  ## keep (triangle, feature) pairs hit an odd number of times
  pair <- as.double(tri) * (max(feat) + 1) + feat
  u <- unique(pair)
  grp <- match(pair, u)
  odd <- tabulate(grp, nbins = length(u)) %% 2L == 1L
  first <- !duplicated(grp)
  data.frame(triangle = tri[first][odd], feature = feat[first][odd])
}
