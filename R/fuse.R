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

  ## one group per distinct set of covering features: the labelling's set
  ## ids, or (for label = "pip") sets built from the index
  group <- x$set_id
  if (is.null(group)) {
    gid <- idx$layer * (max(idx$feature) + 1) + idx$feature
    set_key <- vapply(split(gid, factor(idx$triangle, levels = seq_len(nt))),
                      function(g) paste(g, collapse = " "), character(1))
    group <- match(set_key, unique(set_key))
  }

  geometry <- wk::wk_set_crs(wkpool::tri_dissolve(x$mesh, group), x$crs)

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
