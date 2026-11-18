#' Relations between the features of a polymer
#'
#' Give the DE-9IM relation of every pair of input features that meet, across
#' all layers at once. The relation is read from the mesh by
#' [wkpool::tri_relate()], from the ids of the vertices and triangles that
#' the features share, so no polygon is built and no coordinates are
#' compared. Because every layer is in the one triangulation, features of
#' three or more layers are related in the same pass, which pairwise
#' tools (such as GEOS, two geometries at a time) cannot do on shared
#' vertices.
#'
#' The relation is a nine character string, as from `ST_Relate()`: how the
#' interior, boundary and exterior of feature `a` meet those of feature `b`
#' (`"F"` for not at all, or the dimension of the intersection, `"0"`,
#' `"1"` or `"2"`). Named predicates are patterns on that string. For
#' example, two features overlap in area when the first character is `"2"`,
#' and touch when it is `"F"` and they are listed at all.
#'
#' Pairs that are not listed are disjoint. Features that are covered by no
#' triangle (empty features) are not listed. The answers are exact for the
#' mesh, which is the answer for the input polygons when each crossing is
#' computed exactly; crossings are computed in double precision (see
#' [polymer()]).
#'
#' @param x A polymer (see [polymer()]).
#' @param cross_layer If `TRUE` (the default), only pairs of features from
#'   different layers are returned. Use `FALSE` to include the relations
#'   between features of the same layer, such as which neighbours touch.
#'
#' @return A data frame with one row for each pair of features that meet and
#'   columns `layer_a`, `feature_a`, `layer_b`, `feature_b` (the row within
#'   its layer) and `relate`, the DE-9IM string. Feature `a` comes from the
#'   earlier layer, or the earlier row within a layer.
#' @export
#' @examples
#' pool <- polymer(A, B, C)
#' r <- relate(pool)
#' r
#'
#' ## features of different layers whose interiors overlap in area
#' r[substr(r$relate, 1, 1) == "2", ]
#'
#' ## neighbours within a layer too
#' relate(pool, cross_layer = FALSE)
relate <- function(x, cross_layer = TRUE) {
  check_polymer(x)
  idx <- x$index
  empty <- data.frame(layer_a = integer(), feature_a = integer(),
                      layer_b = integer(), feature_b = integer(),
                      relate = character())
  if (nrow(idx) < 1L) return(empty)

  ## one group for each (layer, feature) present in the mesh, in layer order
  key <- unique(idx[order(idx$layer, idx$feature), c("layer", "feature")])
  gid <- match(paste(idx$layer, idx$feature), paste(key$layer, key$feature))
  groups <- data.frame(triangle = idx$triangle, group = gid)

  r <- wkpool::tri_relate(x$mesh, groups)
  if (nrow(r) < 1L) return(empty)
  out <- data.frame(layer_a = key$layer[r$a], feature_a = key$feature[r$a],
                    layer_b = key$layer[r$b], feature_b = key$feature[r$b],
                    relate = r$relate, stringsAsFactors = FALSE)
  if (cross_layer) out <- out[out$layer_a != out$layer_b, , drop = FALSE]
  out <- out[order(out$layer_a, out$feature_a, out$layer_b, out$feature_b), ,
             drop = FALSE]
  rownames(out) <- NULL
  out
}
