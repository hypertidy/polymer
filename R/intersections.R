



#' N intersections
#'
#' Find all fragments that are intersected by any other feature in any layer.
#'
#' Returns a simple features data frame with all triangles that occur `n` times with
#' `n = 2` as a minimum. Each triangle feature contains a nested data frame in `idx`
#' that keeps the links to the input layers by `layer`, `object` and `path`.
#'
#' @param x polymer
#' @param n minimum number of intersections to keep
#' @param ... ignored for now
#' @param keep_index for expert use only, maintains the list of triangle indexes
#' on the sf output (and sf cannot plot if that is present)
#'
#' @return sf data frame
#' @export
#'
#' @examples
#' library(sf)
#' plot(A["layer"], reset = TRUE)
#' plot(B, add = TRUE, col = "hotpink")
#' plot(C, add = TRUE, col = "firebrick")
#'
#' sb <- polymer(A, B, C)
#' plot(layer_n(sb), add = TRUE, col = "grey")
#' plot(layer_n(sb, n = 3), add = TRUE, col = "dodgerblue")
layer_n <- function(x, n = 2, ..., keep_index = FALSE) {
  triangles <- x$index %>%
    dplyr::group_by(.data$triangle_idx) %>%
    dplyr::mutate(nn = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$nn >= n) %>%
    dplyr::transmute(path = .data$path_, .data$triangle_idx)
  gmap <- x$geometry_map %>%
    dplyr::select(.data$object_, .data$layer, .data$path)
  ## every unique triangle keeps a record of which path, object, layer
          ## (a bit of redundancy until we get a single path/object index or ...)
  idx <- purrr::map_df(split(triangles, triangles$triangle_idx),
             function(piece) {
               ## path joins us to layer + object
               piece %>% dplyr::inner_join(gmap, "path")
             }) %>% dplyr::group_by(.data$triangle_idx) %>% tidyr::nest()

  ## now build each triangle
  P <- x$primitives$P
  TR <- x$primitives$T
  geom <- sf::st_sfc(purrr::map(idx$triangle_idx, ~sf::st_polygon(list(P[TR[.x, ][c(1, 2, 3, 1)], ]))))
  if (keep_index) {
    out <- sf::st_sf(id = seq_along(geom),
            idx = idx,
            geometry = geom)
  } else {
    out <- sf::st_sf(id = seq_along(geom),
                     geometry = geom)
  }
  out
}
