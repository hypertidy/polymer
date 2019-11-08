
sb_intersection <- function(x, ...) {
  plot(x, border = "grey")
  ## if all layers share a triangle we keep them
  index <- x$index %>%
    dplyr::group_by(.data$triangle_idx) %>%
    dplyr::filter(dplyr::n() > 1) %>% dplyr::ungroup()
  #index$layer <- x$geometry_map$layer[match(index$path_, x$geometry_map$path)]
  triangles <- x$primitives$T[index$triangle_idx, ]
  polypath(head(x$primitives$P[t(cbind(triangles, NA)), ], -1L), ...)
}


#' N intersections
#'
#' Find all fragments that are intersected by any other feature in any layer.
#'
#' Returns a simple features data frame with all triangles that occur `n` times with
#' `n = 2` as a minimum. Each triangle feature contains a nested data frame in `idx`
#' that keeps the links to the input layers by `layer`, `object` and `path`.
#' @param x polymer
#' @param n minimum number of intersections to keep
#' @param ... ignored for now
#'
#' @return sf data frame
#' @export
#'
#' @examples
#' plot(A["layer"], reset = TRUE)
#' plot(B, add = TRUE, col = "hotpink")
#' plot(C, add = TRUE, col = "firebrick")
#'
#' sb <- polymer(A, B, C)
#' plot(n_intersections(sb), add = TRUE, col = "grey")
#' plot(n_intersections(sb, n = 3), add = TRUE, col = "dodgerblue")
n_intersections <- function(x, n = 2, ...) {
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
  sf::st_sf(idx = idx, geometry = sf::st_sfc(purrr::map(idx$triangle_idx, ~sf::st_polygon(list(P[TR[.x, ][c(1, 2, 3, 1)], ])))))
}
