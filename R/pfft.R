
#' All extents
#'
#' (This function probably belongs in spex). Find the extent of all paths within an object.
#'
#' The `path_` identifier is included, but won't be of use without an
#' existing `PATH` object. The path order is implicit as per the gibble
#' geometry map.
#' @param x Object with paths
#'
#' @return a dataframe of object and extent values (xmin, xmax, ymin, ymax)
#' @noRd
#'
#' @examples
#' data("minimal_mesh", package = "silicate")
#' extents(minimal_mesh)
extents <- function(x) {
  UseMethod("extents")
}
#' @noRd
extents.default <- function(x) extents(silicate::PATH(x))





#' @noRd
#' @importFrom rlang .data
#' @importFrom dplyr %>%
extents.PATH <- function(x) {
  x[["path"]] %>% dplyr::select(.data$path_) %>%
    dplyr::inner_join(x[["path_link_vertex"]], "path_") %>%
    dplyr::inner_join(x[["vertex"]], "vertex_") %>%
    dplyr::group_by(.data$path_) %>%
    dplyr::summarize(xmn = min(.data$x_), xmx = max(.data$x_), ymn = min(.data$y_), ymx = max(.data$y_))
}


pfft_edge_RTriangle <- function (x, ...)
{
  ps <- RTriangle::pslg(P = as.matrix(x[["vertex"]][c("x_",
                                                      "y_")]), S = matrix(match(silicate::sc_edge(x) %>% dplyr::select(.data$.vx0,
                                                                                                                       .data$.vx1) %>% as.matrix() %>% t() %>% as.vector(),
                                                                                x[["vertex"]][["vertex_"]]), ncol = 2, byrow = TRUE))
  RTriangle::triangulate(ps, ...)
}

pfft_path_triangle_map <- function (x, RTri)
{
  centroids <- matrix(unlist(lapply(split(RTri[["P"]][t(RTri[["T"]]),
  ], rep(seq(nrow(RTri$T)), each = 3)), .colMeans, 3, 2)),
  ncol = 2, byrow = TRUE)
  ex <- extents(x)
  gm <- x[["path"]]
  pipmap <- split(ex, ex$path_)[unique(ex$path_)] %>% purrr::map(~(centroids[,
                                                                             1] >= .x[["xmn"]] & centroids[, 1] <= .x[["xmx"]] & centroids[,
                                                                                                                                           2] >= .x[["ymn"]] & centroids[, 2] <= .x[["ymx"]]))
  pipmap <- pipmap[gm$path_]
  len <- purrr::map_int(pipmap, sum)
  lc <- split(silicate::sc_coord(x), rep(seq_len(nrow(gm)),
                                         gm$ncoords_))
  pip <- pipmap
  for (i in seq_along(pipmap)) {
    if (len[i] > 0) {
      pip[[i]][pipmap[[i]]] <- abs(polyclip::pointinpolygon(list(x = centroids[pipmap[[i]],
                                                                               1], y = centroids[pipmap[[i]], 2]), list(x = lc[[i]][["x_"]],
                                                                                                                        y = lc[[i]][["y_"]]))) > 0L
    }
    else {
      pip[[i]][] <- FALSE
    }
  }
  ix <- lapply(pip, which)
  tibble::tibble(path_ = rep(names(ix), lengths(ix)), triangle_idx = unlist(ix))
}
