#' Space bucket
#'
#' Convert a collection of sf data frame polygon layers to a single pool
#' of triangles.
#'
#' Each triangle is identified by which path in the inputs it belongs to. None of
#' this is very useable yet. Holes can be identified but aren't at the moment, any
#' path that is a hole is identified per triangle.
#'
#' `input` is a list with all input objects
#' `primitives` is the triangulation object
#' `geometry_map` is the paths with their row count
#' `index` is the mapping between triangle and path/s
#' @param ... sf polygon data frame inputs
#'
#' @return a spacebucket, see details
#' @export
#' @importFrom rlang .data
#' @examples
#' spacebucket(A, B, C)
spacebucket <- function(...) {
  ## combine each layer
  inputs <- list(...)
  inputs0 <- lapply(seq_along(inputs),
                    function(x) sf::st_sf(layer = rep(x, length(inputs[[x]][[1]])), geometry = sf::st_geometry(inputs[[x]])))
  #  mesh_pool <- silicate::SC(do.call(rbind, inputs0))

  ## TODO1
  ## triangulate the mesh
  sfall <- do.call(rbind, inputs0)
  path <- silicate::PATH(sfall)
  RTri <- pfft::edge_RTriangle(path)

  ## TODO2
  ## identify all points by overlap with inputs
  map <- pfft::path_triangle_map(path, RTri)

  ## TODO3
  ## sort out common CRS for inputs

  index <-   map %>% dplyr::mutate(path_ = match(.data$path_, path$path$path_))
  paths <- path[["path"]] %>%
    dplyr::transmute(.data$subobject,
                     .data$object,
                     .data$ncoords_,
                     path = dplyr::row_number())

  layers <- unlist(lapply(seq_along(inputs), function(a) rep(a, nrow(inputs[[a]]))))
  paths$layer <- layers[paths$object]
  out <- list(input = inputs0,
              primitives = RTri,
              geometry_map = paths,
              index = index)
  class(out) <- "spacebucket"
  out
}

#' Print the primitive space bucket
#'
#' Print a short description of the bucket contents.
#' @param x spacebucket
#' @param ... ignored
#'
#' @return x invisibly
#' @export
#'
#' @examples
#' spacebucket(A, B, C)
print.spacebucket <- function(x, ...) {
  cat("SPACE BUCKET:\n")
  cat(sprintf("Layers:    %i\n", length(x$input)))
  cat(sprintf("Polygons:  %i\n", sum(unlist(lapply(x$input, nrow)))))
  cat(sprintf("Triangles: %i\n", length(unique(x$index$triangle_idx))))
  cat(sprintf("(Overlaps: %i)\n", sum(table(x$index$triangle_idx) > 1)))
  invisible(x)
}

#' Plot the primitive space bucket
#'
#' @param x spacebucket
#' @param ... arguments to [polypath]
#'
#' @return nothing
#' @export
#' @importFrom graphics plot polypath
#' @importFrom utils head
#' @examples
#' plot(spacebucket(A, B, C))
#' library(sf)
#' example(st_read)
#' x <- nc[1:5, ]
#' bucket <- spacebucket(nc, st_jitter(nc, amount = 0.1))
#' plot(bucket)
plot.spacebucket <- function(x, ...) {
  plot(x$primitives$P, pch = ".", asp = 1)
  polypath(head(x$primitives$P[t(cbind(x$primitives$T, x$primitives$T[,1], NA)), ], -1), ...)
  invisible(NULL)
}
## needs to be in silicate
get_projection.sfc <- function(x, ...) attr(x, "crs")[["proj4string"]]
get_projection.sf <- function(x, ...) attr(sf::st_geometry(x), "crs")[["proj4string"]]


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
