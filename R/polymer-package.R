#' @keywords internal
"_PACKAGE"

#' Field and soil polygons
#'
#' Two polygon layers used to show overlay: `soil` has 12 polygons with a
#' made-up soil type, `field` has two rectangular fields that cross them.
#' The example comes from a question on the GIS Stack Exchange
#' (<https://gis.stackexchange.com/a/230161>), using the Luxembourg
#' boundaries shipped with the 'raster' package.
#'
#' @format 'sf' data frames in longitude and latitude (WGS84):
#' * `soil`: 12 rows, `soil` (character) and `geometry`.
#' * `field`: 2 rows, `field` (character) and `geoms`.
#' @source The `soil` boundaries are the Luxembourg file `lux.shp` from the
#'   'raster' package, which derives from GADM (<https://gadm.org>). GADM data
#'   carry their own licence terms, separate from polymer's MIT licence; these
#'   boundaries may be replaced by a rebuilt layer in a later release.
#' @name field
#' @docType data
#' @aliases soil
#' @examples
#' polymer(soil, field)
NULL

#' Simple overlapping layers
#'
#' `A`, `B` and `C` are three partially overlapping polygon layers with
#' different attributes: `A` is four squares in a 2 by 2 grid over the unit square, `B` is a
#' single triangle and `C` a single thin rectangle.
#'
#' @format 'sf' data frames with no coordinate reference system:
#' * `A`: 4 rows, `layer`, `a` and `geometry`.
#' * `B`: 1 row, `b` and `geometry`.
#' * `C`: 1 row, `c` and `geometry`.
#' @docType data
#' @name ABC
#' @aliases A B C
#' @examples
#' polymer(A, B, C)
NULL
