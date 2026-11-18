#' @keywords internal
"_PACKAGE"

#' @useDynLib polymer, .registration = TRUE
NULL

#' Field and soil polygons
#'
#' Two polygon layers used to show overlay: `soil` is a coverage of 12
#' quadrilaterals (a jittered 4 by 3 grid) with a made-up soil type, and
#' `field` has two rectangular fields that cross them. The layout follows an
#' example from a question on the GIS Stack Exchange
#' (<https://gis.stackexchange.com/a/230161>), but both layers are generated
#' by `data-raw/field-soil.R`, with no external data.
#'
#' @format 'sf' data frames in longitude and latitude (WGS84):
#' * `soil`: 12 rows, `soil` (character) and `geometry`.
#' * `field`: 2 rows, `field` (character) and `geoms`.
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
