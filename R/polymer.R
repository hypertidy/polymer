#' Build a polymer mesh from polygon layers
#'
#' Combine one or more polygon layers into a single triangle mesh, where
#' every input edge from every layer is kept as a chain of mesh edges, and
#' record which input features cover each triangle.
#'
#' All the inputs' edges are triangulated together in one constrained
#' Delaunay triangulation. Where edges from different features cross, the
#' crossing is inserted as a new vertex, so the mesh is a partition of the
#' plane that respects every input boundary. Vertices are shared on exact
#' coordinate equality before triangulation.
#'
#' Each triangle is then labelled with the features that cover it. The
#' winding number of every feature is propagated across the mesh from the
#' outside: stepping over a mesh edge adds or subtracts one for each input
#' edge lying on it, by its direction. This uses no coordinates, so a sliver
#' triangle cannot be mislabelled, and edges not on the walk are checked for
#' agreement, which detects broken noding (a warning is given). Shells are
#' oriented counter-clockwise and holes clockwise first, the first ring of
#' each polygon being its shell, so holes are subtracted. The fill rule then
#' decides which windings count as covered: `"nonzero"` (any non-zero
#' winding, so a duplicated part stays covered), `"evenodd"` (odd winding, so
#' a duplicated part cancels, as in polymer 0.3.0) or `"positive"`.
#' Triangles covered by no feature (gaps enclosed by the inputs) are dropped.
#'
#' The result is the mesh and its index, not a set of polygons. Use
#' [fuse()] or [layer_n()] to get polygons back.
#'
#' @param ... Polygon layers. Each can be anything the 'wk' package can read
#'   (an 'sf' data frame or `sfc`, a `wk::wkb()` or `wk::wkt()` vector, and
#'   so on). Data frames keep their non-geometry columns as attributes.
#' @param rule The fill rule: `"nonzero"` (the default), `"evenodd"` or
#'   `"positive"`.
#' @param engine The triangulation engine. Only `"cdtr"` is available.
#' @param label How triangles are labelled: `"mesh"` (the default) propagates
#'   winding numbers over the mesh, `"pip"` tests each triangle's centroid
#'   against every ring, as polymer 0.3.0 did. `"pip"` is kept for
#'   comparison and will be removed.
#'
#' @return A `polymer` object, a list with
#' * `mesh`: a `wkpool_tri` (see [wkpool::new_wkpool_tri()]) holding the
#'   triangles on a shared vertex pool. The vertex table has an `origin`
#'   column: `"input"` for input vertices and `"crossing"` for vertices
#'   inserted where input edges cross.
#' * `index`: a data frame with one row per triangle and covering feature:
#'   `triangle` (row in `mesh`), `layer` (position of the layer in `...`) and
#'   `feature` (row within that layer).
#' * `layers`: a list with one data frame of attributes per layer (no
#'   geometry).
#' * `crs`: the coordinate reference system shared by the inputs.
#'
#' @export
#' @examples
#' pool <- polymer(A, B, C)
#' pool
#' head(pool$index)
#'
#' ## the mesh is a 'wkpool' triangle pool
#' pool$mesh
polymer <- function(..., rule = c("nonzero", "evenodd", "positive"),
                    engine = "cdtr", label = c("mesh", "pip")) {
  rule <- match.arg(rule)
  engine <- match.arg(engine)
  label <- match.arg(label)
  inputs <- list(...)
  if (length(inputs) < 1L) stop("polymer() needs at least one layer")

  geoms <- lapply(inputs, layer_geometry)
  crs <- common_crs(geoms)
  n_each <- vapply(geoms, function(g) length(wk::as_wkb(g)), integer(1))
  geom_all <- do.call(c, lapply(geoms, function(g) {
    wk::wk_set_crs(wk::wk_drop_m(wk::wk_drop_z(wk::as_wkb(g))), NULL)
  }))
  check_polygonal(geom_all)

  ## feature ids across all layers, and the layer each belongs to
  feature_layer <- rep(seq_along(inputs), n_each)
  feature_row <- sequence(n_each)

  ## vertex pool on the exact rung: shared on identical coordinates
  pool <- wkpool::merge_coincident(wkpool::establish_topology(geom_all))
  v <- wkpool::pool_vertices(pool)
  s <- wkpool::pool_segments(pool)

  ## ring roles from the inputs: the first ring of each part is its shell.
  ## Rings are reoriented, shells counter-clockwise and holes clockwise, so
  ## winding counts shells up and holes down whatever the input order.
  rings <- ring_table(wk::wk_coords(geom_all))
  if (nrow(rings) != length(unique(s$.path)) || any(s$.path > nrow(rings))) {
    stop("could not match the vertex pool's rings to the input rings")
  }
  flip <- rings$sign[s$.path] < 0
  s0 <- ifelse(flip, s$.vx1, s$.vx0)
  s1 <- ifelse(flip, s$.vx0, s$.vx1)
  keep_s <- s0 != s1
  s0 <- s0[keep_s]
  s1 <- s1[keep_s]
  s_feature <- s$.feature[keep_s]
  if (length(s0) < 3L) stop("the inputs have no polygon edges")

  ## CDT cannot take the same segment twice, so triangulate each distinct
  ## segment once and keep the map back to every input segment on it, with
  ## whether it runs the same way
  lo <- pmin(s0, s1)
  hi <- pmax(s0, s1)
  key <- paste(lo, hi)
  first <- !duplicated(key)
  s_unique <- match(key, key[first])
  s_same <- s0 == s0[first][s_unique]

  tri <- cdtr::cdt_triangulate(v$x, v$y, match(s0[first], v$.vx), match(s1[first], v$.vx),
                               erase = "outer", edge_sources = label == "mesh")

  TT <- tri$T
  if (label == "mesh") {
    lab <- label_mesh(tri, s_feature, s_unique, s_same, rule)
    if (lab$inconsistent_edges > 0) {
      warning(sprintf("labelling found %i inconsistent mesh edges: the noding is broken and labels may be wrong",
                      as.integer(lab$inconsistent_edges)), call. = FALSE)
    }
    hits <- lab$hits
  } else {
    ## centroid of each triangle against every ring
    cx <- (tri$P[TT[, 1L], 1L] + tri$P[TT[, 2L], 1L] + tri$P[TT[, 3L], 1L]) / 3
    cy <- (tri$P[TT[, 1L], 2L] + tri$P[TT[, 2L], 2L] + tri$P[TT[, 3L], 2L]) / 3
    hits <- label_triangles(cx, cy, wk::wk_coords(geom_all), rings, rule)
  }

  ## keep only triangles inside at least one feature
  keep <- sort(unique(hits$triangle))
  TT <- TT[keep, , drop = FALSE]
  hits$triangle <- match(hits$triangle, keep)
  hits <- hits[order(hits$triangle, hits$feature), , drop = FALSE]

  vertices <- data.frame(.vx = seq_len(nrow(tri$P)),
                         x = tri$P[, 1L], y = tri$P[, 2L],
                         origin = as.character(tri$origin))
  mesh <- wkpool::new_wkpool_tri(vertices, TT[, 1L], TT[, 2L], TT[, 3L],
                                 crs = crs)

  index <- data.frame(triangle = hits$triangle,
                      layer = feature_layer[hits$feature],
                      feature = feature_row[hits$feature])
  rownames(index) <- NULL

  new_polymer(mesh = mesh, index = index,
              layers = mapply(layer_attributes, inputs, n_each, SIMPLIFY = FALSE),
              crs = crs)
}

new_polymer <- function(mesh, index, layers, crs) {
  structure(list(mesh = mesh, index = index, layers = layers, crs = crs),
            class = "polymer")
}

## the geometry column of a data frame, or the input itself
layer_geometry <- function(x) {
  if (is.data.frame(x)) {
    is_geom <- vapply(x, wk::is_handleable, logical(1))
    if (!any(is_geom)) stop("a data frame layer has no geometry column")
    return(x[[which(is_geom)[1L]]])
  }
  if (!wk::is_handleable(x)) {
    stop("each layer must be a geometry vector or a data frame with a geometry column")
  }
  x
}

## the non-geometry columns of a data frame as a plain data frame
layer_attributes <- function(x, n) {
  if (!is.data.frame(x)) return(data.frame(row.names = seq_len(n)))
  cols <- as.list(x)
  cols <- cols[!vapply(cols, wk::is_handleable, logical(1))]
  structure(cols, class = "data.frame", row.names = seq_len(n))
}

common_crs <- function(geoms) {
  crs <- lapply(geoms, wk::wk_crs)
  for (i in seq_along(crs)[-1L]) {
    if (!wk::wk_crs_equal(crs[[1L]], crs[[i]])) {
      stop("all layers must have the same coordinate reference system")
    }
  }
  crs[[1L]]
}

check_polygonal <- function(x) {
  type <- wk::wk_meta(x)$geometry_type
  ## 3 polygon, 6 multipolygon; empty or missing features carry no edges
  bad <- !is.na(type) & !type %in% c(3L, 6L)
  if (any(bad)) stop("polymer() needs polygon or multipolygon geometry")
  invisible(x)
}

#' Print a polymer
#'
#' Print a short summary of a polymer mesh: the number of layers, input
#' features, triangles, crossing vertices and the deepest overlap.
#'
#' @param x A polymer.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#' @export
#'
#' @examples
#' print(polymer(A, B, C))
print.polymer <- function(x, ...) {
  v <- wkpool::pool_vertices(x$mesh)
  depth <- tabulate(x$index$triangle, nbins = length(x$mesh))
  cat("polymer mesh:\n")
  cat(sprintf("Layers:    %i\n", length(x$layers)))
  cat(sprintf("Features:  %i\n", sum(vapply(x$layers, nrow, integer(1)))))
  cat(sprintf("Triangles: %i\n", length(x$mesh)))
  cat(sprintf("Vertices:  %i (%i at crossings)\n", nrow(v), sum(v$origin == "crossing")))
  cat(sprintf("Overlaps:  %i triangles covered more than once, at most %i deep\n",
              sum(depth > 1L), max(c(0L, depth))))
  invisible(x)
}

#' Plot a polymer
#'
#' Draw the triangles of the mesh. With `show_intersection = TRUE` the
#' triangles covered by two or more input features are filled.
#'
#' @param x A polymer.
#' @param ... Arguments passed to [graphics::polygon()], such as `col` and
#'   `border`.
#' @param show_intersection Logical, fill the triangles covered more than
#'   once (default `FALSE`).
#'
#' @return `x`, invisibly.
#' @export
#' @importFrom graphics plot polygon
#' @examples
#' plot(polymer(A, B, C))
#' plot(polymer(A, B, C), show_intersection = TRUE, col = "firebrick")
plot.polymer <- function(x, ..., show_intersection = FALSE) {
  v <- wkpool::pool_vertices(x$mesh)
  t <- wkpool::pool_triangles(x$mesh)
  graphics::plot(range(v$x), range(v$y), type = "n", asp = 1,
                 xlab = "", ylab = "", axes = FALSE)
  ring_xy <- function(t) {
    idx <- as.vector(rbind(t$.vx0, t$.vx1, t$.vx2, NA))
    idx <- match(idx, v$.vx)
    list(x = v$x[idx], y = v$y[idx])
  }
  if (show_intersection) {
    graphics::polygon(ring_xy(t), border = "grey45", col = NA)
    depth <- tabulate(x$index$triangle, nbins = nrow(t))
    graphics::polygon(ring_xy(t[depth > 1L, , drop = FALSE]), ...)
  } else {
    graphics::polygon(ring_xy(t), ...)
  }
  invisible(x)
}
