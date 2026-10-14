# polymer2 prototype
# Triangle pool data fusion: wk → wkpool → RTriangle → geos → membership table
#
# Dependencies: wk, wkpool, geos, RTriangle
# No sf anywhere.

# ---- build step --------------------------------------------------------

#' Build a polymer pool from multiple wk-handleable layers
#'
#' @param ... wk-handleable geometry vectors (one per layer)
#' @param attributes optional list of data.frames, one per layer
#' @return A polymer object (list of data.frames)
polymer <- function(..., attributes = NULL) {
  layers <- list(...)
  n_layers <- length(layers)

  # 1. Convert each layer to wkpool, accumulate into combined vertex + segment pool
  #    with layer provenance
  combined <- pm_combine_layers(layers)

  # 2. Feed to RTriangle
  tri <- RTriangle::triangulate(
    RTriangle::pslg(P = combined$P, S = combined$S, H = combined$H),
    Q = TRUE
  )

  # 3. Build triangle table with areas
  n_tri <- nrow(tri$T)
  verts <- tri$P

  v1 <- tri$T[, 1]; v2 <- tri$T[, 2]; v3 <- tri$T[, 3]
  areas <- 0.5 * abs(
    (verts[v2, 1] - verts[v1, 1]) * (verts[v3, 2] - verts[v1, 2]) -
      (verts[v3, 1] - verts[v1, 1]) * (verts[v2, 2] - verts[v1, 2])
  )

  triangles <- data.frame(
    tid = seq_len(n_tri),
    v1 = v1, v2 = v2, v3 = v3,
    area = areas
  )

  # 4. Compute centroids
  cx <- (verts[v1, 1] + verts[v2, 1] + verts[v3, 1]) / 3
  cy <- (verts[v1, 2] + verts[v2, 2] + verts[v3, 2]) / 3
  centroids <- wk::xy(cx, cy)

  # 5. Classify triangles against each input layer using geos
  membership <- pm_classify(centroids, layers)

  # 6. Build signature table (equivalence classes)
  signatures <- pm_build_signatures(membership, n_tri)

  # 7. Assemble the pool object
  pool <- structure(
    list(
      vertices = data.frame(vid = seq_len(nrow(verts)), x = verts[, 1], y = verts[, 2]),
      triangles = triangles,
      membership = membership,
      signatures = signatures,
      layers = layers,         # keep originals for later queries
      attributes = attributes,
      n_layers = n_layers,
      centroids = centroids
    ),
    class = "polymer"
  )
  pool
}

# ---- internal build helpers ---------------------------------------------

#' Combine multiple layers into a single vertex pool + segment matrix for RTriangle
pm_combine_layers <- function(layers) {
  all_verts <- list()
  all_segs <- list()
  all_holes <- list()
  seg_prov <- list()
  offset <- 0L

  for (i in seq_along(layers)) {
    # wkpool gives us what we need
    wp <- wkpool::establish_topology(layers[[i]])
    wp <- wkpool::merge_coincident(wp, tolerance = 0)

    v <- wkpool::pool_vertices(wp)   # .vx, x, y
    s <- wkpool::pool_segments(wp)   # .vx0, .vx1, .feature

    # Offset vertex indices for this layer
    all_verts[[i]] <- cbind(v$x, v$y)
    all_segs[[i]] <- cbind(s$.vx0 + offset, s$.vx1 + offset)

    # Track provenance: which layer and feature each segment came from
    seg_prov[[i]] <- data.frame(
      layer_id = i,
      object_id = s$.feature
    )

    # Hole points: wkpool provides these
    h <- as.data.frame(wkpool::hole_points(wp))  # x, y — one point per hole ring
    if (!is.null(h) && nrow(h) > 0) {
      all_holes[[i]] <- cbind(h$x, h$y)
    }

    offset <- offset + nrow(v)
  }

  P <- do.call(rbind, all_verts)
  S <- do.call(rbind, all_segs)
  H <- if (length(all_holes) > 0) do.call(rbind, all_holes) else NULL

  # TODO: vertex deduplication across layers at the combined pool level
  # For now, RTriangle handles near-coincident vertices reasonably

  list(P = P, S = S, H = H, seg_prov = do.call(rbind, seg_prov))
}


#' Classify triangle centroids against input layers using geos strtree
pm_classify <- function(centroids, layers) {
  rows <- vector("list", length(layers))

  for (i in seq_along(layers)) {
    # Convert layer to geos for spatial predicates
    g <- geos::as_geos_geometry(layers[[i]])

    # Build spatial index on input polygons
    tree <- geos::geos_strtree(g)

    # Query: which polygons might contain each centroid? (bbox filter)
    candidates <- geos::geos_strtree_query(tree, geos::as_geos_geometry(centroids))

    # Refine: actual containment test
    for (j in seq_along(candidates)) {
      if (length(candidates[[j]]) > 0) {
        # Test each candidate polygon
        cent_geom <- geos::as_geos_geometry(centroids[j])
        for (k in candidates[[j]]) {
          if (geos::geos_contains(g[k], cent_geom)) {
            rows[[i]] <- rbind(rows[[i]], data.frame(
              tid = j,
              layer_id = i,
              object_id = k
            ))
          }
        }
      }
    }
  }

  do.call(rbind, rows)
}


#' Build signature equivalence classes from membership table
pm_build_signatures <- function(membership, n_tri) {
  # A signature is the sorted set of (layer_id, object_id) for each triangle
  # Triangles with identical signatures are topologically equivalent

  # Build a string key per triangle
  keys <- tapply(
    paste(membership$layer_id, membership$object_id, sep = ":"),
    membership$tid,
    function(x) paste(sort(x), collapse = "|")
  )

  # Assign signature IDs
  all_keys <- rep("", n_tri)
  all_keys[as.integer(names(keys))] <- keys

  sig_ids <- match(all_keys, unique(all_keys))

  data.frame(
    tid = seq_len(n_tri),
    signature_id = sig_ids,
    signature_key = all_keys,
    stringsAsFactors = FALSE
  )
}


# ---- print/summary ------------------------------------------------------

#' @export
print.polymer <- function(x, ...) {
  n_overlap <- sum(table(x$membership$tid) > 1)
  cat(sprintf(
    "polymer mesh:\n  Layers:    %d\n  Triangles: %d\n  Signatures: %d\n  (Overlapping triangles: %d)\n",
    x$n_layers,
    nrow(x$triangles),
    length(unique(x$signatures$signature_id)),
    n_overlap
  ))
  invisible(x)
}


# ---- query functions (no geometry reconstruction) -----------------------

#' Area-weighted cross-tabulation between layers
#'
#' @param pool A polymer object
#' @param rows_layer Index of layer for rows
#' @param rows_col Column name in that layer's attributes
#' @param cols_layer Index of layer for columns
#' @param cols_col Column name in that layer's attributes
#' @return data.frame with area totals for each combination
pm_crosstab <- function(pool, rows_layer, rows_col, cols_layer, cols_col) {
  m <- pool$membership
  tri <- pool$triangles[, c("tid", "area")]

  # Get triangles in both layers
  m_rows <- m[m$layer_id == rows_layer, c("tid", "object_id")]
  m_cols <- m[m$layer_id == cols_layer, c("tid", "object_id")]

  # Inner join on tid = triangles present in both layers
  shared <- merge(m_rows, m_cols, by = "tid", suffixes = c("_row", "_col"))

  # Join areas
  shared <- merge(shared, tri, by = "tid")

  # Join row attributes
  if (!is.null(pool$attributes[[rows_layer]])) {
    row_attrs <- pool$attributes[[rows_layer]]
    row_attrs$object_id_row <- seq_len(nrow(row_attrs))
    shared <- merge(shared, row_attrs[, c("object_id_row", rows_col)], by = "object_id_row")
  }

  # Join column attributes
  if (!is.null(pool$attributes[[cols_layer]])) {
    col_attrs <- pool$attributes[[cols_layer]]
    col_attrs$object_id_col <- seq_len(nrow(col_attrs))
    shared <- merge(shared, col_attrs[, c("object_id_col", cols_col)], by = "object_id_col")
  }

  # Aggregate
  aggregate(area ~ get(rows_col) + get(cols_col), data = shared, FUN = sum)
}


#' Coverage fractions: what fraction of each object in `of_layer` is covered
#' by each object in `by_layer`
#'
#' @param pool A polymer object
#' @param of_layer Index of layer whose objects define the denominator
#' @param by_layer Index of layer whose objects define the grouping
#' @return data.frame: of_object, by_object, area, frac
pm_coverage <- function(pool, of_layer, by_layer) {
  m <- pool$membership
  tri <- pool$triangles[, c("tid", "area")]

  # Total area of each object in of_layer
  m_of <- m[m$layer_id == of_layer, c("tid", "object_id")]
  m_of <- merge(m_of, tri, by = "tid")
  totals <- aggregate(area ~ object_id, data = m_of, FUN = sum)
  names(totals) <- c("of_object", "total_area")

  # Triangles in both layers
  m_by <- m[m$layer_id == by_layer, c("tid", "object_id")]
  shared <- merge(m_of[, c("tid", "object_id")], m_by, by = "tid", suffixes = c("_of", "_by"))
  names(shared) <- c("tid", "of_object", "by_object")
  shared <- merge(shared, tri, by = "tid")

  # Sum area per (of_object, by_object) pair
  overlap <- aggregate(area ~ of_object + by_object, data = shared, FUN = sum)

  # Join totals and compute fraction
  result <- merge(overlap, totals, by = "of_object")
  result$frac <- result$area / result$total_area
  result$total_area <- NULL
  result[order(result$of_object, -result$frac), ]
}


#' Overlap area between two layers (total or per-object)
#'
#' @param pool A polymer object
#' @param layer_a,layer_b Layer indices
#' @param by_object If TRUE, return per-object-pair areas
#' @return Scalar total overlap area, or data.frame if by_object=TRUE
pm_overlap_area <- function(pool, layer_a, layer_b, by_object = FALSE) {
  m <- pool$membership
  tri <- pool$triangles[, c("tid", "area")]

  m_a <- m[m$layer_id == layer_a, c("tid", "object_id")]
  m_b <- m[m$layer_id == layer_b, c("tid", "object_id")]

  shared <- merge(m_a, m_b, by = "tid", suffixes = c("_a", "_b"))
  shared <- merge(shared, tri, by = "tid")

  if (by_object) {
    aggregate(area ~ object_id_a + object_id_b, data = shared, FUN = sum)
  } else {
    # Deduplicate triangles (a triangle might be in multiple object pairs)
    sum(tri$area[tri$tid %in% unique(shared$tid)])
  }
}


#' Spatial predicates between objects of two layers
#'
#' @param pool A polymer object
#' @param layer_a,layer_b Layer indices
#' @return data.frame with object pairs and predicates
pm_predicates <- function(pool, layer_a, layer_b) {
  m <- pool$membership
  tri <- pool$triangles[, c("tid", "area")]

  # All triangles per object in each layer
  m_a <- m[m$layer_id == layer_a, c("tid", "object_id")]
  m_b <- m[m$layer_id == layer_b, c("tid", "object_id")]

  # For each object in A, which objects in B share triangles?
  shared <- merge(m_a, m_b, by = "tid", suffixes = c("_a", "_b"))

  if (nrow(shared) == 0) {
    return(data.frame(
      object_a = integer(), object_b = integer(),
      intersects = logical(), a_within_b = logical(), b_within_a = logical(),
      overlap_area = numeric()
    ))
  }

  # Per (a, b) pair: count shared triangles, and check containment
  pairs <- unique(shared[, c("object_id_a", "object_id_b")])

  # Total triangle counts per object
  a_counts <- table(m_a$object_id)
  b_counts <- table(m_b$object_id)

  # Shared triangle counts per pair
  shared_counts <- aggregate(tid ~ object_id_a + object_id_b, data = shared,
                             FUN = function(x) length(unique(x)))

  result <- merge(pairs, shared_counts, by = c("object_id_a", "object_id_b"))
  names(result)[3] <- "n_shared"

  result$n_a <- as.integer(a_counts[as.character(result$object_id_a)])
  result$n_b <- as.integer(b_counts[as.character(result$object_id_b)])

  result$intersects <- TRUE
  result$a_within_b <- result$n_shared == result$n_a
  result$b_within_a <- result$n_shared == result$n_b

  # Overlap area
  shared <- merge(shared, tri, by = "tid")
  overlap_areas <- aggregate(
    area ~ object_id_a + object_id_b,
    data = shared[!duplicated(shared[, c("tid", "object_id_a", "object_id_b")]), ],
    FUN = sum
  )
  result <- merge(result, overlap_areas, by = c("object_id_a", "object_id_b"))
  names(result)[names(result) == "area"] <- "overlap_area"

  result[, c("object_id_a", "object_id_b", "intersects",
             "a_within_b", "b_within_a", "overlap_area")]
}


#' Area-weighted attribute summary
#'
#' @param pool A polymer object
#' @param values_layer Layer to take values from
#' @param value_col Column name for the value to aggregate
#' @param by_layer Layer to group by
#' @param fun Aggregation function (default: weighted.mean)
#' @return data.frame: by_object, result
pm_weighted_summary <- function(pool, values_layer, value_col, by_layer,
                                fun = weighted.mean) {
  m <- pool$membership
  tri <- pool$triangles[, c("tid", "area")]

  m_val <- m[m$layer_id == values_layer, c("tid", "object_id")]
  m_by <- m[m$layer_id == by_layer, c("tid", "object_id")]

  shared <- merge(m_val, m_by, by = "tid", suffixes = c("_val", "_by"))
  shared <- merge(shared, tri, by = "tid")

  # Join value attributes
  val_attrs <- pool$attributes[[values_layer]]
  val_attrs$.oid <- seq_len(nrow(val_attrs))
  shared <- merge(shared, val_attrs[, c(".oid", value_col)],
                  by.x = "object_id_val", by.y = ".oid")

  # Weighted aggregation per by_object
  aggregate(
    as.formula(paste(value_col, "~ object_id_by")),
    data = shared,
    FUN = function(vals) fun(vals, shared$area[match(vals, shared[[value_col]])])
  )
}


#' Set-theoretic filter on the triangle pool
#'
#' Returns triangle IDs matching a layer membership condition.
#' The function receives L, where L(i) returns a logical vector
#' (TRUE for each triangle in layer i).
#'
#' @param pool A polymer object
#' @param fn A function of L, e.g. \(L) L(1) & L(2) & !L(3)
#' @return Integer vector of matching triangle IDs
pm_where <- function(pool, fn) {
  m <- pool$membership
  n_tri <- nrow(pool$triangles)
  tri_ids <- seq_len(n_tri)

  L <- function(i) tri_ids %in% m$tid[m$layer_id == i]

  mask <- fn(L)
  which(mask)
}


#' Total area matching a pm_where condition
#'
#' @param pool A polymer object
#' @param fn A function of L, e.g. \(L) L(1) & L(2)
#' @return Numeric scalar: total area of matching triangles
pm_where_area <- function(pool, fn) {
  tids <- pm_where(pool, fn)
  sum(pool$triangles$area[tids])
}


# ---- example usage (not run) --------------------------------------------

if (FALSE) {
  # Two polygon layers as wk vectors (could come from anywhere)
  soil <- gdalraster::ogr_read("soil.gpkg") |> wk::as_wkb()
  landuse <- gdalraster::ogr_read("landuse.gpkg") |> wk::as_wkb()
  catchment <- gdalraster::ogr_read("catchments.gpkg") |> wk::as_wkb()

  soil_attrs <- data.frame(type = c("clay", "sandy_loam", "peat"), pH = c(5.2, 6.8, 4.1))
  landuse_attrs <- data.frame(class = c("forest", "cropland", "urban"))
  catch_attrs <- data.frame(name = c("Upper Derwent", "Lower Derwent"))

  # Build the pool — this is the expensive step, done once
  pool <- polymer(soil, landuse, catchment,
                  attributes = list(soil_attrs, landuse_attrs, catch_attrs))
  pool
  #> polymer mesh:
  #>   Layers:    3
  #>   Triangles: 847
  #>   Signatures: 23
  #>   (Overlapping triangles: 312)

  # ---- analytical queries (no geometry reconstruction) ----

  # What fraction of each catchment is forested?
  pm_coverage(pool, of_layer = 3, by_layer = 2)
  #>   of_object by_object     area  frac
  #>   1         1           4200  0.62   # Upper Derwent 62% forest
  #>   1         2           2580  0.38   # Upper Derwent 38% cropland
  #>   2         1           1100  0.22   # Lower Derwent 22% forest
  #>   ...

  # Total overlap between soil and landuse layers
  pm_overlap_area(pool, 1, 2)
  #> [1] 8934.5

  # Area-weighted mean soil pH per catchment
  pm_weighted_summary(pool, values_layer = 1, value_col = "pH", by_layer = 3)
  #>   object_id_by   pH
  #>   1              5.9   # Upper Derwent
  #>   2              6.2   # Lower Derwent

  # Cross-tab: soil type × land use class → area
  pm_crosstab(pool, rows_layer = 1, rows_col = "type",
              cols_layer = 2, cols_col = "class")
  #>   type        class      area
  #>   clay        forest     1234
  #>   clay        cropland    567
  #>   sandy_loam  forest     2345
  #>   ...

  # Which soil objects are entirely within a single catchment?
  cov <- pm_coverage(pool, of_layer = 1, by_layer = 3)
  cov[cov$frac >= 1.0, ]

  # Spatial predicates
  pm_predicates(pool, layer_a = 1, layer_b = 3)
  #>   object_a  object_b  intersects  a_within_b  b_within_a  overlap_area
  #>   1         1         TRUE        TRUE        FALSE       1234
  #>   2         1         TRUE        FALSE       FALSE        567
  #>   ...

  # Set-theoretic queries
  # Triangles in soil AND landuse but NOT catchment
  tids <- pm_where(pool, \(L) L(1) & L(2) & !L(3))
  pm_where_area(pool, \(L) L(1) & L(2) & !L(3))

  # Triangles where at least 2 layers overlap
  tids_2plus <- pm_where(pool, \(L) (L(1) + L(2) + L(3)) >= 2)
}
