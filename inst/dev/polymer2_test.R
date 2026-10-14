# polymer2 end-to-end test
# Dependencies: wk, geos, RTriangle
# No sf, no wkpool — manual pslg construction to prove the pipeline

library(wk)
library(geos)
library(RTriangle)

# ============================================================
# 1. Create two overlapping polygon layers as wk geometry
# ============================================================

# Layer 1: two squares side by side (soil types)
#   square A: (0,0)-(1,1)    "clay"
#   square B: (1,0)-(2,1)    "sandy_loam"
layer1 <- wk::wkt(c(
  "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
  "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"
))
layer1_attrs <- data.frame(
  object_id = 1:2,
  type = c("clay", "sandy_loam"),
  pH = c(5.2, 6.8)
)

# Layer 2: one rectangle overlapping both squares (land use)
#   rect C: (0.5, 0.25)-(1.5, 0.75)   "cropland"
layer2 <- wk::wkt(c(
  "POLYGON ((0.5 0.25, 1.5 0.25, 1.5 0.75, 0.5 0.75, 0.5 0.25))"
))
layer2_attrs <- data.frame(
  object_id = 1,
  class = "cropland"
)

# ============================================================
# 2. Manually build the pslg (what wkpool would do)
# ============================================================

# Extract coordinates from wk geometries
extract_edges <- function(wk_geom, vertex_offset = 0L) {
  coords <- wk::wk_coords(wk_geom)

  vertices <- unique(coords[, c("x", "y")])
  vertices$vid <- seq_len(nrow(vertices)) + vertex_offset

  # Map coords back to vertex ids
  coord_vid <- vertices$vid[match(
    paste(coords$x, coords$y),
    paste(vertices$x, vertices$y)
  )]

  # Build segments: consecutive pairs within each ring, closing the ring
  segments <- data.frame(v1 = integer(), v2 = integer(), feature = integer())
  for (feat in unique(coords$feature_id)) {
    feat_coords <- coords[coords$feature_id == feat, ]
    vids <- coord_vid[coords$feature_id == feat]
    n <- nrow(feat_coords)
    if (n >= 2) {
      # Segments between consecutive vertices (ring is already closed in WKT
      # so last point == first point, take pairs from 1 to n-1)
      for (i in seq_len(n - 1)) {
        segments <- rbind(segments, data.frame(
          v1 = vids[i], v2 = vids[i + 1], feature = feat
        ))
      }
    }
  }

  list(vertices = vertices, segments = segments)
}

e1 <- extract_edges(layer1, vertex_offset = 0L)
e2 <- extract_edges(layer2, vertex_offset = nrow(e1$vertices))

# Combine into single vertex pool + segment matrix
P <- rbind(
  as.matrix(e1$vertices[, c("x", "y")]),
  as.matrix(e2$vertices[, c("x", "y")])
)
S <- rbind(
  as.matrix(e1$segments[, c("v1", "v2")]),
  as.matrix(e2$segments[, c("v1", "v2")])
)

cat("Vertices:", nrow(P), "\n")
cat("Segments:", nrow(S), "\n")

# ============================================================
# 3. Triangulate
# ============================================================

pslg <- RTriangle::pslg(P = P, S = S)
tri <- RTriangle::triangulate(pslg, Q = TRUE)

cat("Triangles:", nrow(tri$T), "\n")

# ============================================================
# 4. Compute triangle areas and centroids
# ============================================================

verts <- tri$P
v1 <- tri$T[, 1]; v2 <- tri$T[, 2]; v3 <- tri$T[, 3]

areas <- 0.5 * abs(
  (verts[v2, 1] - verts[v1, 1]) * (verts[v3, 2] - verts[v1, 2]) -
    (verts[v3, 1] - verts[v1, 1]) * (verts[v2, 2] - verts[v1, 2])
)

cx <- (verts[v1, 1] + verts[v2, 1] + verts[v3, 1]) / 3
cy <- (verts[v1, 2] + verts[v2, 2] + verts[v3, 2]) / 3

triangles <- data.frame(
  tid = seq_along(areas),
  area = areas
)

centroids <- wk::xy(cx, cy)

cat("Total mesh area:", sum(areas), "\n")
cat("(Expected: 2 sq units from layer1)\n\n")

# ============================================================
# 5. Classify triangles against input layers using geos
# ============================================================

classify_layer <- function(centroids, layer_geom, layer_id) {
  g <- geos::as_geos_geometry(layer_geom)
  cent_g <- geos::as_geos_geometry(centroids)

  rows <- list()
  for (obj_idx in seq_along(g)) {
    inside <- geos::geos_contains(g[obj_idx], cent_g)
    tids <- which(inside)
    if (length(tids) > 0) {
      rows[[length(rows) + 1]] <- data.frame(
        tid = tids,
        layer_id = layer_id,
        object_id = obj_idx
      )
    }
  }
  do.call(rbind, rows)
}

m1 <- classify_layer(centroids, layer1, layer_id = 1)
m2 <- classify_layer(centroids, layer2, layer_id = 2)
membership <- rbind(m1, m2)

cat("Membership rows:", nrow(membership), "\n")
cat("Triangles in layer 1:", length(unique(m1$tid)), "\n")
cat("Triangles in layer 2:", length(unique(m2$tid)), "\n")

# How many triangles are in both layers?
both <- intersect(m1$tid, m2$tid)
cat("Triangles in BOTH layers:", length(both), "\n\n")

# ============================================================
# 6. Query: coverage fractions
# ============================================================

# "What fraction of each soil polygon is covered by cropland?"
cat("== Coverage: fraction of each layer1 object covered by layer2 ==\n")

# Total area per layer1 object
m1_area <- merge(m1, triangles, by = "tid")
totals <- aggregate(area ~ object_id, data = m1_area, FUN = sum)
names(totals) <- c("soil_object", "total_area")

# Shared triangles (in both layers)
shared <- merge(m1[, c("tid", "object_id")], m2[, c("tid", "object_id")],
                by = "tid", suffixes = c("_soil", "_landuse"))
shared <- merge(shared, triangles, by = "tid")

overlap <- aggregate(area ~ object_id_soil, data = shared, FUN = sum)
names(overlap) <- c("soil_object", "overlap_area")

coverage <- merge(totals, overlap, by = "soil_object")
coverage$frac <- coverage$overlap_area / coverage$total_area

# Add attribute labels
coverage$type <- layer1_attrs$type[coverage$soil_object]

print(coverage)
cat("\n")

# Expected: the cropland rect is 1.0 x 0.5 = 0.5 sq units
# It overlaps 0.5 x 0.5 = 0.25 with clay (object 1) and 0.5 x 0.5 = 0.25 with sandy_loam (object 2)
# Each soil square is 1.0 sq units, so fraction should be 0.25 each
cat("Expected: clay frac = 0.25, sandy_loam frac = 0.25\n\n")

# ============================================================
# 7. Query: area-weighted mean pH per land use class
# ============================================================

cat("== Weighted summary: area-weighted mean soil pH in cropland ==\n")

shared$pH <- layer1_attrs$pH[shared$object_id_soil]
wmean <- weighted.mean(shared$pH, shared$area)
cat(sprintf("  Area-weighted mean pH in cropland: %.2f\n", wmean))
cat(sprintf("  (Expected: mean of 5.2 and 6.8 = %.2f, equal areas so simple mean)\n\n", mean(c(5.2, 6.8))))

# ============================================================
# 8. Query: set-theoretic — area in layer1 but NOT layer2
# ============================================================

cat("== Set operation: area of soil NOT covered by cropland ==\n")

soil_only_tids <- setdiff(m1$tid, m2$tid)
soil_only_area <- sum(triangles$area[soil_only_tids])
cat(sprintf("  Area in soil layer but not cropland: %.4f\n", soil_only_area))
cat(sprintf("  (Expected: 2.0 - 0.5 = 1.5)\n\n"))

# ============================================================
# 9. Query: predicates
# ============================================================

cat("== Predicates: does each soil object intersect the cropland? ==\n")

for (obj in unique(m1$object_id)) {
  obj_tris <- m1$tid[m1$object_id == obj]
  shared_tris <- intersect(obj_tris, m2$tid)
  n_obj <- length(obj_tris)
  n_shared <- length(shared_tris)

  cat(sprintf("  soil object %d (%s): intersects=%s, within=%s, overlap_area=%.4f\n",
              obj, layer1_attrs$type[obj],
              n_shared > 0,
              n_shared == n_obj,
              sum(triangles$area[shared_tris])
  ))
}

cat("\n== All queries answered without geometry reconstruction ==\n")

# ============================================================
# 10. Optional: visualise the mesh (base graphics, no sf)
# ============================================================

if (interactive()) {
  plot(NULL, xlim = c(-0.2, 2.2), ylim = c(-0.2, 1.2), asp = 1,
       xlab = "x", ylab = "y", main = "polymer2 triangle pool")

  # Colour by membership signature
  for (i in seq_len(nrow(tri$T))) {
    in_l1 <- i %in% m1$tid
    in_l2 <- i %in% m2$tid

    col <- if (in_l1 && in_l2) "#9b59b6"   # both: purple
    else if (in_l1) "#3498db"         # soil only: blue
    else if (in_l2) "#e74c3c"         # landuse only: red
    else "#cccccc"                     # outside: grey

    polygon(
      verts[tri$T[i, c(1, 2, 3, 1)], 1],
      verts[tri$T[i, c(1, 2, 3, 1)], 2],
      col = col, border = "white", lwd = 0.5
    )
  }

  legend("topright",
         fill = c("#3498db", "#e74c3c", "#9b59b6", "#cccccc"),
         legend = c("soil only", "landuse only", "both", "outside"),
         cex = 0.8
  )
}
