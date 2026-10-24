## Preparation code for two r-spatial/sf requests, as polymer workflows:
##   sf#1959  Esri "Identity" equivalent (split A by B, keep both attributes)
##   sf#2242  n-ary intersection of one layer's polygons, with overlap counts
##
## Not yet run: check each section interactively before using it in a vignette.
## Needs polymer, sf and wk.

library(polymer)

## ---- helpers ---------------------------------------------------------------

## Attributes of one layer for the rows `i`; NA in `i` gives NA rows.
attrs_for <- function(x, i, prefix) {
  a <- sf::st_drop_geometry(x)
  if (ncol(a) == 0L) return(NULL)
  out <- a[i, , drop = FALSE]
  names(out) <- paste0(prefix, names(a))
  rownames(out) <- NULL
  out
}

## Expand fuse() pieces into one row per (piece, feature) pair for a chosen
## layer, so pieces covered by several features of that layer are repeated.
piece_features <- function(pieces, layer) {
  rows <- lapply(seq_len(nrow(pieces)), function(i) {
    o <- pieces$origins[[i]]
    f <- o$feature[o$layer == layer]
    if (length(f)) data.frame(piece = i, feature = f)
  })
  do.call(rbind, rows)
}

## ---- sf#1959: Identity -----------------------------------------------------

## The parts of `x` split along `y`. Each output polygon carries the attributes
## of the `x` feature it lies in and of the `y` feature covering it (NA where
## `y` does not). Areas of `y` outside `x` are dropped, as in Esri Identity.
polymer_identity <- function(x, y) {
  pool <- polymer(x, y)
  pieces <- fuse(pool)

  ## row per (piece, x feature, y feature); a piece with no y feature gets NA
  map <- do.call(rbind, lapply(seq_len(nrow(pieces)), function(i) {
    o <- pieces$origins[[i]]
    fx <- o$feature[o$layer == 1L]
    fy <- o$feature[o$layer == 2L]
    if (!length(fx)) return(NULL)
    if (!length(fy)) fy <- NA_integer_
    expand.grid(piece = i, x = fx, y = fy)
  }))

  out <- data.frame(row.names = seq_len(nrow(map)))
  for (a in list(attrs_for(x, map$x, "x."), attrs_for(y, map$y, "y."))) {
    if (!is.null(a)) out <- cbind(out, a)
  }
  out$geometry <- pieces$geometry[map$piece]
  sf::st_as_sf(out)
}

## The pairwise formulation from the issue, as the baseline.
pairwise_identity <- function(x, y) {
  y <- sf::st_set_agr(y, "constant")
  x <- sf::st_set_agr(x, "constant")
  inter <- sf::st_intersection(x, y)
  rest <- sf::st_difference(x, sf::st_union(y))
  dplyr::bind_rows(inter, rest)
}

## Example with the packaged layers: soil is the target, field the identity layer
res <- polymer_identity(soil, field)
plot(sf::st_geometry(res), col = hcl.colors(nrow(res), "Pastel 1"))

## Checks
## 1. every x feature is reproduced exactly: area by x feature is preserved
pieces <- fuse(polymer(soil, field))
px <- piece_features(pieces, 1L)
a_piece <- as.numeric(sf::st_area(sf::st_as_sfc(pieces$geometry)))[px$piece]
stopifnot(isTRUE(all.equal(as.vector(tapply(a_piece, px$feature, sum)),
                           as.numeric(sf::st_area(soil)), tolerance = 1e-9)))

## 2. pieces do not overlap: summed area equals the area of their union
stopifnot(isTRUE(all.equal(sum(as.numeric(sf::st_area(res))),
                           as.numeric(sf::st_area(sf::st_union(soil))),
                           tolerance = 1e-9)))

## Timing against the pairwise baseline (needs dplyr)
if (requireNamespace("dplyr", quietly = TRUE)) {
  print(system.time(polymer_identity(soil, field)))
  print(system.time(pairwise_identity(soil, field)))
}

## ---- sf#2242: n-ary intersection of one layer ------------------------------

## Overlapping discs at random positions, plus one exact duplicate (the
## issue's st_stack() stops on duplicate geometries).
set.seed(1)
n <- 60
pts <- sf::st_as_sf(data.frame(x = runif(n, 0, 100), y = runif(n, 0, 100),
                               id = seq_len(n)),
                    coords = c("x", "y"), crs = 3857)
discs <- sf::st_buffer(pts, dist = runif(n, 5, 15), nQuadSegs = 12)
discs <- rbind(discs, discs[1, ])

pool <- polymer(discs)
pool
pieces <- fuse(pool)

## overlap count and the ids of the input features behind each piece
pieces$n_overlaps <- pieces$n
pieces$ids <- lapply(pieces$origins, function(o) discs$id[o$feature])
res2 <- sf::st_as_sf(pieces[c("n_overlaps", "ids", "geometry")])
plot(res2["n_overlaps"])

## Checks
## 1. the pieces partition the plane: overlap-weighted area is the summed
##    input area, and plain area is the area of the union
a_piece <- as.numeric(sf::st_area(res2))
stopifnot(isTRUE(all.equal(sum(res2$n_overlaps * a_piece),
                           sum(as.numeric(sf::st_area(discs))),
                           tolerance = 1e-9)))
stopifnot(isTRUE(all.equal(sum(a_piece),
                           as.numeric(sf::st_area(sf::st_union(discs))),
                           tolerance = 1e-9)))

## 2. agreement with sf's single-layer n-ary intersection, by area per count
sf_nary <- sf::st_intersection(discs)
rbind(
  polymer = tapply(a_piece, res2$n_overlaps, sum),
  sf = tapply(as.numeric(sf::st_area(sf_nary)), sf_nary$n.overlaps, sum)
)

## 3. scaling
for (k in c(50, 100, 200)) {
  p <- sf::st_buffer(sf::st_as_sf(data.frame(x = runif(k, 0, 100),
                                             y = runif(k, 0, 100)),
                                  coords = c("x", "y")), 8)
  cat(k, "features: polymer",
      system.time(fuse(polymer(p)))[["elapsed"]], "s; sf",
      system.time(try(sf::st_intersection(p), silent = TRUE))[["elapsed"]], "s\n")
}
