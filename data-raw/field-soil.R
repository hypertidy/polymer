## `soil` is a generated coverage of 12 quadrilaterals (a jittered 4 by 3
## grid, so no edge is axis-aligned), and `field` two rectangles that cross
## it. The layout follows https://gis.stackexchange.com/a/230161 (fields
## crossing soil polygons), but no data from that answer or from GADM is used.
library(sf)

set.seed(3719)
nx <- 4L
ny <- 3L
lon <- seq(5.7, 6.5, length.out = nx + 1L)
lat <- seq(49.4, 50.2, length.out = ny + 1L)
g <- expand.grid(i = 0:nx, j = 0:ny)
vx <- lon[g$i + 1L]
vy <- lat[g$j + 1L]
inner <- g$i > 0 & g$i < nx & g$j > 0 & g$j < ny
vx[inner] <- vx[inner] + runif(sum(inner), -0.04, 0.04)
vy[inner] <- vy[inner] + runif(sum(inner), -0.05, 0.05)
vid <- function(i, j) j * (nx + 1L) + i + 1L

cells <- expand.grid(i = seq_len(nx) - 1L, j = seq_len(ny) - 1L)
rings <- lapply(seq_len(nrow(cells)), function(k) {
  i <- cells$i[k]
  j <- cells$j[k]
  ids <- vid(c(i, i + 1L, i + 1L, i, i), c(j, j, j + 1L, j + 1L, j))
  st_polygon(list(cbind(vx[ids], vy[ids])))
})

soil <- st_sf(soil = LETTERS[c(1:6, 1:6)], geometry = st_sfc(rings, crs = 4326))

field <- c("POLYGON((6 49.75,6 50,6.4 50,6.4 49.75,6 49.75))",
           "POLYGON((5.8 49.5,5.8 49.7,6.2 49.7,6.2 49.5,5.8 49.5))")
field <- st_sf(field = c("x", "y"), geoms = st_as_sfc(field, crs = 4326),
               stringsAsFactors = FALSE)

usethis::use_data(soil, field, overwrite = TRUE, compress = "bzip2")
