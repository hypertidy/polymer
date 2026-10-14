library(sf)
library(dplyr)

## https://gis.stackexchange.com/a/230161/1204
# example data from raster package
soil <- st_read(system.file("external/lux.shp", package="raster")) %>%
  # add in some fake soil type data
  mutate(soil = LETTERS[c(1:6,1:6)]) %>%
  select(soil)

# field polygons
field <- c("POLYGON((6 49.75,6 50,6.4 50,6.4 49.75,6 49.75))",
           "POLYGON((5.8 49.5,5.8 49.7,6.2 49.7,6.2 49.5,5.8 49.5))") %>%
  st_as_sfc(crs = st_crs(soil)) %>%
  st_sf(field = c('x','y'), geoms = ., stringsAsFactors = FALSE)

usethis::use_data(soil, field)

## 2026-10-14: the saved objects carried an old-style sf crs; reset to the
## same CRS with a current sf::st_crs() and saved again
# sf::st_crs(soil) <- sf::st_crs(4326); sf::st_crs(field) <- sf::st_crs(4326)
# save(soil, file = "data/soil.rda", compress = "bzip2")
# save(field, file = "data/field.rda", compress = "bzip2")
