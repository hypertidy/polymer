
library(raster)
library(sf)
library(dplyr)
sst <- aggregate(tabularaster::ghrsst, fact = 5)
sst_ranges <- spex::polygonize(sst %/% 5) %>%
  group_by(analysed.sea.surface.temperature) %>% summarize()
plot(sst_ranges)

topo <- crop(anglr::gebco1, extent(sst))
topo_ranges <-spex::polygonize(topo %/% 500) %>%
  group_by(gebco1) %>% summarize()
sb_intersection(sb, col = "firebrick")

