A <- spex::polygonize(raster::setValues(raster::raster(raster::extent(0, 1, 0, 1), nrows = 2, ncols = 2), 1:4))
A$a <- 1
x <- structure(c(0.655, 0.32, 0.1,
                  0.07, 0.68, 0.27), .Dim = c(3L, 2L), .Dimnames = list(NULL, c("x", "y")))
plot.sf <- function(x, ...) sp::plot(as(x, "Spatial"), ...)
B <- sf::st_sf(b = 2, geometry = sf::st_sfc(sf::st_polygon(list(x[c(1:nrow(x), 1), ]))))
plot(A); plot(B, add = T)

C <- sf::st_sf(c = 3, geometry = sf::st_sfc(st_polygon(list(matrix(c(0.1, 0.8, 0.8, 0.1,
                 0.55, 0.55, 0.63, 0.63), ncol = 2L)[c(1:4, 1), ]))))
plot(C, add = TRUE)


usethis::use_data(A, B, C)
