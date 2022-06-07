library(raster)
library(sf)
library(tidyverse)

plot(raster::raster("/users/seanhardison/downloads/USGS_13_n41w108_20180328.tif"))

d <- raster::raster(here::here("data/USGS_13_n41w107_20220216.tif"))
plot(d)
stb <- sf::st_read(here::here("data/steamboat.kml")) %>% 
  sf::st_zm() %>% 
  st_transform(.,crs = st_crs(d)) %>% 
  fasterize::fasterize(.,d)

#steamboat
crop_extent <- extent(c(xmin = -107,
                        xmax = -106.5,
                        ymin = 40.3,
                        ymax = 40.7))
stb_dem <- crop(d, crop_extent)
plot(stb_dem)
save(stb_dem, file = here::here("data/sbt_dem.rdata"))
