library(tidyverse)
library(osmdata)
library(smoothr)
library(sfnetworks)
library(elevatr)
library(ggplot2)
library(raster)
library(rgdal)
library(rnaturalearth)
library(sp)
library(sf)
library(ggnewscale)
library(raster)

source(file.path("R/query_osm.R"))
load(here::here("data/sbt_dem.rdata"))
ncrs <- st_crs("+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs")

# waypoints
wps <- read.csv(here::here("data/wedding_waypoints.csv"))

# create slope and hillshade
slope = terrain(stb_dem, opt='slope')
aspect = terrain(stb_dem, opt='aspect')
hill = hillShade(slope, aspect, 40, 270)
hill_df <- hill %>% 
  crop(extent(c(xmin = -106.93,
                xmax = -106.79,
                ymin = 40.3425,
                ymax = 40.5))) %>% 
  dream::rst_to_tib(var_name = "slope")

# get topography
stb_cont <- rasterToContour(stb_dem, nlevels = 40)
full_map <- st_sf(geom = st_sfc(st_point(c(-106.93,40.3425)),
                                st_point(c(-106.79, 40.5))))

# contours
stb_sf <- as(stb_cont, "sf") %>% 
  st_transform(4326) %>% 
  mutate(level = as.numeric(level)) %>% 
  smooth(., method = "ksmooth", smoothness = 4) %>% 
  st_crop(full_map)

# full dem
stb_dem_df <- stb_dem %>% 
  crop(extent(c(xmin = -106.93,
                xmax = -106.79,
                ymin = 40.3425,
                ymax = 40.5))) %>% 
  # aggregate(fact = 10) %>% 
  dream::rst_to_tib()

# downtown bounding box
downtown <- st_sf(geom = st_sfc(st_point(c(-106.8625,40.4575)),
                                st_point(c(-106.8215, 40.493))),
                  crs = st_crs(stb_sf))
  
# query OSM----
process <- F
if (process){
  osm_all_roads <- query_osm(key = "highway", bb = "stb_sf")
  lines <- query_osm(key = "power", bb = "stb_sf")
  natural <- query_osm(key = "natural", bb = "stb_sf")
  natural2 <- query_osm(key = "landuse", bb = "stb_sf")
  bridges <- query_osm(key = "bridge", select = "osm_lines",bb = "stb_sf")
  access <- query_osm(key = "access", select = "osm_polygons",bb = "stb_sf")
  waterway <- query_osm(key = "waterway",
                        bb = "stb_sf")
  dt_build <- query_osm(key = "building",
                        bb = "downtown")
  water <- query_osm(key = "water",
                     bb = "stb_sf")
  away <- query_osm(key = "aerialway",
                    bb = "stb_sf")
  rodeo <- query_osm(key = "sport",
                    bb = "stb_sf")
  baseball <- query_osm(key = "leisure",
                        bb = "stb_sf")

  save(osm_all_roads,
       lines,
       baseball,
       natural,
       natural2,
       bridges,
       access,
       waterway,
       dt_build,
       water,
       away,
       rodeo,
       file = here::here("data/osm_data.rdata"))
} else {
  load(here::here("data/osm_data.rdata"))
}

# feature processing
osm_paths <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("path","footway","track")) %>% 
  mutate(is_trail = ifelse(str_detect(name, "Trail"), "Trail","Road"))

np_large_roads <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("motorway","unclassified"))

primary_roads <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("primary","trunk"))

tertiary <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("tertiary"))  

service <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("service","track"))

residential <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("residential"))

secondary <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("secondary"))

trails <- osm_all_roads$osm_lines %>% 
  filter(highway == "path")

dt_box <- natural2$osm_polygons %>% 
  filter(landuse == "commercial") %>% 
  dplyr::select(name) %>% 
  filter(st_area(.) == max(st_area(.)))

liftlines <- away$osm_lines %>% 
  filter(aerialway %in% c("platter",
                          "chair_lift")) %>% 
  dplyr::select(name)

hh_rodeo <- 
  rodeo$osm_polygons %>% 
  filter(sport == "equestrian") %>% 
  dplyr::select(name)

hh_baseball <- 
  rodeo$osm_polygons %>% 
  filter(sport == "baseball") %>% 
  dplyr::select(name)

pks <- 
  natural$osm_points %>% 
  filter(natural == "peak") %>% 
  dplyr::select(name, ele) %>% 
  mutate(name = paste0(name,"\n", ele, "m"))

snet1 <- sfnetworks::st_network_join(sfnetworks::as_sfnetwork(np_large_roads),
                                     sfnetworks::as_sfnetwork(primary_roads)) %>% 
  st_as_sf("edges")  %>% 
  # st_crop(ymin = 40.485, ymax = 40.49,
  #         xmin = -106.84, xmax = -106.83) %>%
  dplyr::select(highway) %>% 
  st_transform(ncrs) %>% 
  # {. ->> snet_ls1} %>% 
  st_buffer(dist = 15) %>%
  mutate(grp = "large") 

snet2 <- sfnetworks::st_network_join(sfnetworks::as_sfnetwork(secondary),
                                     sfnetworks::as_sfnetwork(tertiary)) %>% 
  st_as_sf("edges")  %>% 
  # st_crop(ymin = 40.485, ymax = 40.49,
  #         xmin = -106.84, xmax = -106.83) %>%
  dplyr::select(highway) %>% 
  st_transform(ncrs) %>% 
  # {. ->> snet_ls2} %>% 
  st_buffer(dist = 9) %>%
  mutate(grp = "medium") 

snet3 <- sfnetworks::st_network_join(sfnetworks::as_sfnetwork(service),
                                     sfnetworks::as_sfnetwork(residential)) %>%
  st_as_sf("edges") %>% 
  # st_crop(ymin = 40.485, ymax = 40.49,
  #         xmin = -106.84, xmax = -106.83) %>%
  dplyr::select(highway) %>% 
  st_transform(ncrs) %>% 
  # {. ->> snet_ls3} %>% 
  st_buffer(dist = 3) %>% 
  mutate(grp = "small") %>% 
  bind_rows(.,snet1) %>% 
  bind_rows(.,snet2) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  st_transform(st_crs(osm_paths))

pl <- lines$osm_lines %>% 
  st_transform(4326) %>%  
  dplyr::select(power)

woods <- 
  natural$osm_polygons %>% 
  st_transform(4326) %>% 
  filter(natural %in% c("scrub","wood","forest")) %>% 
  dplyr::select(natural)

woods2 <-   
  natural2$osm_polygons %>% 
  st_transform(4326) %>% 
  filter(landuse %in% c("winter_sports")) %>% 
  dplyr::select(landuse)

rivers <- 
  water$osm_polygons %>% 
  filter(is.na(name)) %>% 
  dplyr::select(osm_id) %>% 
  mutate(area = as.numeric(st_area(.))) %>% 
  filter(area > 10000)

lakes <- 
  water$osm_polygons %>% 
  filter(!is.na(name)) %>% 
  dplyr::select(name)

stream <- 
  waterway$osm_lines %>% 
  filter(waterway == "stream") %>% 
  dplyr::select(waterway)

# inset dem
stb_dem_df2 <- 
  stb_dem %>% 
  raster::crop(extent(downtown)) %>% 
  projectRaster(.,crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  dream::rst_to_tib()

# inset hill shade
hill_df2 <- 
  hill %>% 
  raster::crop(extent(downtown)) %>% 
  projectRaster(.,crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  dream::rst_to_tib()

#map constants
dt_inset_color <- "black"
trail_color <- "purple"
hs_alpha <- 0.8
mp <- 2000

stb_map <-
  ggplot() +
  # hill shade
    geom_tile(data = hill_df, aes(x = longitude, y = latitude, 
                                  fill = slope), show.legend = F) +
    scale_fill_gradient(low = "black", high = "white") +
    new_scale_fill()+
  # dem
  geom_raster(data = stb_dem_df, aes(x = longitude, y = latitude,
                                     fill = fill_var),
              alpha = hs_alpha) +
  scale_fill_gradient2(low = "#cfbd9b",
                       mid = "white",
                       high = "#5a8c54",
                       midpoint = mp) +

  # contours
  geom_sf(data = stb_sf, aes(alpha = level), size = 0.1, color = "#476930") +

  # water features
  geom_sf(data = rivers, color = "lightblue", fill = "lightblue",
          size = 0.5) +
  geom_sf(data = lakes, color = "transparent", fill = "lightblue") +
  geom_sf(data = stream, color = "lightblue",
          size = 0.25) +
  
  # trails
  geom_sf(data = trails, color = "white", size = 0.75) +
  geom_sf(data = trails, color = trail_color, size = 0.65) +
  geom_sf(data = trails, color = "#6e5e44", size = 0.55, lty = "11") +
  
  # roads
  geom_sf(data = snet3, size = 0.05, color = "grey80",
          fill = 'grey60') +

  # power lines
  geom_sf(data = pl, color = "grey", alpha = 0.9, size = 0.25) +
  geom_sf(data = pl %>% st_cast("POINT"), color = "grey", alpha = 0.9,
          size = 0.125) +
  
  # emerald inset
  geom_rect(aes(xmin = -106.8625, ymin = 40.4575,
                xmax =-106.8215, ymax = 40.493),
            fill = "transparent",
            color = dt_inset_color) +
  
  # waypoints
  geom_point(data = wps, 
             aes(x = long, y = lat),
             color = "purple") +

  # theme
  scale_x_continuous(expand = c(0.001,0.001)) +
  scale_y_continuous(expand = c(0.001,0.001)) +
  guides(alpha = "none",
         color= "none",
         fill = "none") +
  theme(rect = element_blank(),
        panel.background = element_rect(fill = "transparent", 
                                        color = NA),
        panel.ontop = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  coord_sf(xlim = c(-106.93,-106.79),
           ylim = c(40.3425, 40.5))

# downtown inset----
dt_build2 <- dt_build$osm_polygons %>% dplyr::select(geometry)%>% 
  st_crop(downtown)
sf::sf_use_s2(FALSE)

wp2 <- wps %>% 
  st_as_sf(coords = c("long","lat"),
           crs = st_crs(dt_build2)) %>% 
  st_crop(downtown)

snet4 <- 
  snet3 %>% 
  st_crop(downtown)

rivers2 <- 
  rivers %>% 
  st_crop(downtown)

trails2 <- 
  trails %>% 
  st_crop(downtown)

stb_sf2 <-
  stb_sf %>% 
  st_crop(downtown)

howelson_lifts <- 
  liftlines %>% 
  st_crop(downtown)

howelson_rodeo <- 
  hh_rodeo %>% 
  st_crop(downtown)

howelson_baseball <- 
  hh_baseball %>% 
  st_crop(downtown)


dt_inset <-
  ggplot() +
  # hill shade
  geom_tile(data = hill_df2, aes(x = longitude, y = latitude, 
                                fill = fill_var), show.legend = F) +
  scale_fill_gradient(low = "black", high = "white") +
  # contours
  new_scale_fill()+
  # dem
  geom_raster(data = stb_dem_df2, aes(x = longitude, y = latitude,
                                     fill = fill_var),
              alpha = hs_alpha) +
  scale_fill_gradient2(low = "#cfbd9b",
                       mid = "white",
                       high = "#5a8c54",
                       midpoint = mp) +
  # features
  geom_sf(data = howelson_rodeo, fill = "brown", color = "transparent",alpha = 0.3) +
  geom_sf(data = howelson_baseball, fill = "green", color = "transparent",alpha = 0.3) +
  geom_sf(data = stb_sf2, aes(alpha = level), size = 0.1, color = "#476930") +
  geom_sf(data = rivers2, color = "lightblue", fill = "lightblue",
          size = 0.5) +
  geom_sf(data = dt_box, color = "transparent", fill = "#b5ae91", alpha = 0.35) +
  geom_sf(data = dt_build2, fill = "grey70", color = "grey30", size = 0.2) +
  geom_sf(data = snet4, size = 0.235, color = "grey60",
          fill = 'white') +
  geom_sf(data = howelson_lifts, lty = "dashed") +
  geom_sf(data = trails2, color = "white", size = 0.85) +
  geom_sf(data = trails2, color = trail_color, size = 0.75) +
  geom_sf(data = trails2, color = "#6e5e44", size = 0.45, lty = "11") +
  geom_sf(data = wp2, color = "purple") +
  # themes
  scale_x_continuous(expand = c(0.001,0.001)) +
  scale_y_continuous(expand = c(0.001,0.001)) +
  geom_rect(aes(xmin = -106.8625, ymin = 40.4575,
                   xmax =-106.8215, ymax = 40.493),
            fill = "transparent",
            color = dt_inset_color) +
  guides(alpha = "none",
         color= "none",
         fill = "none") +
  theme(rect = element_blank(),
        panel.background = element_rect(fill = "transparent", 
                                        color = NA),
        panel.ontop = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

xmin <- 1
xmax <- 12
ymin <- 1
ymax <- 18.5

# combine
stb_map2 <- 
    ggplot() +
    coord_equal(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
    annotation_custom(ggplotGrob(stb_map), xmin = xmin, xmax = xmax, ymin = ymin,
                      ymax = ymax) +
    cowplot::draw_plot(dt_inset, x = 1, y = 1.75, 
                       width = 8, height = 9)   

# stb_map
y <- abs(round(rnorm(1),5))
ggsave(stb_map2,
       filename = here::here("map", paste0("map_raw",y,".tiff")),
       width = 11,
       height = 17,
       units = "in",
       dpi = 300)



