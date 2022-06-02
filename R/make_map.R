library(raster)
library(sf)
library(tidyverse)
library(osmdata)
library(smoothr)
library(sfnetworks)

source(file.path("R/query_osm.R"))
load(here::here("data/sbt_dem.rdata"))
ncrs <- st_crs("+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs")

# get topography
stb_cont <- rasterToContour(stb_dem, nlevels = 40)
stb_sf <- as(stb_cont, "sf") %>% 
  st_transform(4326) %>% 
  mutate(level = as.numeric(level)) %>% 
  smooth(., method = "ksmooth", smoothness = 4)

stb_dem_df <- stb_dem %>% aggregate(fact = 10) %>% dream::rst_to_tib()

downtown <- st_sf(geom = st_sfc(st_point(c(-106.845,40.48)),
                                st_point(c(-106.82, 40.495))),
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

  save(osm_all_roads,
       lines,
       natural,
       natural2,
       bridges,
       access,
       waterway,
       dt_build,
       water,
       away,
       file = here::here("data/osm_data.rdata"))
} else {
  load(here::here("data/osm_data.rdata"))
}



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
  filter(highway %in% c("service"))
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

# snet_ls <- bind_rows(snet_ls1,
#                      snet_ls2,
#                      snet_ls3) %>% 
#   st_cast("POINT")
# 
# snet_p <- 
#   snet3 %>% 
#   st_cast("POINT")
# 
# td <- sf::st_nearest_feature(snet_ls,snet_p) 
# dist = as.numeric(st_distance(snet_ls, snet_p[td,], by_element=TRUE))
# 
# snet4 <- 
#   snet3 %>% 
#   st_join(snet_ls %>% 
#             mutate(dist = dist)) %>% 
#   cbind(.,snet_ls %>% 
#           mutate(dist = dist))
# 
# ggplot() +
#   geom_sf(data = snet4 %>% dplyr::select(dist,geometry.1),
#           aes(color = dist))


# small_roads <- 
#   sfnetworks::st_network_join(sfnetworks::as_sfnetwork(service),
#                               sfnetworks::as_sfnetwork(residential)) %>%
#   st_as_sf("edges") %>% 
#   # st_crop(ymin = 40.485, ymax = 40.49,
#   #         xmin = -106.84, xmax = -106.83) %>%
#   dplyr::select(highway) %>% 
#   st_transform(ncrs) %>% 
#   st_intersection(.,snet3)


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

stb_dem_df2 <- 
  stb_dem %>% 
  # aggregate(fact = 10) %>% 
  raster::crop(extent(downtown)) %>% 
  projectRaster(.,crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  dream::rst_to_tib()

brd <- tibble(x1 = min(stb_dem_df2$longitude),
              x2 = max(stb_dem_df2$longitude),
              y1 = min(stb_dem_df2$latitude),
              y2 = max(stb_dem_df2$latitude))


build <- function(){
  stb_map <-
  ggplot() +
    
  # contours
  geom_raster(data = stb_dem_df, aes(x = longitude, y = latitude,
                                     fill = fill_var)) +
  # ggsci::scale_fill_material("brown") +
  
  scale_fill_gradient2(low = "#c6b594",
                       high = "#558050",
                       # midpoint = 2050) +
                       midpoint = 1800) +
    # coord_sf(ylim = c(40.48, 40.495),
    #          xlim = c(-106.841, -106.8225))
  # = "#7e9e70",
  geom_sf(data = stb_sf, aes(alpha = level), size = 0.1, color = "#476930") +
  
  # vegetation
  # geom_sf(data = woods, fill = "#476930", alpha = 0.15, color = "transparent") +
  # geom_sf(data = woods2, fill = "#476930", alpha = 0.15, color = "transparent") +C
  
  # water features
  geom_sf(data = rivers, color = "lightblue", fill = "lightblue",
          size = 0.5) +
  geom_sf(data = lakes, color = "transparent", fill = "lightblue") +
  geom_sf(data = stream, color = "lightblue",
          size = 0.25) +
  
  # trails
  geom_sf(data = trails, color = "white", size = 0.5) +
  # geom_sf(data = trails, color = "#97825f", size = 0.4, alpha = 0.5) +
  geom_sf(data = trails, color = "purple", size = 0.4, alpha = 0.5) +
  geom_sf(data = trails, color = "#6e5e44", size = 0.3, lty = "11") +
  
  # roads
  # geom_sf(data = snet2, 
  #         color = "black",
  #         fill = "black",
  # #         size = 0.5) +
  # geom_sf(data = snet3, size = 0.1, fill = "black") +
  # geom_sf(data = snet3, size = 0.3, fill = "white") +


  # geom_sf(data = small_roads, size = 0.05, color = "grey") +
  # geom_sf(data = snet3, size = 0.05, color = "grey90",
  #         fill = 'grey30') +
  geom_sf(data = snet3, size = 0.025, color = "grey80",
          fill = 'grey20') +

  # power lines
  geom_sf(data = pl, color = "grey", alpha = 0.9, size = 0.25) +
  geom_sf(data = pl %>% st_cast("POINT"), color = "grey", alpha = 0.9,
          size = 0.125) +
  
  # inset
  geom_rect(data = brd,
            aes(xmin = -106.842, ymin = 40.48,
                xmax =-106.8215, ymax = 40.495),
            fill = "transparent",
            color = "#948663") +
  
  # mountains
  # geom_sf_text(data = pks, aes(label = name),
  #              angle = -90) +
  
  
  # figure 
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
  coord_sf(xlim = c(-106.9,-106.725),
           ylim = c(40.30956, 40.5))
  

# downtown inset----
dt_build2 <- dt_build$osm_polygons %>% dplyr::select(geometry)%>% 
  st_crop(downtown)
sf::sf_use_s2(FALSE)
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



dt_inset <-
  ggplot() +
  # water features
  geom_raster(data = stb_dem_df2, aes(x = longitude, y = latitude,
                                     fill = fill_var)) +
  scale_fill_gradient2(low = "#c6b594",
    # low = "#b3a07d",
    high = "#a6cfa1",
    # midpoint = 2050) +
    midpoint = 2050) +
  geom_sf(data = stb_sf2, aes(alpha = level), size = 0.1, color = "#476930") +
  geom_sf(data = rivers2, color = "lightblue", fill = "lightblue",
          size = 0.5) +
  geom_sf(data = dt_box, color = "transparent", fill = "#b5ae91", alpha = 0.35) +
  geom_sf(data = dt_build2, fill = "grey70", color = "grey60", size = 0.2) +
  geom_sf(data = snet4, size = 0.235, color = "grey",
          fill = 'white') +
  geom_sf(data = howelson_lifts, lty = "dashed") +
  # trails
  geom_sf(data = trails2, color = "white", size = 0.5) +
  geom_sf(data = trails2, color = "#97825f", size = 0.4, alpha = 0.5) +
  geom_sf(data = trails2, color = "#6e5e44", size = 0.3, lty = "11") +
  scale_x_continuous(expand = c(0.001,0.001)) +
  scale_y_continuous(expand = c(0.001,0.001)) +
  geom_rect(data = brd,
               aes(xmin = -106.842, ymin = 40.48,
                   xmax =-106.8215, ymax = 40.495),
            fill = "transparent",
            color = "#948663") +
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
  coord_sf(ylim = c(40.48, 40.495),
           xlim = c(-106.841, -106.8225))


# dt_inset
stb_map2 <- 
  ggplot() +
  coord_equal(xlim = c(1, 11), ylim = c(1, 17), expand = FALSE) +
  annotation_custom(ggplotGrob(stb_map), xmin = 1, xmax = 11, ymin = 1,
                    ymax = 17) +
  cowplot::draw_plot(dt_inset, x = 1, y = 8.5, 
                     width = 4, height = 4) 

  # geom_segment(aes(x = 18.6, y = 6,
  #                  xend = 15, yend = 5),
  #              arrow = arrow(length = unit(0.15, "inches")), lineend = "butt",
  #              linejoin = "mitre") +
  # geom_text(data = landmarks,
  #           aes(x = x,
  #               y = y ,
  #               label = name),
  #           color = "grey20",
  #           size = 5.5)+
  # theme_void()

# stb_map
y <- abs(round(rnorm(1),5))
ggsave(stb_map2,
       filename = here::here("map", paste0("map_raw",y,".pdf")),
       width = 13,
       height = 20,
       units = "in",
       dpi = 200)
}
build()

