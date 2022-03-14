library(raster)
library(sf)
library(tidyverse)
library(osmdata)
library(smoothr)

source(file.path("R/query_osm.R"))
load(here::here("data/sbt_dem.rdata"))

# get topography
sbt_cont <- rasterToContour(sbt_dem, nlevels = 40)
sbt_sf <- as(sbt_cont, "sf") %>% 
  st_transform(4326) %>% 
  mutate(level = as.numeric(level)) %>% 
  smooth(., method = "ksmooth", smoothness = 4)

plot(sbt_sf)
# query OSM
#Query all roads
osm_all_roads <- query_osm(key = "highway", geo = "osm_lines",
                           crop = F, bb = "sbt_sf")

osm_paths <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("path","footway","track")) %>% 
  mutate(is_trail = ifelse(str_detect(name, "Trail"), "Trail","Road"))

large_roads <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("motorway","trunk","primary","secondary","tertiary"))


tertiary <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("tertiary"))  


service <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("service"))
secondary <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("secondary"))
primary <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("primary"))
natural <- query_osm(key = "natural", bb = "sbt_sf")

water <- natural$osm_polygons %>% 
  st_transform(4326) %>% 
  filter(natural %in% c("water","wetland")) %>% 
  dplyr::select(natural)

water <- query_osm(key = "water",
          bb = "sbt_sf")

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

#Bridges
bridges <- query_osm(key = "bridge", geo = "osm_lines")

#Accessibility
access <- query_osm(key = "access", geo = "osm_polygons")

ggplot() +
  geom_sf(data = sbt_sf, aes(alpha = level), size = 0.3) +
  geom_sf(data = rivers, color = "lightblue") +
  geom_sf(data = lakes, color = "transparent", fill = "lightblue") +
  geom_sf(data = large_roads, color = "#000000FF", size = 0.4) +
  scale_x_continuous(expand = c(0.001,0.001)) +
  scale_y_continuous(expand = c(0.001,0.001)) +
  guides(alpha = F,
         color= F) +
  theme(rect = element_rect(fill = "transparent"),
        # panel.grid.major = element_line(color = "grey50", linetype = 1),
        panel.background = element_rect(fill = "white", color = NA),
        panel.ontop = F,
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  coord_sf(xlim = c(-106.9,-106.725),
           ylim = c(40.30956, 40.5))

ggsave(device = "png", 
       width = 4 ,
       height = 3.5,
       units = "in",
       dpi = 300, filename = here::here("beer_map.png"))
