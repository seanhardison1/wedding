library(raster)
library(sf)
library(tidyverse)
library(osmdata)
library(smoothr)

source(file.path("R/query_osm.R"))
load(here::here("data/sbt_dem.rdata"))

# get topography
stb_cont <- rasterToContour(stb_dem, nlevels = 40)
stb_sf <- as(stb_cont, "sf") %>% 
  st_transform(4326) %>% 
  mutate(level = as.numeric(level)) %>% 
  smooth(., method = "ksmooth", smoothness = 4)

stb_dem_df <- stb_dem %>% aggregate(fact = 10) %>% dream::rst_to_tib()

# query OSM
#Query all roads
osm_all_roads <- query_osm(key = "highway", bb = "stb_sf")

osm_paths <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("path","footway","track")) %>% 
  mutate(is_trail = ifelse(str_detect(name, "Trail"), "Trail","Road"))
np_large_roads <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("motorway","unclassified",
                        "secondary","tertiary"))
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
primary <- osm_all_roads$osm_lines %>% 
  filter(highway %in% c("primary"))
trails <- osm_all_roads$osm_lines %>% 
  filter(highway == "path")

lines <- query_osm(key = "power", bb = "stb_sf")
pl <- lines$osm_lines %>% 
  st_transform(4326) %>%  
  dplyr::select(power)


natural <- query_osm(key = "natural", bb = "stb_sf")
natural2 <- query_osm(key = "landuse", bb = "stb_sf")
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

water <- query_osm(key = "water",
          bb = "stb_sf")

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

waterway <- query_osm(key = "waterway",
                   bb = "stb_sf")

stream <- 
  waterway$osm_lines %>% 
  filter(waterway == "stream") %>% 
  dplyr::select(waterway)

#Bridges
bridges <- query_osm(key = "bridge", select = "osm_lines",bb = "stb_sf")

#Accessibility
access <- query_osm(key = "access", select = "osm_polygons",bb = "stb_sf")

stb_map <- 
  ggplot() +
    
  # contours
  geom_raster(data = stb_dem_df, aes(x = longitude, y = latitude, 
                                     fill = fill_var)) +
  # ggsci::scale_fill_material("brown") +
  scale_fill_gradient2(low = "#c6b594", mid = "#b6d7a845", 
                       high = "#b6d7a8", midpoint = 2315) +
  geom_sf(data = stb_sf, aes(alpha = level), size = 0.3, color = "#476930") +
  
  # vegetation
  geom_sf(data = woods, fill = "#476930", alpha = 0.15, color = "transparent") +
  geom_sf(data = woods2, fill = "#476930", alpha = 0.15, color = "transparent") +
  
  # water features
  geom_sf(data = rivers, color = "lightblue", fill = "lightblue",
          size = 0.5) +
  geom_sf(data = lakes, color = "transparent", fill = "lightblue") +
  geom_sf(data = stream, color = "lightblue",
          size = 0.25) +
  
  # trails
  geom_sf(data = trails, color = "#97825f", size = 0.2, alpha = 0.5) +
  geom_sf(data = trails, color = "#6e5e44", size = 0.1, lty = "11") +
  
  # roads
  geom_sf(data = np_large_roads, color = "#000000FF", size = 0.5) +
  geom_sf(data = np_large_roads, color = "white", size = 0.3) +
    
  geom_sf(data = residential, color = "#000000FF", size = 0.4) +
  geom_sf(data = residential, color = "white", size = 0.2) +
  
  geom_sf(data = service, color = "#000000FF", size = 0.4) +
  geom_sf(data = service, color = "white", size = 0.2) +
    
  geom_sf(data = primary_roads, color = "orange",size = 1) + 
  geom_sf(data = primary_roads, color = "white",size = 0.8) + 


  # power lines
  geom_sf(data = pl, color = "grey", alpha = 0.65, size = 0.25) +
  geom_sf(data = pl %>% st_cast("POINT"), color = "grey", alpha = 0.65,
          size = 0.125) +
  
  # figure 
  scale_x_continuous(expand = c(0.001,0.001)) +
  scale_y_continuous(expand = c(0.001,0.001)) +
  guides(alpha = "none",
         color= "none",
         fill = "none") +
  theme(rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "white", color = NA),
        panel.ontop = F,
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  coord_sf(xlim = c(-106.9,-106.725),
           ylim = c(40.30956, 40.5))

ggsave(stb_map,
       filename = here::here("map/map_raw3.pdf"),
       width = 11,
       height = 17,
       units = "in",
       dpi = 200)
