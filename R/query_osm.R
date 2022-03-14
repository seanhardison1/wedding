query_osm <- function(key, value = NULL, bb){
  
  if (is.null(value)){
    group_shapes <- opq(bbox = st_bbox(get(bb))) %>%
      add_osm_feature(key = key, value = NULL) %>%
      osmdata_sf()
  } else {
    group_shapes <- opq(bbox = st_bbox(get(bb))) %>%
      add_osm_feature(key = key, value = value) %>%
      osmdata_sf()
  }
  
  if (!is.null(group_shapes)) message("Query successful")
  return(group_shapes)
}
