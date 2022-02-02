#' Process polygons by:
#' - Removing empty features
#' - Trying to make features valid
#' - Trying to correct bad polygons
#' - Performing polygons union using s2 model = 'closed' on intersecting polygons to remove double counting
#' 
#' @param x A sf object with POLYGON or MULTIPOLYGON features. Other types will be removed
#' @return Returns a sf object of valid POLYGON features free of intersections
clean_polygons <- function(x){
  
  sf::sf_use_s2(TRUE)
  
  message(paste("x has", nrow(x), "features"))
  message(paste("removing features different from POLYGON or MULTIPOLYGON"))
  message(paste("casting MULTIPOLYGON to POLYGON"))
  x <- dplyr::select(x, geometry) %>% 
    sf::st_make_valid(oriented = FALSE, s2_options = s2::s2_options(snap = s2::s2_snap_precision(1e+07))) %>% 
    dplyr::filter(!sf::st_is_empty(geometry)) %>% 
    dplyr::filter(sf::st_is(geometry, type = "POLYGON") | sf::st_is(geometry, type = "MULTIPOLYGON")) %>% 
    sf::st_cast("POLYGON") 
  
  message(paste("trying to fix bad polygons"))
  fixed_bad_polygons <- dplyr::filter(x, !sf::st_is_valid(geometry)) %>%
    sf::st_transform("+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs") %>%  
    sf::st_simplify() %>% 
    sf::st_transform(sf::st_crs(x)) %>% 
    sf::st_cast("POLYGON") %>% 
    dplyr::filter(sf::st_is_valid(geometry))
  
  x <- dplyr::filter(x, sf::st_is_valid(geometry)) %>%
    dplyr::bind_rows(fixed_bad_polygons)
  
  message(paste("x has now", nrow(x), "valid features"))
  
  message(paste("performing union transformation"))
  united_x <- sf::st_as_s2(x) %>% 
    s2_union_split_agg(options = s2_options(model = "closed")) %>% 
    sf::st_as_sf() %>% 
    sf::st_cast("POLYGON")
  
  message(paste("trying to fix bad polygons created by st_union"))
  fixed_bad_polygons <- united_x %>% 
    dplyr::filter(!sf::st_is_valid(geometry)) %>%
    sf::st_make_valid(x, oriented = FALSE, s2_options = s2::s2_options(snap = s2::s2_snap_precision(1e+07))) %>% 
    sf::st_transform("+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs") %>% 
    sf::st_simplify() %>% 
    sf::st_transform(sf::st_crs(x)) %>% 
    sf::st_cast("POLYGON") %>% 
    dplyr::filter(sf::st_is_valid(geometry))
  
  # gathering results into single object and fill small polygon holes
  result <- dplyr::filter(united_x, sf::st_is_valid(geometry)) %>% 
    dplyr::bind_rows(fixed_bad_polygons) %>% 
    smoothr::fill_holes(units::set_units(1, ha))
  
  message(paste("returning polygons"))
  return(result)
  
}