# adapted from https://wilkelab.org/practicalgg/articles/goode.html
plot_goode_homolosine_world_map <- function(ocean_color = "#56B4E950", 
                                            land_color = "gray95",
                                            grid_color = "grey60",
                                            grid_size = 0.1,
                                            country_borders_color = "grey60",
                                            country_borders_size = 0.1,
                                            family = "sans"){

  crs_goode <- "+proj=igh +ellps=WGS84 +units=m +no_defs"
  
  world_sf <- sf::st_as_sf(rworldmap::getMap(resolution = "low")) %>% 
    sf::st_transform(crs = crs_goode)
  
  # projection outline in long-lat coordinates
  lats <- c(
    90:-90, # right side down
    -90:0, 0:-90, # third cut bottom
    -90:0, 0:-90, # second cut bottom
    -90:0, 0:-90, # first cut bottom
    -90:90, # left side up
    90:0, 0:90, # cut top
    90 # close
  )
  longs <- c(
    rep(180, 181), # right side down
    rep(c(80.01, 79.99), each = 91), # third cut bottom
    rep(c(-19.99, -20.01), each = 91), # second cut bottom
    rep(c(-99.99, -100.01), each = 91), # first cut bottom
    rep(-180, 181), # left side up
    rep(c(-40.01, -39.99), each = 91), # cut top
    180 # close
  )
  
  goode_outline <- 
    list(cbind(longs, lats)) %>%
    sf::st_polygon() %>%
    sf::st_sfc(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>% 
    sf::st_transform(crs = crs_goode)
  
  # get the bounding box in transformed coordinates and expand by 10%
  xlim <- st_bbox(goode_outline)[c("xmin", "xmax")]*1.1
  ylim <- st_bbox(goode_outline)[c("ymin", "ymax")]*1.1
  
  # turn into enclosing rectangle
  goode_encl_rect <- 
    list(
      cbind(
        c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]), 
        c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1])
      )
    ) %>%
    sf::st_polygon() %>%
    sf::st_sfc(crs = crs_goode)

  goode_without <- sf::st_difference(goode_encl_rect, goode_outline)
  
  y_grid <- lapply(seq(-80, 90, 20)   + 2, function(y) sf::st_point(c(longitude = -175, latitude =   y)))
  x_grid <- lapply(seq(-150, 150, 50) - 9, function(x) sf::st_point(c(longitude =    x, latitude = -62)))
  
  # goode_grid <- sf::st_as_sfc(c(y_grid, x_grid), crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  #   sf::st_as_sf() %>% 
  #   dplyr::mutate(label = c(paste0(abs(seq(-80, 90, 20)),   "°", c(rep("S", 4), "", rep("N", 4))), 
  #                           paste0(abs(seq(-160, 180, 20)), "°", c(rep("W", 8), "", rep("E", 8))))) %>% 
  #   sf::st_transform(crs = crs_goode)
  
  goode_grid <- sf::st_as_sfc(c(y_grid, x_grid), crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
    sf::st_as_sf() %>% 
    dplyr::mutate(label = c(paste0(abs(seq(-80, 90, 20)),   "°", c(rep("S", 4), "", rep("N", 4))), 
                            paste0(abs(seq(-150, 150, 50)), "°", c(rep("W", 3), "", rep("E", 3))))) %>% 
    sf::st_transform(crs = crs_goode)
  
  gp_map <- ggplot(world_sf) + 
    theme_minimal_grid() +
    geom_sf(fill = land_color, color = country_borders_color, size = country_borders_size/.pt) +
    geom_sf(data = goode_without, fill = "white", color = "NA") +
    coord_sf(crs = crs_goode, xlim = 0.95*xlim, ylim = 0.95*ylim, expand = FALSE) +
    geom_sf_text(data = goode_grid, colour = grid_color, size = 3, family = family,
                 mapping = aes(label = label)) + 
    theme(
      axis.title = element_blank(),
      panel.background = element_rect(fill = ocean_color, color = NA, linewidth = 2),
      panel.grid.major = element_line(color = grid_color, linewidth = grid_size)
    )
  gp_map
  return(gp_map)
  
}
