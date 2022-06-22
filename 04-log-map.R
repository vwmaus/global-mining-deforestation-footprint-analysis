library(stringr)
library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(scales)
library(viridis)
library(ggpubr)
library(stars)
library(rworldmap)
library(cowplot)

source("R/00_plot_goode_homolosine_world_map.R")

# ------------------------------------------------------------------------------
# set input data version 
data_version <- "20220203"
path_mining_area <- str_c("./data/global_mining_and_quarry_",data_version,".gpkg") 
path_mining_cluster <- str_c("./data/hcluster_concordance_",data_version,".csv")
path_forest_loss <- "./data/global_mining_and_quarry_forest_loss.csv"
path_to_mining_polygons <- "./data/global_mining_and_quarry_20220203.gpkg"

# a<-st_read("./data/global_mining_and_quarry_20220203.gpkg") |> 
#   filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) |> 
#   st_write(path_to_mining_polygons, delete_dsn = TRUE)

mine_features <- st_read(path_to_mining_polygons) |> 
  select(id)

forest_loss <- read_csv(path_forest_loss) |> 
  filter(!is.na(year)) |> 
  filter(year > 2000, year < 2020) |> 
  mutate(year_group = ifelse(year < 2010, "2001-2009", "2010-2019")) |> 
  group_by(id, isoa3, country, ecoregion, biome, year_group) |> 
  summarise(across(matches('area_forest_loss'), sum)) 

forest_loss

mining_cluster <- read_csv(path_mining_cluster) |> 
  select(id, id_hcluster, area, list_of_commodities, type_of_commodities) |> 
  rename(area_mine = area) |> 
  mutate(id = str_remove_all(id, 'A'))

mine_features_areas <- mine_features |> 
  left_join(mining_cluster) |> 
  left_join(forest_loss)

# data check 
mine_features_areas |> 
  st_drop_geometry() |> 
  mutate(Unknown = type_of_commodities == "Unknown") |> 
  group_by(Unknown) |> 
  summarise(area = sum(area_mine)) |> 
  mutate(perc = area / sum(area))

mine_features_areas |> 
  st_drop_geometry() |> 
  mutate(Unknown = type_of_commodities == "Unknown") |> 
  group_by(Unknown) |> 
  summarise(area = sum(area_forest_loss_000, na.rm = TRUE)) |> 
  mutate(perc = area / sum(area))

mine_features_points <- mine_features_areas %>% 
  transmute(fl = ifelse(is.na(area_forest_loss_000), 0, area_forest_loss_000)) %>% 
  mutate(geom = st_centroid(geom))

st_write(mine_features_points, "data/global_mining_cum_forest_loss.gpkg", delete_dsn = TRUE)

mine_features_points_goode <- 
  st_transform(mine_features_points, crs = "+proj=igh +ellps=WGS84 +units=m +no_defs")

grid_50 <- st_make_grid(mine_features_points_goode, cellsize = 50000) |> 
  st_as_sf() |> 
  st_filter(mine_features_points_goode)

grid_50_forest_loss <- mine_features_points_goode |> 
  aggregate(grid_50, sum, na.rm = TRUE) |> 
  filter(fl > 0)


# --------------------------------------------------------------------------------------
# define ggplot theme ------------------------------------------------------------------
textwidth <- 345 # Get from latex comand: \the\textwidth in pt -- divide by 2.835 to mm
textheight <- 550 # Get from latex comand: \the\textheight in pt -- divide by 2.835 to mm
font_size <- 14 # font size in pt 
pt_to_mm <- 2.835
font_family <- "sans"
th <- ggplot2::theme(axis.text = ggplot2::element_text(size = font_size, family = font_family), 
                     text = ggplot2::element_text(size = font_size, family = font_family)) 

W_gp <- plot_goode_homolosine_world_map(ocean_color = "#e5f1f8", land_color = "gray95", family = font_family,
                                        grid_color = "grey60", grid_size = 0.1,
                                        country_borders_color = "grey60", country_borders_size = 0.1) +
  ggplot2::geom_sf(data = grid_50_forest_loss, mapping = aes(fill = fl * 100), color = NA, lwd = 0, size = 0) +
  ggplot2::coord_sf(crs = "+proj=igh", expand = FALSE) +
  # viridis::scale_fill_viridis(option = "viridis", begin = 0, end = 1, direction = -1, discrete = FALSE) +
  viridis::scale_fill_viridis(option = "turbo", begin = 0, end = 1, direction = 1, 
                              discrete = FALSE, 
                              trans = log10_trans(),
                              breaks = c(0.01, 2.5, 400, 60000),
                              labels = function(x) sprintf("%g", x)
                              ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box.spacing = unit(0.0, "cm"),
    legend.key.size = unit(0.3, "cm"),
    legend.key.width = unit(textheight/15, "mm"),
    plot.margin = margin(t = -1, r = -1, b = 0, l = -1, unit = "cm")
  ) +
  labs(fill = bquote(Area~(ha))) + 
  th

ggplot2::ggsave(plot = W_gp, bg = "#ffffff",
                filename = "output/global_forest_loss_mining_area.png",
                width = textwidth, height = 170, units = "mm", scale = 1)
