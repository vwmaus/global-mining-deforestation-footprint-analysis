library(stringr)
library(dplyr)
library(sf)
library(progress)
library(tibble)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)
library(foreach)
library(viridis)
library(ggpubr)
library(fasterize)

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
  group_by(id, isoa3, country, ecoregion, biome) |> 
  summarise(across(matches('area_forest_loss'), sum)) 

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

# --------------------------------------------------------------------------------------
# create global 30arcsecond grid (approximately 1 kilometer)
path_to_area_weights <- paste0("./data/mining_area_weights_5arcminute.tif")
system.time(system(paste0("python3 ./python/calculate_area_weights.py \\
                          -i ",path_to_mining_polygons," \\
                          -o ",path_to_area_weights," \\
                          -xmin ",  -180," \\
                          -xmax ",   180," \\
                          -ymin ",   -90," \\
                          -ymax ",    90," \\
                          -ncol ",  4320," \\
                          -nrow ",  2160)))

r <- raster(path_to_area_weights)

plot(r)

crs_goode <- "+proj=igh +ellps=WGS84 +units=m +no_defs"
