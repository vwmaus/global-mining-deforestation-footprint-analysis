library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(sf)

gee_version_all <- "20240829a" # Maus et al. + OSM
release_version <- "v2"
dir.create(str_c("./output/data-release-", release_version), recursive = TRUE)

# reade source files
global_mining_forest_loss <- read_csv(str_c("./output/global_mining_and_quarry_forest_loss_", gee_version_all, ".csv"))

cluster_ids <- group_by(global_mining_forest_loss, id) |>
    reframe(id_hcluster = unique(id_hcluster), list_of_commodities = unique(list_of_commodities))

global_mining_polygons <- st_read(str_c("data/mining-tree-cover-loss-",gee_version_all,"/mining_features_biomes_20240829a.geojson"))  |>
    left_join(cluster_ids, by = join_by("id")) |>
    select(id, id_hcluster, isoa3, country, area = mine_area, list_of_materials = list_of_commodities, ecoregion, biome, geom = geometry) |>
    mutate(ecoregion = ifelse(ecoregion == "N/A", NA, ecoregion),
           biome = ifelse(biome == "N/A", NA, biome))

global_mining_forest_loss <- select(global_mining_forest_loss, id, year, isoa3, starts_with("area_tree_cover_"), starts_with("area_forest_loss_"))

global_mining_materials_forest_loss <- read_csv(str_c("./output/global_commodity_forest_loss_", gee_version_all, ".csv")) |>
    select(year, isoa3, material_name, ids = id, starts_with("area_forest_loss_"))

# write release files
names(global_mining_polygons)
st_write(global_mining_polygons, str_c("./output/data-release-",release_version,"/global_mining_polygons.gpkg"), layer = "mining_polygons")

names(global_mining_forest_loss)
write_csv(global_mining_forest_loss, str_c("./output/data-release-",release_version,"/global_mining_forest_loss.csv"))

names(global_mining_materials_forest_loss)
write_csv(global_mining_materials_forest_loss, str_c("./output/data-release-",release_version,"/global_mining_materials_forest_loss.csv"))

