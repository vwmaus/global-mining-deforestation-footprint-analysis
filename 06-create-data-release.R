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

global_mining_forest_loss <- read_csv("data/") 

cluster_ids <- group_by(global_mining_forest_loss, id) |>
    reframe(id_hcluster = unique(id_hcluster), list_of_commodities = unique(list_of_commodities))

global_mining_polygons <- st_read(str_c("data/mining-tree-cover-loss-",gee_version_all,"/mining_features_biomes_20240829a.geojson"))  |>
    left_join(cluster_ids, by = join_by("id")) |>
    relocate(id_hcluster, .after = id) 
    
select(id, cluster_ids, isoa3, country, ecoregion, biome, area, geom = geometry)

select(id, id_hcluster, list_of_commodities, year, all_of(starts_with("area_tree_cover_|area_forest_loss_")))

global_mining_materials_forest_loss <- read_csv(str_c("./output/global_commodity_forest_loss_", gee_version_all, ".csv"))

# write release files
write_csv(global_mining_materials_forest_loss, str_c("./output/data-release-",release_version,"/global_mining_materials_forest_loss.csv"))

write_csv(global_mining_forest_loss, str_c("./output/data-release-",release_version,"/global_mining_forest_loss.csv"))

st_write(global_mining_polygons, str_c("./output/data-release-",release_version,"/global_mining_polygons.gpkg"), layer = "mining_polygons")

