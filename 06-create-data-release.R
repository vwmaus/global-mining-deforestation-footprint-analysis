library(sf)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(rnaturalearth)
library(rnaturalearthdata)

release_version <- "v3"
dir.create(str_c("./output/data-release-", release_version), recursive = TRUE)

# Reade source files
mine_forest_loss <- read_csv("./output/global_mining_forest_loss_2000-2023.csv")

mine_polygons <- st_read("./data/20260325-all_materials/mine_polygons.gpkg")

# Add spatial extensions
sf_use_s2(FALSE)

if(!file.exists("./ecoregions/Ecoregions2017.shp")){
    download.file("https://storage.googleapis.com/teow2016/Ecoregions2017.zip", destfile = "./data/Ecoregions2017.zip")
    unzip("./data/Ecoregions2017.zip", exdir = "./data/ecoregions/")
}

biomes <- st_read("./data/ecoregions/Ecoregions2017.shp") |>
    select(biome_name = BIOME_NAME)

world_map <- ne_countries(scale = "medium", returnclass = "sf") |>
    select(country_name = admin, country_isoa3 = adm0_a3)

extensions_tbl <- st_centroid(mine_polygons) |>
    st_join(world_map, join = st_nearest_feature) |>
    st_join(biomes, join = st_nearest_feature) |>
    st_drop_geometry() |>
    as_tibble() |>
    select(id, id_cluster, country_name, country_isoa3, biome_name, primary_materials_list, materials_list, area_mine, data_source)

# Write data release files
names(mine_forest_loss)
write_csv(mine_forest_loss, str_c("./output/data-release-",release_version,"/mine_forest_loss.csv"))

names(extensions_tbl)
write_csv(extensions_tbl, str_c("./output/data-release-",release_version,"/extensions_tbl.csv"))
