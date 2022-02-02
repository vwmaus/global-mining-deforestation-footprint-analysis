library(s2)
library(sf)
library(dplyr)
source("R/s2_union_split_agg.R")
source("R/clean_polygons.R")

# TODO: add datasets to Zenodo
# TODO: add script to download from Zenodo

# Import OpenStreetMap quarry land use 
osm_quarry <- st_read(here::here(data_intermediate, "data/osm_quarry_check_20211125.gpkg")) |> 
  st_set_crs(4326)

# Import mining areas from the Global Resource Use research team 
gru_quarry <- st_read(here::here(data_intermediate, "data/wu_quarry_check_20211125.gpkg")) |> 
  st_set_crs(4326) |>  
  select(geom)

# Integrate datasets 
global_quarry <- bind_rows(wu_quarry, osm_quarry) |> 
  rename(geometry = geom) |> 
  clean_polygons()

st_write(global_quarry, "global_quarry.gpkg", delete_dsn = TRUE)
st_write(global_quarry, "global_quarry.shp", delete_dsn = TRUE)

