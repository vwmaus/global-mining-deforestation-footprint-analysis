# --------------------------------------------------------------------------------------
# This script merges OSM and global mining polygons from Maus et al. (2022)
# Input datasets are: 1) polygons from Maus et al. (2022) https://doi.org/10.1594/PANGAEA.942325 and polygon features landuse=quarry from the OSM database
# The resulting dataset is published in Zenodo https://doi.org/10.5281/zenodo.7307210

library(s2)
library(sf)
library(dplyr)
source("R/s2_union_split_agg.R")
source("R/clean_polygons.R")

# --------------------------------------------------------------------------------------
# get world map from Eurostat  ---------------------------------------------------------
if(!file.exists("./data/countries_polygon.gpkg")){
  download.file(url = "https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/countries/download/ref-countries-2016-01m.geojson.zip",
                destfile = "./data/ref-countries-2016-01m.geojson.zip", mode = "w")
  unzip(zipfile = "./data/ref-countries-2016-01m.geojson.zip", files = "CNTR_RG_01M_2016_4326.geojson", exdir = "./data/eurostat", overwrite = TRUE)
  sf::st_read(dsn = "./data/eurostat/CNTR_RG_01M_2016_4326.geojson") %>%
    dplyr::select(ISO3_CODE, COUNTRY_NAME = NAME_ENGL) %>%
    sf::st_make_valid() %>%
    sf::st_cast("POLYGON") %>%
    sf::st_write(dsn = "./data/countries_polygon.gpkg", delete_dsn = TRUE)
}

world_map <- sf::st_read(dsn = "./data/countries_polygon.gpkg")

# --------------------------------------------------------------------------------------
# Import mining datsets 

# Import OpenStreetMap quarry land use 
osm_quarry <- st_read("data/osm_quarry_check_20211125.gpkg") |> 
  st_set_crs(4326)

# Import mining areas from the Global Resource Use research team 
gru_quarry <- st_read("data/wu_quarry_check_20211125.gpkg") |> 
  st_set_crs(4326) |>  
  select(geom)

# Integrate datasets 
system.time(global_quarry <- bind_rows(gru_quarry, osm_quarry) |> 
              rename(geometry = geom) |> 
              clean_polygons())

global_quarry <- mutate(global_quarry, id = stringr::str_pad(id, 7, pad = "0"))

# --------------------------------------------------------------------------------------
# get country names intersecting mining polygons ---------------------------------------
ids_intersects <- sf::st_intersects(global_quarry, world_map)

# polygons with single country intersection
country_names <- world_map %>%
  sf::st_drop_geometry() %>%
  tibble::as_tibble() %>%
  dplyr::slice(unlist(ids_intersects[lengths(ids_intersects) == 1])) %>%
  dplyr::mutate(id = global_quarry$id[lengths(ids_intersects) == 1])

# correct for multiple intersections by keeping only the country with the largest share of the mine in terms of area
ids_multi_mines <- which(lengths(ids_intersects) > 1)
ids_multi_country <- ids_intersects[ids_multi_mines] %>% unlist() %>% unique()
country_names <- global_quarry %>%
  dplyr::slice(ids_multi_mines) %>% 
  sf::st_intersection(world_map[ids_multi_country,]) %>%
  dplyr::mutate(area = sf::st_area(geometry)) %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(id) %>%
  dplyr::top_n(1, area) %>%
  dplyr::ungroup() %>%
  dplyr::select(-area) %>%
  dplyr::bind_rows(country_names)

# correct for missing intersection by selecting the closest country
country_names <- world_map %>%
  dplyr::slice(sf::st_nearest_feature(global_quarry[which(lengths(ids_intersects) < 1),], .)) %>%
  sf::st_drop_geometry() %>%
  tibble::as_tibble() %>%
  dplyr::mutate(id = global_quarry$id[lengths(ids_intersects) < 1]) %>%
  dplyr::bind_rows(country_names)

# --------------------------------------------------------------------------------------
# calculate mining area in km^2 an join country names ----------------------------------
global_quarry <- global_quarry %>%
  dplyr::left_join(country_names) %>%
  dplyr::mutate(area = sf::st_area(geometry) %>% units::set_units("km^2")) |> 
  select(id, isoa3 = ISO3_CODE, country = COUNTRY_NAME, area)

st_write(global_quarry, "./data/global_mining_and_quarry_20220203.gpkg", delete_dsn = TRUE)

global_quarry |> 
  st_drop_geometry() |> 
  as_tibble() |> 
  group_by(isoa3, country) |>
  summarise(area = sum(area)) |> 
  arrange(desc(area))

global_quarry |> 
  st_drop_geometry() |> 
  as_tibble() |> 
  summarise(area = sum(area)) 
