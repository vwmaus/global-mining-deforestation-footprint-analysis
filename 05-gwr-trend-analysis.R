# Runs a Global Weighted Regression (GWR) on the forest loss time series

library(sf)
library(GWmodel)
library(dplyr)
library(tibble)
library(readr)
library(tidyr)
library(stars)
library(parallel)

# ------------------------------------------------------------------------------
# join datasets
path_forest_loss_all <- "./output/global_mining_and_quarry_forest_loss_20221123a.csv"
path_to_mining_polygons <- "./output/global_mining_and_quarry_20220203.gpkg"

# Define years
na_list <- as.list(rep(0, 19))
names(na_list) <- c(2001:2019)

gwr_data <- read_csv(path_forest_loss_all) |> 
  filter(!is.na(year)) |> 
  filter(year > 2000, year < 2020) |> 
  transmute(id, year, fl = ifelse(is.na(area_forest_loss_000), 0, area_forest_loss_000) - ifelse(is.na(area_forest_loss_025), 0, area_forest_loss_025)) |> 
  arrange(id, year) |> 
  mutate(fl = 100 * fl) |> 
  pivot_wider(names_from = year, values_from = fl) |> 
  left_join(st_read(path_to_mining_polygons) |> st_centroid() |> select(id, geom)) |> 
  arrange(id) |> 
  st_as_sf() |> 
  st_transform(crs = "+proj=igh +ellps=WGS84 +units=m +no_defs") |> 
  replace_na(na_list)

# grid_50 <- st_make_grid(gwr_data, cellsize = 50000) |> 
#   st_as_sf() |> 
#   st_filter(gwr_data) 

# grid_50_ts <- gwr_data |> 
#   select(-id) |> 
#   aggregate(grid_50, sum, na.rm = TRUE) |> 
#   mutate(id = row_number())

gwr_fit_points <- st_make_grid(gwr_data, cellsize = 50000, what = "centers") |> 
  st_as_sf() |> 
  st_filter(gwr_data) |> 
  dplyr::transmute(id, geometry)

gwr_data_ts <- gwr_data |> 
  st_centroid() |> 
  pivot_longer(names_to = "year", values_to = "area", cols = c(-geometry, -id)) |> 
  mutate(year = as.numeric(year)) |> 
  arrange(id, year)

# Find optimal GWR bandwidth - Takes a long time ~24h on 4 cores
if(!file.exists("./output/gwr_bandwidth.rds")){
  bw_val <- GWmodel::bw.gwr(area~year, data = as_Spatial(gwr_grid_50_ts), approach = 'AICc', 
                            kernel = "gaussian", adaptive = FALSE, parallel.method = "omp", parallel.arg = detectCores())
  write_rds(bw_val, file = "./output/gwr_bandwidth.rds")
}
read_rds("./output/gwr_bandwidth.rds")


# Run GWR for fit points on a 50 x 50 km grid only where mining forest loss is present 
# The centroids of mining polygons are used os observations - Processing takes ~10min
if(!file.exists("./output/gwr_res.rds")){
  gwr_res <- GWmodel::gwr.basic(formula = area~year, data = as_Spatial(gwr_grid_50_ts), regression.points = as_Spatial(gwr_fit_points),
                                bw = bw_val, kernel = "gaussian", adaptive = FALSE, 
                                parallel.method = "omp", parallel.arg = detectCores())
  write_rds(gwr_res, file = "./output/gwr_res.rds")
}
read_rds("./output/gwr_res.rds")

gwr_res
