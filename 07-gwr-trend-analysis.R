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
path_forest_loss_all <- "./output/global_mining_and_quarry_forest_loss_20240829a.csv"
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
  left_join(st_read(path_to_mining_polygons, quiet = TRUE) |> select(id, geom)) |>
  arrange(id) |>
  st_as_sf() |>
  st_centroid() |>
  st_transform(crs = "+proj=igh +ellps=WGS84 +units=m +no_defs") |>
  replace_na(na_list) |>
  select(c("id", names(na_list))) # sort columns

gwr_grid_50 <- st_make_grid(gwr_data, cellsize = 50000) |>
  st_as_sf() |>
  st_filter(gwr_data) |>
  rename(geometry = x) |>
  mutate(id_cell = row_number())

gwr_fit_points <- gwr_grid_50 |>
  st_centroid()

gwr_data_ts <- gwr_data |>
  select(-id) |>
  aggregate(gwr_grid_50, sum, na.rm = TRUE) |>
  st_join(gwr_fit_points, join = st_intersects) |>
  st_centroid() |>
  pivot_longer(names_to = "year", values_to = "area", cols = c(-id_cell, -geometry)) |>
  mutate(year = as.numeric(year)) |>
  arrange(id_cell, year)

# Find optimal GWR bandwidth for 50 x 50 km grid only where mining forest loss is present- ~28h on 12 cores
if(!file.exists("./output/gwr_bandwidth.rds")){
  system.time(
    bw_val <- bw.gwr(formula = area~year, data = as_Spatial(gwr_data_ts), approach = 'AICc',
                     kernel = "gaussian", adaptive = FALSE,
                     parallel.method = "omp", parallel.arg = detectCores())
    )
  write_rds(bw_val, file = "./output/gwr_bandwidth.rds")
}
bw_val <- read_rds("./output/gwr_bandwidth.rds")
bw_val

# Run GWR for fit points on a 50 x 50 km grid only where mining forest loss is present - ~4min on 12 cores
if(!file.exists("./output/gwr_res.rds")){
  system.time(
    gwr_res <- gwr.basic(formula = area~year, data = as_Spatial(gwr_data_ts), regression.points = as_Spatial(gwr_fit_points),
                         kernel = "gaussian", adaptive = FALSE, bw = bw_val,
                         parallel.method = "omp", parallel.arg = detectCores())
  )
  write_rds(gwr_res, file = "./output/gwr_res.rds")
}
gwr_res <- read_rds("./output/gwr_res.rds")

gwr_res

