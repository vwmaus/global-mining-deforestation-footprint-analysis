library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(units)
library(progress)
library(units)

# ------------------------------------------------------------------------------
# get forest loss files 
forest_loss_path <- 
  dir("./data/gee_platform_forest_loss_20220203", 
      pattern = "tree_cover_loss_mines_", full.names = TRUE)

# ------------------------------------------------------------------------------
# tidy forest loss files
out <- tibble::tibble(id = character(), year = double())
for(f in forest_loss_path){
  
  print(str_c("Processing ", f))
  
  tree_cover <- str_remove_all(basename(f), "tree_cover_loss_mines_") %>%
    str_remove_all(str_c("_", data_version, ".csv"))
  
  mines_gee <- read_csv(f, show_col_types = FALSE) |> 
    select(id, forest_loss = groups) |> 
    separate_rows(forest_loss, sep="\\},\\s*") |> 
    mutate(forest_loss = forest_loss |> 
             str_remove_all("\\}") |> 
             str_remove_all("\\{") |> 
             str_remove_all("\\[") |> 
             str_remove_all("\\]")) |> 
    mutate(year = as.numeric(str_extract(forest_loss, pattern = "(?<=group\\=).+(?=,)")) + 2000,
           area = as.numeric(str_extract(forest_loss, pattern = "(?<=sum\\=).+(?=$)")),
           area = set_units(set_units(area, m^2), km^2)) |> 
    dplyr::arrange(id, year) |> 
    dplyr::select(id, year, area)|> 
    dplyr::rename(!!paste0("area_forest_loss_", tree_cover) := area)
  
  out <- dplyr::full_join(out, mines_gee, by = c("id" = "id", "year" = "year"))
  
}

# ------------------------------------------------------------------------------
# check forest loss cover area
dplyr::summarise_all(select(out, -id, -year), sum, na.rm = TRUE) |> 
  select(area_forest_loss_000, area_forest_loss_025, area_forest_loss_050,
         area_forest_loss_075, area_forest_loss_100)

# ------------------------------------------------------------------------------
# add attributes 
mines_gee <- st_read("./data/gee_platform_forest_loss_20220203/mining_features_20220203.geojson", quiet = TRUE)

mines_gee |> 
  st_drop_geometry() |>
  as_tibble() |>
  select(id, isoa3, country, ecoregion, biome) |> 
  right_join(out) |>
  readr::write_csv("./data/global_mining_and_quarry_forest_loss.csv")




