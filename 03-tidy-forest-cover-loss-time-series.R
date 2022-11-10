# BEFORE RUNNING THIS SCRIPT
#   1. UPLOAD global_mining_and_quarry_20220203.gpkg to Google Earth Engine GEE platform
#   2. Run the script 01-gee-calculate-tree-cover-loss.js on GEE platform 
#   3. Download all files from Google drive folder "GEE" to the "./data/mining-tree-cover-loss-<version>"
#   4. The script below will tidy the tree cover loss time series coming from GEE and add commodities
# 
# The datasets generated in this script are published in Zenodo https://doi.org/10.5281/zenodo.7299103

library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(units)
library(progress)
library(units)
library(readr)

# Replace to process a different version
gee_version <- "20220203"

# ------------------------------------------------------------------------------
# get forest loss files 
forest_loss_path <- 
  dir(str_c("./data/mining-tree-cover-loss-",gee_version), 
      pattern = "tree_cover_loss_mines_", full.names = TRUE)

# ------------------------------------------------------------------------------
# tidy forest loss files
out <- tibble::tibble(id = character(), year = double())
for(f in forest_loss_path){
  
  print(str_c("Processing ", f))
  
  tree_cover <- str_remove_all(basename(f), "tree_cover_loss_mines_") %>%
    str_remove_all(str_c("_", gee_version, ".csv"))
  
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
    dplyr::select(id, year, area) |> 
    dplyr::mutate(id = str_pad(id, 7, "0", side = 'left')) |> # correct ids when not char
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
mines_gee <- dir(str_c("./data/mining-tree-cover-loss-",gee_version), pattern = ".geojson", full.names = TRUE) |> 
  st_read(quiet = TRUE) |> 
  dplyr::mutate(id = str_pad(id, 7, "0", side = 'left')) # correct ids when not char

mines_gee <- mines_gee |> 
  st_drop_geometry() |>
  as_tibble() |>
  select(id, isoa3, country, ecoregion, biome) |> 
  right_join(out)

# ------------------------------------------------------------------------------
# add commodities
if(file.exists(str_c("./data/hcluster_concordance_",gee_version,".csv"))){
  mines_gee <- read_csv(str_c("./data/hcluster_concordance_",gee_version,".csv")) |> 
    select(id, id_hcluster, area_mine = area, list_of_commodities) |> 
    mutate(id = str_remove_all(id, 'A')) |> 
    left_join(mines_gee)
} else {
  print("WARNING: Commodities cluster is missing. Results will be saved without commodities.")
}
readr::write_csv(mines_gee, str_c("./output/global_mining_and_quarry_forest_loss_",gee_version,".csv"))
