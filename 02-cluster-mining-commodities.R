library(stringr)
library(dplyr)
library(sf)
library(progress)
library(tibble)
library(readr)
library(tidyr)
library(fastcluster)
library(parallel)
library(foreach)
library(doParallel)

# ------------------------------------------------------------------------------
# set input data version 
data_version <- "20220203"
path_mining_area <- str_c("./data/global_mining_and_quarry_",data_version,".gpkg") 
path_mining_commodities <- "./data/snl/mining_commodities.gpkg"
path_clusting_dataset <- str_c("./data/snl/clustering_dataset_", data_version, ".gpkg")

# ------------------------------------------------------------------------------
# set cluster 
cl <- makeCluster(parallel::detectCores(), type = "FORK")
registerDoParallel(cl)

# ------------------------------------------------------------------------------
# prepare datasets for clustering
create_clusting_dataset <- function(){
  st_read(path_mining_area, quiet = TRUE) |> 
    mutate(id = str_c("A", id)) |> 
    # The dataset below was extracted from the SNL database which requires license
    bind_rows(st_read(path_mining_commodities, quiet = TRUE) |> 
                mutate(id = str_c("C", str_pad(id, width = 7, pad = 0), dataset = "commodities"))) |> 
    st_write(path_clusting_dataset, layer = "mining_features", delete_dsn = TRUE, quiet = TRUE)
  return(NULL)
}
create_clusting_dataset()

isoa3_list <- 
  st_read(dsn = path_clusting_dataset, quiet = TRUE,
          query = "SELECT isoa3 FROM mining_features") |> 
  as_tibble() |> 
  distinct() |> 
  arrange(isoa3)

# ------------------------------------------------------------------------------
# calculate distance matrix per country in parallel
source("./R/calc_dist_matrix.R")
pb <- progress_bar$new(
  format = "  downloading [:bar] :percent in :elapsed",
  total = length(isoa3_list$isoa3), clear = FALSE, width= 60)
dist_files <- foreach(
  mine_split = isoa3_list$isoa3, 
  .combine = 'c'
) %do% {
  calc_dist_matrix(
    x = mine_split, 
    in_path = path_clusting_dataset,
    layer = "mining_features",
    split_att = "isoa3", 
    output_dir = str_c("./data/dist_matrix_", data_version),
    pb = pb)
}

# ------------------------------------------------------------------------------
# hcluster polygons and SNL coordinates
h <- units::set_units(10000, m)
dist_files <- dir(str_c("./data/dist_matrix_", data_version), full.names = TRUE)
names(dist_files) <- stringr::str_remove_all(basename(dist_files), ".rds")
mine_clusters <- foreach(
  f = isoa3_list$isoa3, 
  .combine = 'bind_rows'
) %dopar% {
  
  id_hcluster <- 1
  
  out <- 
    st_read(dsn = path_clusting_dataset, 
            query = str_c("SELECT id, isoa3, country, area, list_of_commodities FROM mining_features WHERE isoa3 = \"", f, "\""), 
            quiet = TRUE) |>
    as_tibble() |> 
    mutate(id_hcluster = id_hcluster, .after = id)
  
  if(nrow(out)>1){
    id_hcluster <- readRDS(dist_files[[f]]) |> 
      fastcluster::hclust(method = "single") |> 
      cutree(h = as.numeric(units::set_units(h, m)))
  }
  
  out$id_hcluster <- id_hcluster
  
  out
  
}

# create cluster ids concordance
source("./R/fun_collapse_groups.R")
hcluster_concordance <- mine_clusters |> 
  group_by(id_hcluster, isoa3) |>
  mutate(id_hcluster = str_c("H", str_pad(cur_group_id(), pad = "0", width = 7)),
         list_of_commodities = fun_collapse_groups(list_of_commodities)) |>
  ungroup() |> 
  filter(str_detect(id, "A"))

sum(mine_clusters$area, na.rm = TRUE)
sum(hcluster_concordance$area)

hcluster_concordance |> 
  mutate(comm = is.na(list_of_commodities)) |> 
  group_by(comm) |> 
  summarise(area = sum(area))

hcluster_concordance <- hcluster_concordance |> 
  mutate(type_of_commodities = ifelse(is.na(list_of_commodities), "Unknown", 
                                      ifelse(str_detect(list_of_commodities, "Coal"), 
                                             ifelse(str_detect(list_of_commodities, ","), "Coal+Metals", "Coal"), "Metals"))) 

write_csv(hcluster_concordance, str_c("./data/hcluster_concordance_",data_version,".csv"))

hcluster_concordance |> 
  group_by(type_of_commodities) |> 
  summarise(area = sum(area)) |> 
  mutate(perc = area / sum(area))

