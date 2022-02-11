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

# ------------------------------------------------------------------------------
# prepare datasets for clustering
path_clusting_dataset <- str_c("./data/snl/clustering_dataset_", data_version, ".gpkg")
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

# ------------------------------------------------------------------------------
# declare function to compute distance matrices for each country
calc_dist_matrix <-
  function(x,
           in_path,
           split_att,
           layer,
           output_dir = ".",
           pb = NULL) {
    
    if(!is.null(pb)) pb$tick()
    
    gc()
    
    path_features_dist_meter <- 
      stringr::str_glue("{output_dir}/{x}.rds")
    
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    
    # stop processing if job has less than two features ------------------------
    x <- st_read(
      dsn = in_path, 
      query = str_c("SELECT id, geom FROM ", layer, " WHERE isoa3 = \"", x, "\""), 
      quiet = TRUE)
    
    if( nrow(x) < 2 ){
      return(NULL)
    }
    
    # compute geographical distance in parallel --------------------------------
    if(!file.exists(path_features_dist_meter)){
      
      ids <- x$id
      x <- sf::st_geometry(x)
      dist_matrix <- foreach(
        xi = split(x, 1:length(x)), 
        .combine = 'rbind'
      ) %dopar% {
        sf::st_distance(x = xi, y = x)
      }
      
      row.names(dist_matrix) <- ids
      saveRDS(as.dist(dist_matrix), file = path_features_dist_meter)
      
    }
    
    return(path_features_dist_meter)
    
}

# ------------------------------------------------------------------------------
# calculate distance matrix per country in parallel
cl <- makeCluster(parallel::detectCores(), type = "FORK")
registerDoParallel(cl)
isoa3_list <- 
  st_read(dsn = path_clusting_dataset, quiet = TRUE,
          query = "SELECT isoa3 FROM mining_features") |> 
  as_tibble() |> 
  distinct() |> 
  arrange(isoa3)
pb <- progress_bar$new(
  format = "  downloading [:bar] :percent in :elapsed",
  total = length(isoa3_list$isoa3), clear = FALSE, width= 60)
dist_matrix_paths <- foreach(
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

# TODO: test hclust 

h <- units::set_units(10000, m)
dist_files <- dir(glue::glue(derived_data_path, "/dist_matrix"), full.names = TRUE)
names(dist_files) <- stringr::str_remove_all(basename(dist_files), ".rds")
mine_clusters <- parallel::mclapply(names(mine_split), function(f){
  hcluster <- 1
  dbcluster <- 1
  if(f %in% names(dist_files)){
    dist_matrix <- readRDS(dist_files[[f]])
    hcluster <- fastcluster::hclust(dist_matrix, method = "single")
    %>% 
      cutree(h = as.numeric(units::set_units(h, m)))
    dbcluster <- dbscan::dbscan(dist_matrix, eps = as.numeric(h),
                                minPts = 1)$cluster
  }
  mine_split[[f]] %>% 
    sf::st_drop_geometry() %>% 
    tibble::as_tibble() %>% 
    dplyr::transmute(hcluster_id = hcluster,
                     dbcluster_id = dbcluster,
                     dataset_id = dataset_id, 
                     country_isoa3 = country_isoa3,
                     dataset_name = dataset_name)
}) %>% 
  dplyr::bind_rows() %>% 
  dplyr::group_by(country_isoa3, hcluster_id) %>% 
  dplyr::mutate(hcluster_id = str_pad(cur_group_id(), pad = "0", 20))
%>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(country_isoa3, dbcluster_id) %>%
  dplyr::mutate(dbcluster_id = str_pad(cur_group_id(), pad = "0", 20))
%>% 
  dplyr::ungroup() %>% 
  dplyr::select(hcluster_id, dbcluster_id, dataset_id, dataset_name) 