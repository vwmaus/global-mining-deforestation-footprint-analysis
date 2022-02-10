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
# read input data
data_version <- "20220203"
mining_areas <- 
  str_c("./data/global_mining_and_quarry_",data_version,".gpkg") |> 
  st_read() |> 
  mutate(dataset = "areas")

# This dataset was extracted from the SNL databse which requires a license
mining_commodities <- st_read("./data/snl/mining_commodities.gpkg") |> 
  mutate(id = str_pad(id, width = 7, pad = 0), dataset = "commodities")

# ------------------------------------------------------------------------------
# declare function to compute distance matrices for each country
calc_dist_matrix <-
  function(x,
           split_att,
           output_dir = ".",
           pb = NULL) {
    
    if(!is.null(pb)) pb$tick()
    
    path_features_dist_meter <- 
      stringr::str_glue("{output_dir}/{x[[split_att]][1]}.rds")
    
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    
    # stop processing if job has less than two features ------------------------
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
mine_features <- bind_rows(mining_areas, mining_commodities)
pb <- progress_bar$new(
  format = "  downloading [:bar] :percent in :elapsed",
  total = length(unique(mine_features$isoa3)), clear = FALSE, width= 60)
dist_matrix_paths <- foreach(
  mine_split = split(mine_features, mine_features$isoa3), 
  .combine = 'c'
) %do% {
  calc_dist_matrix(
    x = mine_split, 
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