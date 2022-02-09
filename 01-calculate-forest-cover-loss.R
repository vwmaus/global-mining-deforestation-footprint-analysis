library(reticulate)
library(geojsonio)
library(stringr)
library(rgee)
library(rgeeExtra)
library(dplyr)
library(sf)
library(progress)
library(readr)
library(tidyr)

ee_check()
ee_Initialize()

# ------------------------------------------------------------------------------
# set all local and gee input datasets 
data_version <- "20220203"
global_quarry_path <- str_c("./data/global_mining_and_quarry_",data_version,".gpkg")

ecoregions <- ee$FeatureCollection("RESOLVE/ECOREGIONS/2017")

wcmc_areas <- ee$FeatureCollection("WCMC/WDPA/current/polygons")

gfc <- ee$Image('UMD/hansen/global_forest_change_2020_v1_8')

# ------------------------------------------------------------------------------
# prepare forest cover loss image 
loss_area_image <- gfc$select('loss')$multiply(ee$Image$pixelArea())$addBands(gfc$select('lossyear'))

# ------------------------------------------------------------------------------
# prepare tree cover masks for different initial forest cover shares
treecover2000 <- gfc$select('treecover2000')
treecover_mask <- treecover2000$updateMask(treecover2000$gt(0))$rename('forest_loss_000')$addBands(list(
  treecover2000$updateMask(treecover2000$lte(25))$rename('forest_loss_025'),
  treecover2000$updateMask(treecover2000$gt(25)$And(treecover2000$lte(50)))$rename('forest_loss_050'),
  treecover2000$updateMask(treecover2000$gt(50)$And(treecover2000$lte(75)))$rename('forest_loss_075'),
  treecover2000$updateMask(treecover2000$gt(75))$rename('forest_loss_100')))

# ------------------------------------------------------------------------------
# prepare tree cover masks for protected areas and different initial forest cover shares
# Not applicable, Not assigned, or Not reported -- these are aggregated and calculated by difference
wcmc_types <- tribble(~id, ~description,
                      'Ia',  'strict nature reserve',
                      'Ib',  'wilderness area',
                      'II',  'national park',
                      'III', 'natural monument or feature',
                      'IV',  'habitat/species management area',
                      'V',   'protected landscape/seascape',
                      'VI',  'PA with sustainable use of natural resources',
                      'All', 'All types of protected areas')

for (l in treecover_mask$bandNames()$getInfo()) {
  tmp_mask <- treecover_mask$select(l)
  for (p in wcmc_types$id){
    wcmc_mask <- tmp_mask$updateMask(ee$Image$constant(1)$clip(wcmc_areas)$mask())
    if(p != 'All'){
      protected_areas <- wcmc_areas$filter(ee$Filter$inList('IUCN_CAT', list(p)))
      wcmc_mask <- tmp_mask$updateMask(ee$Image$constant(1)$clip(protected_areas)$mask())
    }
    new_band <- str_c(l, '_', p)
    treecover_mask <- treecover_mask$addBands(wcmc_mask$rename(new_band))
  }
}

# ------------------------------------------------------------------------------
# ecoregions spatial filter as geometries that intersect quarry and saveAll join
spatialFilter <- ee$Filter$intersects(leftField = '.geo', rightField = '.geo', maxError = 10)
distSaveAll = ee$Join$saveAll(matchesKey = 'ecoregion')

# ------------------------------------------------------------------------------
# define processing tiles
# tiles <- rbind(c(-180,-90), c(180,-90), c(180,90), c(-180,90), c(-180,-90)) |> 
#   list() |> 
#   st_polygon() |> 
#   st_sfc(crs = 'EPSG:4326') |> 
#   st_make_grid(
#     cellsize = c(5, 5),
#     what = "polygons",
#     square = TRUE,
#     flat_topped = FALSE
#   ) |> 
#   st_as_sf() |> 
#   mutate(tile_id = sapply(x, FUN = function(x){
#     st_coordinates(x) |> 
#       as_tibble() |>
#       group_by(L1, L2) |> 
#       summarise(X = min(X), Y = min(Y), .groups = 'drop') |> 
#       transmute(tile_id = str_c(
#         ifelse(X < 0, 
#                str_c(str_pad(abs(X), width = 3, pad = '0'), 'W'), 
#                str_c(str_pad(abs(X), width = 3, pad = '0'), 'E')),
#         ifelse(Y < 0, 
#                str_c(str_pad(abs(Y), width = 2, pad = '0'), 'S'), 
#                str_c(str_pad(abs(Y), width = 2, pad = '0'), 'N'))
#       )) |> 
#       as.character()
#   })) 

# ------------------------------------------------------------------------------
# define mining and quarry processing tiles
n_tile_features <- 300
global_quarry <- st_read(global_quarry_path, quiet = TRUE) |> 
  filter(country != "Antarctica") |> # OSM includes some polygons in Antarctica: Removed 
  mutate(tile_id = rep(1:n(), each = n_tile_features)[1:n()])
  # st_join(y = tiles, left = TRUE, join = function(x, y, ...) st_intersects(st_centroid(st_geometry(x)), y, ...))

# ------------------------------------------------------------------------------
# calculate forest cover loss within mining and quarry per tile
gee_dir <- str_c("./data/gee_tiles_forest_loss_", data_version)
dir.create(gee_dir, showWarnings = FALSE, recursive = TRUE)
pb <- progress_bar$new(total = length(unique(global_quarry$tile_id)))
for(tl in sort(unique(global_quarry$tile_id))){
  
  pb$tick()
  
  tile_feat <- filter(global_quarry, tile_id == tl)
  
  # Add ecoregions as a property of quarry and mining dataset
  tile_feat <- distSaveAll$apply(sf_as_ee(tile_feat), ecoregions, spatialFilter)$map(function(feat) {
    eco <- ee$Feature(ee$List(feat$get('ecoregion'))$get(0))
    ECO_ID <- eco$get('ECO_ID')
    ECO_NAME <- eco$get('ECO_NAME')
    BIOME_NAME <- eco$get('BIOME_NAME')
    BIOME_NUM <- eco$get('BIOME_NUM')
    properties <- feat$propertyNames()
    selectProperties <- properties$filter(ee$Filter$neq('item', 'ecoregion'))
    return(feat$select(selectProperties)$set(
      'ecoregion_id', ECO_ID,
      'ecoregion', ECO_NAME,
      'biome_id', BIOME_NUM,
      'biome', BIOME_NAME
    ))
  })
  
  # compute forest loss area 
  gee_floss <- tile_feat$map(function(feat) {
    out_list <- lapply(treecover_mask$bandNames()$getInfo(), FUN = function(l){
      loss_area_image$mask(treecover_mask$select(l))$reduceRegion(
        reducer = ee$Reducer$sum()$group({groupField = 1}),
        geometry = feat$geometry(),
        scale = 30,
        tileScale = 2
      )$rename(list("groups"), list(l))
    })
    out_dic <- out_list[[1]]
    for (i in 1:length(out_list)) {
      out_dic <-out_dic$combine({reducer = out_list[[i]]})
    }
    ee$Feature(
      feat$geometry()$centroid(100),
      out_dic
    )$copyProperties(feat)
  }) |> 
    ee_as_sf() |>
    sf::st_drop_geometry()  |> 
    tibble::as_tibble()
  
  # Tidy forest loss time series
  res_loss <- lapply(select(gee_floss, matches('forest_loss')) |> names(), function(i){
    gee_floss |> 
      select_at(c('id', i)) |> 
      rename_with(~ c('id', 'forest_loss'), all_of(c('id', i))) 
      unnest(cols = forest_loss) |> 
      mutate(year = 2000 + as.numeric(stringr::str_extract(forest_loss, pattern = "(?<=\"group\": ).*(?=,)")),
             area = stringr::str_replace(forest_loss, " \\}", "END"),
             area = as.numeric(stringr::str_extract(area, pattern = "(?<=\"sum\": ).*(?=END)")),
             area = units::set_units(units::set_units(area, m^2), km^2)) |> 
      group_by(id) |> 
      nest() |> 
      ungroup() |> 
      rename_with(~ c('id', i), all_of(c('id', 'data'))) 
  })
  
  res <- select(gee_floss, -matches('forest_loss'))
  for (i in seq_along(res_loss)) {
    res <- left_join(res, res_loss[[i]], by = c('id' = 'id'))
  }
  
  write_rds(res, str_c(gee_dir, '/', str_pad(tl, width = 4, pad = "0"), '.rds'))
  
}

# TODO: Cluster commodities
