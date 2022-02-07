library(reticulate)
library(geojsonio)
library(stringr)
library(rgee)
library(dplyr)
library(sf)

ee_check()
ee_Initialize()

v <- "20220203"

global_quarry <- st_read(str_c("./data/global_mining_and_quarry_",v,".gpkg")) |> 
  slice(1:10) |> 
  sf_as_ee()

ecoregions <- ee$FeatureCollection("RESOLVE/ECOREGIONS/2017")

wcmc_areas <- ee$FeatureCollection("WCMC/WDPA/current/polygons")

gfc <- ee$Image('UMD/hansen/global_forest_change_2020_v1_8')

treecover <- gfc$select('treecover2000')
treecover000 <- treecover$gt(0)
treecover025 <- treecover$lte(25)
treecover050 <- treecover$gt(25)$And(treecover$lte(50))
treecover075 <- treecover$gt(50)$And(treecover$lte(75))
treecover100 <- treecover$gt(75)

loss_image <- gfc$select('loss')
loss_area_image <- loss_image$multiply(ee$Image$pixelArea())
loss_year <- gfc$select('lossyear')

# Define a spatial filter as geometries that intersect
spatialFilter <- ee$Filter$intersects(leftField = '.geo', rightField = '.geo', maxError = 10)

# Define a saveAll join
distSaveAll = ee$Join$saveAll(matchesKey = 'ecoregion')

# Add ecoregions as a property of mines
global_quarry <- distSaveAll$apply(global_quarry, ecoregions, spatialFilter)$map(function(feat) {
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


data <- global_quarry$map(function(feature) {
  loss000 <- loss_area_image$mask(treecover000)$addBands(loss_year)$reduceRegion(
    reducer = ee$Reducer$sum()$group({groupField = 1}),
    geometry = feature$geometry(),
    scale = 30,
    tileScale = 2
  )$rename(list("groups"), list("forest_loss_area_000"))
  loss025 <- loss_area_image$mask(treecover025)$addBands(loss_year)$reduceRegion(
    reducer = ee$Reducer$sum()$group({groupField = 1}),
    geometry = feature$geometry(),
    scale = 30,
    tileScale = 2
  )$rename(list("groups"), list("forest_loss_area_025"))
  loss050 <- loss_area_image$mask(treecover050)$addBands(loss_year)$reduceRegion(
    reducer = ee$Reducer$sum()$group({groupField = 1}),
    geometry = feature$geometry(),
    scale = 30,
    tileScale = 2
  )$rename(list("groups"), list("forest_loss_area_050"))
  loss075 <- loss_area_image$mask(treecover075)$addBands(loss_year)$reduceRegion(
    reducer = ee$Reducer$sum()$group({groupField = 1}),
    geometry = feature$geometry(),
    scale = 30,
    tileScale = 2
  )$rename(list("groups"), list("forest_loss_area_075"))
  loss100 <- loss_area_image$mask(treecover100)$addBands(loss_year)$reduceRegion(
    reducer = ee$Reducer$sum()$group({groupField = 1}),
    geometry = feature$geometry(),
    scale = 30,
    tileScale = 2
  )$rename(list("groups"), list("forest_loss_area_100"))
  ee$Feature(
        feature$geometry()$centroid(100),
        loss000$combine({reducer2 = loss025})$combine({reducer3 = loss050})$combine({reducer4 = loss075})$combine({reducer3 = loss100})
  )$copyProperties(feature)
})

ee_as_sf(data)

ee_as_sf(global_quarry)

# TODO: loop for each protected option
for (p in c('Ia')) {
  protected_areas <- wcmc_areas$filter(ee$Filter$inList('IUCN_CAT', p))
  protcted_mask <- ee.Image.constant(1).clip(protected_areas).mask()
  treecover000p <- treecover000$updateMask(protcted_mask)
}

# TODO: Cluster commodities

