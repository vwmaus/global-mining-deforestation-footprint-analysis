# Load the rgee package
library(rgee)

# Initialize Earth Engine
ee_Initialize()

# Import datasets
wcmc_areas <- ee$FeatureCollection("WCMC/WDPA/current/polygons")
mines <- ee$FeatureCollection("users/maus/global_mine_land_use_v1-0/mine_polygons")
gfc <- ee$Image("UMD/hansen/global_forest_change_2023_v1_11")

# Settings
v <- '20241004'  # Output version
y_start <- 2000  # Starting year
y_end <- 2023    # Ending year

# Input mines
mines <- mines$select(c('system:index', 'id'), c('gid', 'id'))

# Define the static part of the selectors list
yearSelectors <- c()
for (year in y_start:y_end) {
  yearSelectors <- c(yearSelectors, paste0(year - y_start, '_loss'))
}
selectors <- c('gid', 'id', 'area_mine_gee', 'treecover2000', yearSelectors)

# Export function
export_table <- function(table, description) {
  task <- ee_table_to_drive(
    collection = ee$FeatureCollection(table),
    description = description,
    folder = paste0('mining-tree-cover-loss-', v),
    fileFormat = 'CSV',
    selectors = selectors
  )
  task$start()
}

# Define function to compute forest loss
get_forest_loss <- function(trc, batch) {
  
  # Create the initial cover area image
  treecover2000 <- trc$gt(0)$multiply(ee$Image$pixelArea())$rename('treecover2000')
  
  # Create the mine area image
  area_mine_gee <- ee$Image$pixelArea()$rename('area_mine_gee')
  
  # Create loss area per year bands
  loss_area_bands <- ee$List$sequence(y_start - y_start, y_end - y_start)$map(
    ee_utils_pyfunc(function(year) {
      year <- ee$Number(year)
      loss_in_year <- gfc$select('loss')$multiply(ee$Image$pixelArea())$
        mask(gfc$select('lossyear')$eq(year))
      return(loss_in_year)
    })
  )
  loss_area_by_year <- ee$ImageCollection(loss_area_bands)$toBands()
  
  # Combine all bands into one image
  image_with_bands <- treecover2000$addBands(area_mine_gee)$addBands(loss_area_by_year)
  
  # Perform reduceRegions
  loss_with_properties <- image_with_bands$reduceRegions(
    collection = batch,
    reducer = ee$Reducer$sum(),
    scale = 30,
    tileScale = 2
  )
  
  return(loss_with_properties)
}

# Initial forest cover
treecover <- gfc$select('treecover2000')
treecover000 <- treecover$updateMask(treecover$gt(0))
treecover025 <- treecover$updateMask(treecover$gt(0)$And(treecover$lte(25)))
treecover050 <- treecover$updateMask(treecover$gt(25)$And(treecover$lte(50)))
treecover075 <- treecover$updateMask(treecover$gt(50)$And(treecover$lte(75)))
treecover100 <- treecover$updateMask(treecover$gt(75))

# List of trc images
trc_images <- list(
  list(image = treecover000, name = '000'),
  list(image = treecover025, name = '025'),
  list(image = treecover050, name = '050'),
  list(image = treecover075, name = '075'),
  list(image = treecover100, name = '100')
)

# List of protected categories, including 'all' for all areas
protected_categories <- list(
  list(name = 'AL', areas = NULL),        # Unmasked areas
  list(name = 'PR', areas = wcmc_areas)   # All protected areas
)

categories <- c('Ia', 'Ib', 'II', 'III', 'IV', 'V', 'VI')

# Add specific categories to the list
for (cat in categories) {
  protected_category <- list(
    name = cat,
    areas = wcmc_areas$filter(ee$Filter$eq('IUCN_CAT', cat))
  )
  protected_categories <- append(protected_categories, list(protected_category))
}

# Function to safely create protected mask
createProtectedMask <- function(protectedAreas) {
  if (!is.null(protectedAreas)) {
    # Create mask for protected areas
    return(ee$Image(0)$byte()$paint(protectedAreas, 1)$selfMask())
  } else {
    # Return an all-ones mask for 'all' areas
    return(ee$Image(1)$byte())
  }
}

# Higher-order function to create the mapping function
getMaskedTrcImages <- function(protected_mask) {
  function(trc_dict) {
    trc_image <- trc_dict$image
    trc_name <- trc_dict$name
    trc_image_masked <- trc_image$updateMask(protected_mask)
    return(list(
      image = trc_image_masked,
      name = trc_name
    ))
  }
}

# Loop through each protected category, including 'all'
for (protected_category in protected_categories) {
  p_name <- protected_category$name
  protected_areas <- protected_category$areas
  
  # Create the protected mask
  protected_mask <- createProtectedMask(protected_areas)
  
  # Get the mapping function with the current protected_mask
  maskedTrcImages <- getMaskedTrcImages(protected_mask)
  
  # For each tree cover image, create the masked or unmasked version
  trc_images_masked <- lapply(trc_images, maskedTrcImages)
  
  # Process all mines at once
  for (trc_image_dict in trc_images_masked) {
    trc_image <- trc_image_dict$image
    trc_name <- trc_image_dict$name
    
    loss_with_properties <- get_forest_loss(trc_image, mines)
    
    # Update the description to include the protected category
    description <- paste0('tree_cover_loss_mines_', trc_name, '_', p_name, '_', v)
    
    export_table(loss_with_properties, description)
  }
}
