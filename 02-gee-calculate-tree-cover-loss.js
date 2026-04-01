var wcmc_areas = ee.FeatureCollection("WCMC/WDPA/current/polygons"),
    mines = ee.FeatureCollection("users/maus/global_mine_land_use_v1-0/mine_polygons"),
    gfc = ee.Image("UMD/hansen/global_forest_change_2023_v1_11"),
    plantations_nigeria = ee.Image("projects/mine-the-gap/assets/plantations_nigeria"),
    plantations_southeastasia = ee.Image("projects/mine-the-gap/assets/plantations_southeastasia");
    
// Combine into a single plantation mask using the first band (first year) of each
var plantations = ee.ImageCollection([
  plantations_nigeria.select(0),
  plantations_southeastasia.select(0)
]).mosaic();

/*

This script computes the area of tree cover loss annually for the given set of mines.
It generates two sets of files: one for all tree cover loss, and a separate, 
space-optimized set strictly for loss within plantations.

Author: Victor Maus
Data: March 24, 2026

*/

// Settings
var v = '20260324'; // output version
var y_start = 2000; // starting year
var y_end = 2023;   // ending year

// input mines
var mines = mines.select(['system:index', 'id'], ['gid', 'id']);
//print(mines.limit(10));
//var amine = mines.filter(ee.Filter.eq('id', 'A0208936'));
//print(amine)

// Define the static part of the selectors list
var yearSelectors = [];
for (var year = y_start; year <= y_end; year++) {
  yearSelectors.push(year-y_start + '_loss');
}
var selectors = ['gid', 'id', 'area_mine_gee', 'treecover2000'].concat(yearSelectors);
//print(selectors)

// Export function
function export_table(table, description) {
  Export.table.toDrive({
    collection: ee.FeatureCollection(table),
    description: description,
    folder: 'mining-tree-cover-loss-' + v,
    fileFormat: 'CSV',
    selectors: selectors
  });
}

// Define function to compute forest loss (Restored to original structure)
function get_forest_loss(trc, batch) {
  
  // Create initial tree cover mask
  var treecover_mask = trc.gt(0);
    
  // Create the initial cover area image
  var treecover2000 = treecover_mask.multiply(ee.Image.pixelArea()).rename('treecover2000');
  
  // Create the mine area image
  var area_mine_gee = ee.Image.pixelArea().rename('area_mine_gee');

  // Create loss area per year bands
  var loss_area_bands = ee.List.sequence(y_start-y_start, y_end-y_start).map(function(year) {
    year = ee.Number(year);
    var loss_in_year = gfc.select(['loss']).updateMask(treecover_mask).multiply(ee.Image.pixelArea())
      .mask(gfc.select(['lossyear']).eq(year));
    return loss_in_year;
  });
  var loss_area_by_year = ee.ImageCollection(loss_area_bands).toBands();

  // Combine all bands into one image
  var image_with_bands = treecover2000
    .addBands(area_mine_gee)
    .addBands(loss_area_by_year);
  
  // Perform reduceRegions
  var loss_with_properties = image_with_bands.reduceRegions({
    collection: batch,
    reducer: ee.Reducer.sum().unweighted(), // Kept unweighted to prevent memory crash
    scale: 30,
    crs: 'EPSG:4326', // Kept explicit crs for projection stability
    tileScale: 16 // Kept to handle the 261,000 polygons
  });

  return loss_with_properties;
  
}

// Initial forest cover
var treecover = gfc.select(['treecover2000']);
var treecover000 = treecover.updateMask(treecover.gt(0));
var treecover025 = treecover.updateMask(treecover.gt(0).and(treecover.lte(25)));
var treecover050 = treecover.updateMask(treecover.gt(25).and(treecover.lte(50)));
var treecover075 = treecover.updateMask(treecover.gt(50).and(treecover.lte(75)));
var treecover100 = treecover.updateMask(treecover.gt(75));

// List of trc images
var trc_images = [
  {'image': treecover000, 'name': '000'},
  {'image': treecover025, 'name': '025'},
  {'image': treecover050, 'name': '050'},
  {'image': treecover075, 'name': '075'},
  {'image': treecover100, 'name': '100'}
];

// List of protected categories, including 'all' for all areas
var protected_categories = [
  {'name': 'ALL', 'areas': null},       // Unmasked areas
  {'name': 'PRT', 'areas': wcmc_areas}  // All protected areas
];

var categories = [
  {cat: 'Ia',  name: 'SNR'}, // Strict Nature Reserve
  {cat: 'Ib',  name: 'WLA'}, // Wilderness Area
  {cat: 'II',  name: 'NPA'}, // National Park
  {cat: 'III', name: 'NMF'}, // Natural Monument or Feature
  {cat: 'IV',  name: 'HMA'}, // Habitat/Species Management Area
  {cat: 'V',   name: 'PLS'}, // Protected Landscape/Seascape
  {cat: 'VI',  name: 'SUN'}  // Sustainable Use of Natural Resources  
];

// Add specific categories to the list
for (var idx = 0; idx < categories.length; idx++) {
  protected_categories.push({
    'name': categories[idx].name,
    'areas': wcmc_areas.filter(ee.Filter.eq('IUCN_CAT', categories[idx].cat))
  });
}

// Function to safely create protected mask
function createProtectedMask(protectedAreas) {
  if (protectedAreas) {
    // Create mask for protected areas
    return ee.Image(0).byte().paint(protectedAreas, 1).selfMask();
  } else {
    // Return an all-ones mask for 'all' areas
    return ee.Image(1).byte();
  }
}

// Higher-order function to create the mapping function
function getMaskedTrcImages(protected_mask) {
  return function(trc_dict) {
    var trc_image = trc_dict.image;
    var trc_name = trc_dict.name;
    var trc_image_masked = trc_image.updateMask(protected_mask);
    return {
      'image': trc_image_masked,
      'name': trc_name
    };
  };
}

// Loop through each protected category, including 'all'
for (var pcIdx = 0; pcIdx < protected_categories.length; pcIdx++) {
  var protected_category = protected_categories[pcIdx];
  var p_name = protected_category.name;
  var protected_areas = protected_category.areas;

  // Create the protected mask
  var protected_mask = createProtectedMask(protected_areas);

  // Get the mapping function with the current protected_mask
  var maskedTrcImagesFunction = getMaskedTrcImages(protected_mask);

  // For each tree cover image, create the masked or unmasked version
  var trc_images_masked = trc_images.map(maskedTrcImagesFunction);

  // Process all mines at once
  for (var j = 0; j < trc_images_masked.length; j++) {
    var trc_image_dict = trc_images_masked[j];
    var trc_image = trc_image_dict.image;
    var trc_name = trc_image_dict.name;

    // --- 1. STANDARD EXPORT ---
    var loss_with_properties = get_forest_loss(trc_image, mines);
    var description = 'tree_cover_loss_mines_' + trc_name + '_' + p_name + '_' + v;
    export_table(loss_with_properties, description);

    // --- 2. PLANTED EXPORT ---
    // Mask the current tree cover image specifically with the plantations mask
    var planted_mask = plantations.gt(0);
    var trc_image_planted = trc_image.updateMask(planted_mask);
    
    var loss_planted = get_forest_loss(trc_image_planted, mines);
    
    // Filter to keep ONLY polygons that have planted tree cover > 0
    // This removes polygons with null/0 data, massively saving space and export time.
    var loss_planted_filtered = loss_planted.filter(ee.Filter.gt('treecover2000', 0));

    var desc_planted = 'tree_cover_loss_mines_PLANTED_' + trc_name + '_' + p_name + '_' + v;
    export_table(loss_planted_filtered, desc_planted);
  }
}
