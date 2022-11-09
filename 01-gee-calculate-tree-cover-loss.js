/**** Start of imports. If edited, may not auto-convert in the playground. ****/
var gfc = ee.Image("UMD/hansen/global_forest_change_2020_v1_8"),
    ecoregions = ee.FeatureCollection("RESOLVE/ECOREGIONS/2017"),
    wcmc_areas = ee.FeatureCollection("WCMC/WDPA/current/polygons"),
    region2 = ee.FeatureCollection("users/maus/mining_and_quarry/global_mining_and_quarry_20220203"),
    region1 = ee.FeatureCollection("users/maus/mining_and_quarry/global_mining_polygons_v2");
/***** End of imports. If edited, may not auto-convert in the playground. *****/
var mines = region1.select(['system:index', 'FID', 'ISO3_CO', 'COUNTRY', 'AREA'],['gid', 'id', 'isoa3', 'country', 'mine_area'])//.limit(10)
//var mines = region2.select(['system:index', 'id', 'isoa3', 'country', 'area'],['gid', 'id', 'isoa3', 'country', 'mine_area'])//.limit(10)
//print(mines.limit(1))
var v = '20221109'

// export function
function export_table (table, description) { 
  Export.table.toDrive({
    collection: ee.FeatureCollection(table),
    description: description,
    folder: 'GEE',
    fileFormat: 'CSV',
    selectors: ['gid', 'id', 'isoa3', 'country', 'ecoregion_id', 'ecoregion', 'biome_id', 'biome', 'mine_area', 'groups']
  });
}

// Define a spatial filter as geometries that intersect.
var spatialFilter = ee.Filter.intersects({
  leftField: '.geo',
  rightField: '.geo',
  maxError: 10
});

// Define a saveAll join.
var distSaveAll = ee.Join.saveAll({
  matchesKey: 'ecoregion'
});

// Add ecoregions as a property of mines
var eco_mines = distSaveAll.apply(mines, ecoregions, spatialFilter)
  .map(function(feat) {
    var eco = ee.Feature(ee.List(feat.get('ecoregion')).get(0));
    var ECO_ID = eco.get('ECO_ID');
    var ECO_NAME = eco.get('ECO_NAME');
    var BIOME_NAME = eco.get('BIOME_NAME');
    var BIOME_NUM = eco.get('BIOME_NUM');
    var properties = feat.propertyNames();
    var selectProperties = properties.filter(ee.Filter.neq('item', 'ecoregion'));

    return feat.select(selectProperties).set(
      'ecoregion_id', ECO_ID,
      'ecoregion', ECO_NAME,
      'biome_id', BIOME_NUM,
      'biome', BIOME_NAME
    );
});

Export.table.toDrive({
    collection: eco_mines,
    description: 'mining_features_' + v,
    folder: 'GEE',
    fileFormat: 'GeoJSON'
  });
  
// initial forest cover 
var treecover = gfc.select(['treecover2000']);
var treecover000 = treecover.updateMask(treecover.gt(0));
var treecover025 = treecover.updateMask(treecover.lte(25));
var treecover050 = treecover.updateMask(treecover.gt(25).and(treecover.lte(50)));
var treecover075 = treecover.updateMask(treecover.gt(50).and(treecover.lte(75)));
var treecover100 = treecover.updateMask(treecover.gt(75));

// Get the forest loss image
var loss_image = gfc.select(['loss']);
var loss_area_image = loss_image.multiply(ee.Image.pixelArea());
var loss_year = gfc.select(['lossyear']);

// compute forest loss 
function get_forest_loss (trc){
  var loss_by_year = loss_area_image.mask(trc).addBands(loss_year).reduceRegions({
   collection: eco_mines,
   reducer: ee.Reducer.sum().group({
     groupField: 1
   }),
   scale: 30,
   tileScale: 2
  });
  return loss_by_year;
}


// ALL MINING AREAS
//print(get_forest_loss(treecover000))
export_table(get_forest_loss(treecover000), 'tree_cover_loss_mines_000_' + v);
export_table(get_forest_loss(treecover025), 'tree_cover_loss_mines_025_' + v);
export_table(get_forest_loss(treecover050), 'tree_cover_loss_mines_050_' + v);
export_table(get_forest_loss(treecover075), 'tree_cover_loss_mines_075_' + v);
export_table(get_forest_loss(treecover100), 'tree_cover_loss_mines_100_' + v);
  
// ALL PROTECTED AREAS
var treecover000p = treecover000.updateMask(ee.Image.constant(1).clip(wcmc_areas).mask());
var treecover025p = treecover025.updateMask(ee.Image.constant(1).clip(wcmc_areas).mask());
var treecover050p = treecover050.updateMask(ee.Image.constant(1).clip(wcmc_areas).mask());
var treecover075p = treecover075.updateMask(ee.Image.constant(1).clip(wcmc_areas).mask());
var treecover100p = treecover100.updateMask(ee.Image.constant(1).clip(wcmc_areas).mask());

export_table(get_forest_loss(treecover000p), 'tree_cover_loss_mines_000_p_' + v);
export_table(get_forest_loss(treecover025p), 'tree_cover_loss_mines_025_p_' + v);
export_table(get_forest_loss(treecover050p), 'tree_cover_loss_mines_050_p_' + v);
export_table(get_forest_loss(treecover075p), 'tree_cover_loss_mines_075_p_' + v);
export_table(get_forest_loss(treecover100p), 'tree_cover_loss_mines_100_p_' + v);

// PROTECTED CLASSES 
//
// Ia (strict nature reserve)
// Ib (wilderness area)
// II (national park)
// III (natural monument or feature)
// IV (habitat/species management area)
// V (protected landscape/seascape)
// VI (PA with sustainable use of natural resources), 
// Not applicable, Not assigned, or Not reported -- these are aggregated andcalculated by differece


// Ia (strict nature reserve)
var p = 'Ia';
var protected_areas = wcmc_areas.filter(ee.Filter.inList('IUCN_CAT', [p]));
var protcted_mask = ee.Image.constant(1).clip(protected_areas).mask();
treecover000p = treecover000.updateMask(protcted_mask);
treecover025p = treecover025.updateMask(protcted_mask);
treecover050p = treecover050.updateMask(protcted_mask);
treecover075p = treecover075.updateMask(protcted_mask);
treecover100p = treecover100.updateMask(protcted_mask);

export_table(get_forest_loss(treecover000p), 'tree_cover_loss_mines_000_' + p + '_' + v);
export_table(get_forest_loss(treecover025p), 'tree_cover_loss_mines_025_' + p + '_' + v);
export_table(get_forest_loss(treecover050p), 'tree_cover_loss_mines_050_' + p + '_' + v);
export_table(get_forest_loss(treecover075p), 'tree_cover_loss_mines_075_' + p + '_' + v);
export_table(get_forest_loss(treecover100p), 'tree_cover_loss_mines_100_' + p + '_' + v);

// Ib (wilderness area)
p = 'Ib';
protected_areas = wcmc_areas.filter(ee.Filter.inList('IUCN_CAT', [p]));
protcted_mask = ee.Image.constant(1).clip(protected_areas).mask();
treecover000p = treecover000.updateMask(protcted_mask);
treecover025p = treecover025.updateMask(protcted_mask);
treecover050p = treecover050.updateMask(protcted_mask);
treecover075p = treecover075.updateMask(protcted_mask);
treecover100p = treecover100.updateMask(protcted_mask);

export_table(get_forest_loss(treecover000p), 'tree_cover_loss_mines_000_' + p + '_' + v);
export_table(get_forest_loss(treecover025p), 'tree_cover_loss_mines_025_' + p + '_' + v);
export_table(get_forest_loss(treecover050p), 'tree_cover_loss_mines_050_' + p + '_' + v);
export_table(get_forest_loss(treecover075p), 'tree_cover_loss_mines_075_' + p + '_' + v);
export_table(get_forest_loss(treecover100p), 'tree_cover_loss_mines_100_' + p + '_' + v);


// II (national park)
p = 'II';
protected_areas = wcmc_areas.filter(ee.Filter.inList('IUCN_CAT', [p]));
protcted_mask = ee.Image.constant(1).clip(protected_areas).mask();
treecover000p = treecover000.updateMask(protcted_mask);
treecover025p = treecover025.updateMask(protcted_mask);
treecover050p = treecover050.updateMask(protcted_mask);
treecover075p = treecover075.updateMask(protcted_mask);
treecover100p = treecover100.updateMask(protcted_mask);

export_table(get_forest_loss(treecover000p), 'tree_cover_loss_mines_000_' + p + '_' + v);
export_table(get_forest_loss(treecover025p), 'tree_cover_loss_mines_025_' + p + '_' + v);
export_table(get_forest_loss(treecover050p), 'tree_cover_loss_mines_050_' + p + '_' + v);
export_table(get_forest_loss(treecover075p), 'tree_cover_loss_mines_075_' + p + '_' + v);
export_table(get_forest_loss(treecover100p), 'tree_cover_loss_mines_100_' + p + '_' + v);


// III (natural monument or feature)
p = 'III';
protected_areas = wcmc_areas.filter(ee.Filter.inList('IUCN_CAT', [p]));
protcted_mask = ee.Image.constant(1).clip(protected_areas).mask();
treecover000p = treecover000.updateMask(protcted_mask);
treecover025p = treecover025.updateMask(protcted_mask);
treecover050p = treecover050.updateMask(protcted_mask);
treecover075p = treecover075.updateMask(protcted_mask);
treecover100p = treecover100.updateMask(protcted_mask);

export_table(get_forest_loss(treecover000p), 'tree_cover_loss_mines_000_' + p + '_' + v);
export_table(get_forest_loss(treecover025p), 'tree_cover_loss_mines_025_' + p + '_' + v);
export_table(get_forest_loss(treecover050p), 'tree_cover_loss_mines_050_' + p + '_' + v);
export_table(get_forest_loss(treecover075p), 'tree_cover_loss_mines_075_' + p + '_' + v);
export_table(get_forest_loss(treecover100p), 'tree_cover_loss_mines_100_' + p + '_' + v);

// IV (habitat/species management area)
p = 'IV';
protected_areas = wcmc_areas.filter(ee.Filter.inList('IUCN_CAT', [p]));
protcted_mask = ee.Image.constant(1).clip(protected_areas).mask();
treecover000p = treecover000.updateMask(protcted_mask);
treecover025p = treecover025.updateMask(protcted_mask);
treecover050p = treecover050.updateMask(protcted_mask);
treecover075p = treecover075.updateMask(protcted_mask);
treecover100p = treecover100.updateMask(protcted_mask);

export_table(get_forest_loss(treecover000p), 'tree_cover_loss_mines_000_' + p + '_' + v);
export_table(get_forest_loss(treecover025p), 'tree_cover_loss_mines_025_' + p + '_' + v);
export_table(get_forest_loss(treecover050p), 'tree_cover_loss_mines_050_' + p + '_' + v);
export_table(get_forest_loss(treecover075p), 'tree_cover_loss_mines_075_' + p + '_' + v);
export_table(get_forest_loss(treecover100p), 'tree_cover_loss_mines_100_' + p + '_' + v);

// V (protected landscape/seascape)
p = 'V';
protected_areas = wcmc_areas.filter(ee.Filter.inList('IUCN_CAT', [p]));
protcted_mask = ee.Image.constant(1).clip(protected_areas).mask();
treecover000p = treecover000.updateMask(protcted_mask);
treecover025p = treecover025.updateMask(protcted_mask);
treecover050p = treecover050.updateMask(protcted_mask);
treecover075p = treecover075.updateMask(protcted_mask);
treecover100p = treecover100.updateMask(protcted_mask);

export_table(get_forest_loss(treecover000p), 'tree_cover_loss_mines_000_' + p + '_' + v);
export_table(get_forest_loss(treecover025p), 'tree_cover_loss_mines_025_' + p + '_' + v);
export_table(get_forest_loss(treecover050p), 'tree_cover_loss_mines_050_' + p + '_' + v);
export_table(get_forest_loss(treecover075p), 'tree_cover_loss_mines_075_' + p + '_' + v);
export_table(get_forest_loss(treecover100p), 'tree_cover_loss_mines_100_' + p + '_' + v);


// VI (PA with sustainable use of natural resources), 
p = 'VI';
protected_areas = wcmc_areas.filter(ee.Filter.inList('IUCN_CAT', [p]));
protcted_mask = ee.Image.constant(1).clip(protected_areas).mask();
treecover000p = treecover000.updateMask(protcted_mask);
treecover025p = treecover025.updateMask(protcted_mask);
treecover050p = treecover050.updateMask(protcted_mask);
treecover075p = treecover075.updateMask(protcted_mask);
treecover100p = treecover100.updateMask(protcted_mask);

export_table(get_forest_loss(treecover000p), 'tree_cover_loss_mines_000_' + p + '_' + v);
export_table(get_forest_loss(treecover025p), 'tree_cover_loss_mines_025_' + p + '_' + v);
export_table(get_forest_loss(treecover050p), 'tree_cover_loss_mines_050_' + p + '_' + v);
export_table(get_forest_loss(treecover075p), 'tree_cover_loss_mines_075_' + p + '_' + v);
export_table(get_forest_loss(treecover100p), 'tree_cover_loss_mines_100_' + p + '_' + v);

