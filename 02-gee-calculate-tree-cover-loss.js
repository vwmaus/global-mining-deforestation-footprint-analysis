var gfc = ee.Image("UMD/hansen/global_forest_change_2020_v1_8"),
    wcmc_areas = ee.FeatureCollection("WCMC/WDPA/current/polygons"),
    maus_and_osm = ee.FeatureCollection("users/maus/mining_and_quarry/mining_features_biomes_20221123a"),
    maus_2022 = ee.FeatureCollection("users/maus/mining_and_quarry/mining_features_biomes_20221123b");
    
    var v = '20240829a'
    var mines = maus_and_osm
    //var mines = maus_and_osm.filter(ee.Filter.eq('isoa3', 'BRA')).sort('mine_area', false).limit(10);
    
    // export function
    function export_table (table, description) { 
      Export.table.toDrive({
        collection: ee.FeatureCollection(table),
        description: description,
        folder: 'mining-tree-cover-loss-' + v ,
        fileFormat: 'CSV',
        selectors: ['gid', 'id', 'isoa3', 'country', 'ecoregion_id', 'ecoregion', 'biome_id', 'biome', 'mine_area', 'groups']
      });
    }
    
    // Define function to compute forest loss and convert groups to properties
    function get_forest_loss(trc) {
      
      var loss_by_year = loss_area_image.mask(trc).addBands(loss_year).reduceRegions({
        collection: mines,
        reducer: ee.Reducer.sum().group({
          groupField: 1
        }),
        scale: 30,
        tileScale: 2
      });
      
      // Calculate the initial cover area
      var initial_cover_area = trc.gt(0).multiply(ee.Image.pixelArea()).reduceRegions({
        collection: mines,
        reducer: ee.Reducer.sum(),
        scale: 30,
        tileScale: 2
      });
      
      // Calculate the mine area (in m²)
      var mine_area = ee.Image.pixelArea().reduceRegions({
        collection: mines,
        reducer: ee.Reducer.sum(),
        scale: 30,
        tileScale: 2
      });
    
      // Convert group results to properties
      var loss_with_properties = loss_by_year.map(function(feature) {
        var groups = ee.List(feature.get('groups'));
    
        // Generate a list of possible group numbers (1 to 20)
        var propertyNames = ee.List.sequence(1, 20).map(function(i) {
          return ee.String('y').cat(ee.Number(i).add(2000).format('%.0f'));
        });
    
        // Map over the sequence and retrieve the sum for each group or return 'NA'
        var propertyValues = ee.List.sequence(1, 20).map(function(i) {
          var groupItem = groups.map(function(group) {
            group = ee.Dictionary(group);
            return ee.Algorithms.If(
              ee.Number(group.get('group')).eq(i),
              group.get('sum'),
              null
            );
          }).filter(ee.Filter.notNull(['item'])).get(0);
    
          return ee.Algorithms.If(
            ee.Algorithms.IsEqual(groupItem, null),
            'NA',
            groupItem
          );
        });
    
        // Create a dictionary from property names and values
        var properties = ee.Dictionary.fromLists(propertyNames, propertyValues);
    
        // Get the corresponding initial cover feature
        var matching_cover = initial_cover_area.filter(ee.Filter.eq('gid', feature.get('gid'))).first();
        var matching_mine_area = mine_area.filter(ee.Filter.eq('gid', feature.get('gid'))).first();
        
        // Extract the sum property (initial cover area) and add it as a new property
        var initial_area = ee.Number(matching_cover.get('sum'));
        var mine_area_pixel_gee = ee.Number(matching_mine_area.get('sum'));
        var mine_area_feature_gee = feature.geometry().area();
        
        // Return the feature with the new properties added
        return feature.set(properties).set({
          'mine_area_ft_gee': mine_area_feature_gee,
          'mine_area_px_gee': mine_area_pixel_gee,
          'tree_cover_2000': initial_area
        }).set('groups', null);  // Remove the 'groups' property
    
      });
    
      return loss_with_properties;
    }
    
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
    // Not applicable, Not assigned, or Not reported -- these are aggregated and calculated by differece
    
    
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
    
    