var ecoregions = ee.FeatureCollection("RESOLVE/ECOREGIONS/2017"),
    maus_and_osm = ee.FeatureCollection("users/maus/mining_and_quarry/global_mining_and_quarry_20220203"),
    maus_2022 = ee.FeatureCollection("users/maus/mining_and_quarry/global_mining_polygons_v2");

    var v = '20240829'

    var mines_a = maus_and_osm.select(['system:index', 'id', 'isoa3', 'country', 'area'],['gid', 'id', 'isoa3', 'country', 'mine_area'])
    var mines_b = maus_2022.select(['system:index', 'FID', 'ISO3_CO', 'COUNTRY', 'AREA'],['gid', 'id', 'isoa3', 'country', 'mine_area'])
    
    // Define a spatial filter of geometries that intersect
    var spatial_intersects = ee.Filter.intersects({
      leftField: '.geo',
      rightField: '.geo',
      maxError: 10
    });
    
    // Define a saveAll join
    var distSaveAll = ee.Join.saveAll({
      matchesKey: 'ecoregion',
      measureKey: 'key',
      outer: true
    });
    
    // Define function get biomes 
    function get_biomes (mines) { 
      var res = distSaveAll.apply(mines, ecoregions, spatial_intersects).map(function(feat) {
        
        var eco = ee.Feature(ee.List(feat.get('ecoregion')).get(0));
    
        return ee.Feature(feat.geometry(), {
          'gid': feat.get('gid'),
          'id': feat.get('id'),
          'isoa3': feat.get('isoa3'),
          'country': feat.get('country'),
          'mine_area': feat.get('mine_area'),
          'ecoregion_id': ee.Algorithms.If(feat.get('ecoregion'), eco.get('ECO_ID'), null),
          'ecoregion': ee.Algorithms.If(feat.get('ecoregion'), eco.get('ECO_NAME'), null),
          'biome_id': ee.Algorithms.If(feat.get('ecoregion'), eco.get('BIOME_NUM'), null),
          'biome': ee.Algorithms.If(feat.get('ecoregion'), eco.get('BIOME_NAME'), null)
        });
      });
      return(res)
    }
    
    var eco_mines = get_biomes(mines_a);
    
    Export.table.toAsset({
        collection: eco_mines,
        description: 'mining_features_biomes_' + v + 'a',
        assetId: 'mining_and_quarry/mining_features_biomes_' + v + 'a'
      });
      
    Export.table.toDrive({
        collection: get_biomes(mines_a),
        description: 'mining_features_biomes_' + v + 'a',
        folder: 'mining-tree-cover-loss-' + v + 'a',
        fileFormat: 'GeoJSON'
      });
      
    var eco_mines = get_biomes(mines_b);
    
    Export.table.toAsset({
        collection: eco_mines,
        description: 'mining_features_biomes_' + v + 'b',
        assetId: 'mining_and_quarry/mining_features_biomes_' + v + 'b'
      });
      
    Export.table.toDrive({
        collection: get_biomes(mines_b),
        description: 'mining_features_biomes_' + v + 'b',
        folder: 'mining-tree-cover-loss-' + v + 'b',
        fileFormat: 'GeoJSON'
      });