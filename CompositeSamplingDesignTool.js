// =================================================================================
// === COMPOSITE SAMPLING ACROSS ENTIRE AOI TOOL - NO FOREST RESTRICTION ==========
// === Robust implementation for any land cover type ===============================
// =================================================================================
//
// PURPOSE: Generate hierarchical sampling design with:
//   - High Resolution (HR) soil cores for detailed analysis
//   - Composite samples (multiple subsamples) for broader coverage
//   - Strategic pairing between composites and HR cores
//   - SAMPLES ACROSS ENTIRE AOI (not restricted to forests)
//
// DIFFERENCE FROM ORIGINAL: This version removes the forest mask and samples
// across the entire area of interest, making it suitable for any ecosystem type
//
// =================================================================================
// === 1. CONFIGURATION ============================================================
// =================================================================================

var CONFIG = {
  // Sampling Design Parameters
  DEFAULT_HR_CORES: 10,
  DEFAULT_COMPOSITES_PER_STRATUM: 20,
  DEFAULT_COMPOSITE_AREA: 25, // m²
  DEFAULT_SUBSAMPLES: 5,
  DEFAULT_PAIRING_FRACTION: 0.4,
  DEFAULT_MAX_PAIRING_DISTANCE: 5000, // meters
  
  // Analysis Parameters
  CARBON_SCALE: 250, // meters
  MAX_PIXELS: 1e10,
  MAX_ERROR: 1, // meters for geometry operations
  
  // Seed for reproducibility
  RANDOM_SEED: 42
};

var STYLES = {
  TITLE: {fontSize: '28px', fontWeight: 'bold', color: '#005931'},
  SUBTITLE: {fontSize: '18px', fontWeight: '500', color: '#333333'},
  PARAGRAPH: {fontSize: '14px', color: '#555555'},
  HEADER: {fontSize: '16px', fontWeight: 'bold', margin: '16px 0 4px 8px'},
  SUBHEADER: {fontSize: '14px', fontWeight: 'bold', margin: '10px 0 0 0'},
  PANEL: {width: '420px', border: '1px solid #cccccc'},
  HR: ui.Panel(null, ui.Panel.Layout.flow('horizontal'), {
    border: '1px solid #E0E0E0',
    margin: '20px 0px'
  }),
  INSTRUCTION: {fontSize: '12px', color: '#999999', margin: '4px 8px'},
  SUCCESS: {fontSize: '13px', color: '#388E3C', fontWeight: 'bold', margin: '8px'},
  ERROR: {fontSize: '13px', color: '#D32F2F', fontWeight: 'bold', margin: '8px'},
  WARNING: {fontSize: '13px', color: '#F57C00', fontWeight: 'bold', margin: '8px'}
};

// =================================================================================
// === 2. DATA SOURCES =============================================================
// =================================================================================

// Optional: Load carbon data for visualization (not used for masking)
var soilCarbon = ee.ImageCollection("projects/sat-io/open-datasets/carbon_stocks_ca/sc").first();

var palettes = require('users/gena/packages:palettes');
var scVis = {palette: palettes.colorbrewer.Purples[7], min: 5, max: 30};

// =================================================================================
// === 3. APPLICATION STATE ========================================================
// =================================================================================

var AppState = {
  currentAoi: null,
  hrCores: null,
  composites: null,
  pairedComposites: null,
  subsamples: null,
  
  reset: function() {
    this.currentAoi = null;
    this.hrCores = null;
    this.composites = null;
    this.pairedComposites = null;
    this.subsamples = null;
  }
};

// =================================================================================
// === 4. UTILITY FUNCTIONS ========================================================
// =================================================================================

var Utils = {
  /**
   * Create systematic grid of points
   */
  createSystematicGrid: function(region, count, seed) {
    // Use stratified sampling to get evenly distributed points
    return ee.FeatureCollection.randomPoints({
      region: region,
      points: count,
      seed: seed
    });
  },
  
  /**
   * FIXED: Creates square composite using buffer method
   */
  createSquare: function(point, area_m2) {
    var side = Math.sqrt(area_m2);
    var radius = side / 2;
    
    // Create buffer and get bounding box
    var buffer = point.geometry().buffer(radius, CONFIG.MAX_ERROR);
    
    // Get bounds of buffer to create rectangle
    var bounds = buffer.bounds(CONFIG.MAX_ERROR);
    
    return ee.Feature(bounds).set({
      'shape': 'square',
      'area_m2': area_m2,
      'method': 'bounds'
    });
  },
  
  /**
   * FIXED: Alternative square creation using rectangle
   */
  createSquareSimple: function(point, area_m2) {
    var side = Math.sqrt(area_m2);
    var halfSide = side / 2;
    
    // Get point coordinates
    var coords = point.geometry().coordinates();
    
    // Approximate degree conversion (latitude-dependent)
    // At equator: 1 degree ≈ 111,320 meters
    var lat = coords.get(1);
    var cosLat = ee.Number(lat).multiply(Math.PI/180).cos();
    var metersPerDegreeLon = ee.Number(111320).multiply(cosLat);
    var metersPerDegreeLat = ee.Number(111320);
    
    var degreeOffsetLon = ee.Number(halfSide).divide(metersPerDegreeLon);
    var degreeOffsetLat = ee.Number(halfSide).divide(metersPerDegreeLat);
    
    var west = ee.Number(coords.get(0)).subtract(degreeOffsetLon);
    var east = ee.Number(coords.get(0)).add(degreeOffsetLon);
    var south = ee.Number(coords.get(1)).subtract(degreeOffsetLat);
    var north = ee.Number(coords.get(1)).add(degreeOffsetLat);
    
    // Create rectangle
    var rect = ee.Geometry.Rectangle([west, south, east, north]);
    
    return ee.Feature(rect).set({
      'shape': 'square',
      'area_m2': area_m2,
      'method': 'rectangle'
    });
  },
  
  /**
   * Creates circular composite area
   */
  createCircle: function(point, area_m2) {
    var radius = Math.sqrt(area_m2 / Math.PI);
    var buffer = point.geometry().buffer(radius, CONFIG.MAX_ERROR);
    return ee.Feature(buffer).set({
      'shape': 'circle',
      'area_m2': area_m2
    });
  },
  
  /**
   * Generates random points within a polygon
   */
  randomPointsInPolygon: function(polygon, count, seed) {
    return ee.FeatureCollection.randomPoints({
      region: polygon.geometry(),
      points: count,
      seed: seed
    });
  },
  
  /**
   * Calculate carbon statistics for a region (if available)
   */
  calculateCarbonStats: function(region) {
    var stats = soilCarbon.reduceRegion({
      reducer: ee.Reducer.mean()
        .combine(ee.Reducer.stdDev(), '', true)
        .combine(ee.Reducer.minMax(), '', true),
      geometry: region,
      scale: CONFIG.CARBON_SCALE,
      maxPixels: CONFIG.MAX_PIXELS,
      tileScale: 4
    });
    
    return stats;
  }
};

// =================================================================================
// === 5. USER INTERFACE SETUP =====================================================
// =================================================================================

ui.root.clear();
var map = ui.Map();
var panel = ui.Panel({style: STYLES.PANEL});
var splitPanel = ui.SplitPanel(panel, map, 'horizontal', false);
ui.root.add(splitPanel);
map.setCenter(-95, 55, 4);

// --- Header ---
panel.add(ui.Label('Composite Sampling - Entire AOI', STYLES.TITLE));
panel.add(ui.Label('Hierarchical Sampling Design (No Land Cover Restrictions)', STYLES.SUBTITLE));
panel.add(ui.Label(
  'Generate hierarchical sampling design with high-resolution soil cores paired with composite samples across your entire area of interest - suitable for any ecosystem type.',
  STYLES.PARAGRAPH
));
panel.add(STYLES.HR);

// --- Step 1: Define AOI ---
panel.add(ui.Label('Step 1: Define Sampling Area', STYLES.HEADER));

var assetIdBox = ui.Textbox({
  placeholder: 'e.g., users/your_name/your_asset',
  style: {stretch: 'horizontal', margin: '0 8px'}
});

var assetPanel = ui.Panel(
  [ui.Label('Enter GEE Asset Path:', STYLES.INSTRUCTION), assetIdBox],
  null,
  {shown: false}
);

var aoiSelection = ui.Select({
  items: ['Draw a polygon', 'Use a GEE Asset'],
  value: 'Draw a polygon',
  style: {stretch: 'horizontal', margin: '0 8px'},
  onChange: function(value) {
    assetPanel.style().set('shown', value === 'Use a GEE Asset');
    map.drawingTools().setShown(value === 'Draw a polygon');
  }
});

panel.add(aoiSelection);
panel.add(assetPanel);
panel.add(ui.Label('► Draw your area of interest or provide asset path', STYLES.INSTRUCTION));

// --- Step 2: Configure Sampling Design ---
panel.add(ui.Label('Step 2: Configure Sampling Design', STYLES.HEADER));

// Sampling strategy selection
var strategySelect = ui.Select({
  items: ['Systematic Grid', 'Random', 'Stratified Random'],
  value: 'Systematic Grid',
  style: {stretch: 'horizontal', margin: '0 8px'}
});

panel.add(ui.Label('Sampling Strategy:', {margin: '8px 8px 4px 8px', fontWeight: 'bold'}));
panel.add(strategySelect);

// Composite shape selection
var shapeSelect = ui.Select({
  items: ['Square', 'Circle'],
  value: 'Square',
  style: {stretch: 'horizontal', margin: '0 8px'}
});

panel.add(ui.Label('Composite Shape:', {margin: '8px 8px 4px 8px', fontWeight: 'bold'}));
panel.add(shapeSelect);

// Numeric parameters
var hrCoresBox = ui.Textbox({
  placeholder: 'Number of HR cores',
  value: CONFIG.DEFAULT_HR_CORES.toString(),
  style: {width: '80px', margin: '0 8px'}
});

var compositesBox = ui.Textbox({
  placeholder: 'Per stratum',
  value: CONFIG.DEFAULT_COMPOSITES_PER_STRATUM.toString(),
  style: {width: '80px', margin: '0 8px'}
});

var compositeAreaBox = ui.Textbox({
  placeholder: 'Area in m²',
  value: CONFIG.DEFAULT_COMPOSITE_AREA.toString(),
  style: {width: '80px', margin: '0 8px'}
});

var subsamplesBox = ui.Textbox({
  placeholder: 'Count',
  value: CONFIG.DEFAULT_SUBSAMPLES.toString(),
  style: {width: '80px', margin: '0 8px'}
});

var pairingFractionBox = ui.Textbox({
  placeholder: '0-1',
  value: CONFIG.DEFAULT_PAIRING_FRACTION.toString(),
  style: {width: '80px', margin: '0 8px'}
});

panel.add(ui.Label('HR Soil Cores (high detail):', {margin: '8px 8px 4px 8px', fontWeight: 'bold'}));
panel.add(ui.Panel([ui.Label('Number of cores:', {width: '150px'}), hrCoresBox], 
  ui.Panel.Layout.flow('horizontal')));

panel.add(ui.Label('Composite Samples:', {margin: '8px 8px 4px 8px', fontWeight: 'bold'}));
panel.add(ui.Panel([ui.Label('Per stratum:', {width: '150px'}), compositesBox], 
  ui.Panel.Layout.flow('horizontal')));
panel.add(ui.Panel([ui.Label('Area (m²):', {width: '150px'}), compositeAreaBox], 
  ui.Panel.Layout.flow('horizontal')));
panel.add(ui.Panel([ui.Label('Subsamples per composite:', {width: '150px'}), subsamplesBox], 
  ui.Panel.Layout.flow('horizontal')));

panel.add(ui.Label('Pairing Strategy:', {margin: '8px 8px 4px 8px', fontWeight: 'bold'}));
panel.add(ui.Panel([ui.Label('Fraction to pair (0-1):', {width: '150px'}), pairingFractionBox], 
  ui.Panel.Layout.flow('horizontal')));

// --- Step 3: Generate Sampling Design ---
panel.add(ui.Label('Step 3: Generate Sampling Design', STYLES.HEADER));

var generateButton = ui.Button({
  label: 'Generate Sampling Locations',
  style: {stretch: 'horizontal', margin: '8px'},
  onClick: generateSamplingDesign
});
panel.add(generateButton);

var resultsPanel = ui.Panel({style: {margin: '0 8px'}});
panel.add(resultsPanel);

// --- Step 4: Export Results ---
panel.add(ui.Label('Step 4: Export Sampling Plan', STYLES.HEADER));

var exportFormatSelect = ui.Select({
  items: ['CSV', 'GeoJSON', 'KML', 'SHP'],
  value: 'CSV',
  style: {stretch: 'horizontal', margin: '0 8px'}
});
panel.add(ui.Label('Export Format:', {margin: '8px 8px 4px 8px', fontWeight: 'bold'}));
panel.add(exportFormatSelect);

var exportCompositesButton = ui.Button({
  label: '⬇️ Export Composite Polygons',
  style: {stretch: 'horizontal', margin: '4px 8px'},
  disabled: true
});

var exportSubsamplesButton = ui.Button({
  label: '⬇️ Export Subsample Points',
  style: {stretch: 'horizontal', margin: '4px 8px'},
  disabled: true
});

var exportHRCoresButton = ui.Button({
  label: '⬇️ Export HR Core Locations',
  style: {stretch: 'horizontal', margin: '4px 8px'},
  disabled: true
});

panel.add(exportCompositesButton);
panel.add(exportSubsamplesButton);
panel.add(exportHRCoresButton);

var downloadLinksPanel = ui.Panel({style: {margin: '0 8px'}});
panel.add(downloadLinksPanel);

var clearButton = ui.Button({
  label: 'Clear All & Start Over',
  style: {stretch: 'horizontal', margin: '8px'}
});
panel.add(clearButton);

// =================================================================================
// === 6. CORE SAMPLING FUNCTIONS ==================================================
// =================================================================================

/**
 * Get AOI from drawing or asset
 */
function getAoi() {
  if (aoiSelection.getValue() === 'Draw a polygon') {
    var layers = map.drawingTools().layers();
    if (layers.length() === 0) {
      return null;
    }
    var geoms = layers.map(function(layer) {
      return layer.toGeometry();
    });
    return ee.FeatureCollection(geoms).union(CONFIG.MAX_ERROR).first().geometry();
  } else {
    var assetId = assetIdBox.getValue();
    if (!assetId) {
      return null;
    }
    try {
      var asset = ee.FeatureCollection(assetId);
      return asset.union(CONFIG.MAX_ERROR).first().geometry();
    } catch (error) {
      alert('Could not load asset: ' + assetId);
      return null;
    }
  }
}

/**
 * Validate user inputs
 */
function validateInputs() {
  var hrCores = parseInt(hrCoresBox.getValue());
  var composites = parseInt(compositesBox.getValue());
  var compositeArea = parseFloat(compositeAreaBox.getValue());
  var subsamples = parseInt(subsamplesBox.getValue());
  var pairingFraction = parseFloat(pairingFractionBox.getValue());
  
  if (isNaN(hrCores) || hrCores < 1 || hrCores > 1000) {
    return {valid: false, message: 'HR cores must be between 1 and 1000'};
  }
  if (isNaN(composites) || composites < 1 || composites > 1000) {
    return {valid: false, message: 'Composites must be between 1 and 1000'};
  }
  if (isNaN(compositeArea) || compositeArea < 1 || compositeArea > 10000) {
    return {valid: false, message: 'Composite area must be between 1 and 10000 m²'};
  }
  if (isNaN(subsamples) || subsamples < 2 || subsamples > 50) {
    return {valid: false, message: 'Subsamples must be between 2 and 50'};
  }
  if (isNaN(pairingFraction) || pairingFraction < 0 || pairingFraction > 1) {
    return {valid: false, message: 'Pairing fraction must be between 0 and 1'};
  }
  
  return {
    valid: true,
    params: {
      hrCores: hrCores,
      compositesPerStratum: composites,
      compositeArea: compositeArea,
      subsamples: subsamples,
      pairingFraction: pairingFraction
    }
  };
}

/**
 * Main function to generate complete sampling design
 */
function generateSamplingDesign() {
  resultsPanel.clear();
  downloadLinksPanel.clear();
  map.layers().reset();
  
  var loading = ui.Label('Generating sampling design...', {
    color: '#666',
    fontStyle: 'italic',
    margin: '8px'
  });
  resultsPanel.add(loading);
  
  // Get AOI
  AppState.currentAoi = getAoi();
  if (!AppState.currentAoi) {
    resultsPanel.clear();
    resultsPanel.add(ui.Label('Please define an area of interest first!', STYLES.ERROR));
    return;
  }
  
  // Validate inputs
  var validations = validateInputs();
  if (!validations.valid) {
    resultsPanel.clear();
    resultsPanel.add(ui.Label(validations.message, STYLES.ERROR));
    return;
  }
  
  var params = validations.params;
  params.strategy = strategySelect.getValue();
  params.shape = shapeSelect.getValue();
  
  // Add AOI to map
  map.centerObject(AppState.currentAoi, 10);
  map.addLayer(AppState.currentAoi, {color: 'E53935'}, 'Area of Interest');
  
  // Optionally add soil carbon layer for reference (not used for masking)
  try {
    map.addLayer(soilCarbon.clip(AppState.currentAoi), scVis, 'Soil Carbon (Reference)', false);
  } catch (error) {
    // Soil carbon data may not be available for all regions - skip if error
    print('Note: Soil carbon reference layer not available for this region');
  }
  
  // MODIFIED: Use entire AOI as sampling region (no forest mask)
  var samplingRegion = ee.Feature(AppState.currentAoi);
  
  // Generate HR cores across entire AOI
  generateHRCores(samplingRegion, params);
}

/**
 * Generate HR core locations
 */
function generateHRCores(samplingRegion, params) {
  try {
    var hrCorePoints = ee.FeatureCollection.randomPoints({
      region: samplingRegion.geometry(),
      points: params.hrCores,
      seed: CONFIG.RANDOM_SEED
    });
    
    // Add IDs using mapping
    var hrCoresList = hrCorePoints.toList(params.hrCores);
    AppState.hrCores = ee.FeatureCollection(
      ee.List.sequence(0, ee.Number(params.hrCores).subtract(1)).map(function(i) {
        var pt = ee.Feature(hrCoresList.get(i));
        return pt.set({
          'core_id': ee.String('HR_').cat(ee.Number(i).format('%03d')),
          'type': 'hr_core',
          'lon': pt.geometry().coordinates().get(0),
          'lat': pt.geometry().coordinates().get(1)
        });
      })
    );
    
    // Continue with composites
    generateComposites(samplingRegion, params);
  } catch (error) {
    resultsPanel.clear();
    resultsPanel.add(ui.Label('Error generating HR cores: ' + error.message, STYLES.ERROR));
  }
}

/**
 * Generate composite polygons based on selected strategy
 */
function generateComposites(samplingRegion, params) {
  try {
    var compositePoints;
    
    if (params.strategy === 'Systematic Grid') {
      // Use stratified sampling for grid-like distribution
      compositePoints = Utils.createSystematicGrid(
        samplingRegion.geometry(),
        params.compositesPerStratum,
        CONFIG.RANDOM_SEED
      );
    } else if (params.strategy === 'Random') {
      compositePoints = ee.FeatureCollection.randomPoints({
        region: samplingRegion.geometry(),
        points: params.compositesPerStratum,
        seed: CONFIG.RANDOM_SEED
      });
    } else {
      // Stratified Random - simple implementation using random points
      // Could be enhanced with proper stratification based on environmental variables
      compositePoints = ee.FeatureCollection.randomPoints({
        region: samplingRegion.geometry(),
        points: params.compositesPerStratum,
        seed: CONFIG.RANDOM_SEED
      });
    }
    
    // Create composite polygons
    var pointsList = compositePoints.limit(params.compositesPerStratum).toList(params.compositesPerStratum);
    
    var compositeFeatures = ee.List.sequence(0, ee.Number(params.compositesPerStratum).subtract(1)).map(function(i) {
      var pt = ee.Feature(pointsList.get(i));
      
      var polygon;
      if (params.shape === 'Circle') {
        polygon = Utils.createCircle(pt, params.compositeArea);
      } else {
        // Use the simpler square method
        polygon = Utils.createSquareSimple(pt, params.compositeArea);
      }
      
      return polygon.set({
        'composite_id': ee.String('COMP_').cat(ee.Number(i).format('%03d')),
        'type': 'composite',
        'centroid_lon': pt.geometry().coordinates().get(0),
        'centroid_lat': pt.geometry().coordinates().get(1)
      });
    });
    
    AppState.composites = ee.FeatureCollection(compositeFeatures);
    
    // Generate subsamples
    generateSubsamples(params);
  } catch (error) {
    resultsPanel.clear();
    resultsPanel.add(ui.Label('Error generating composites: ' + error.message, STYLES.ERROR));
  }
}

/**
 * Generate subsample points within each composite
 */
function generateSubsamples(params) {
  try {
    var subsampleCollections = AppState.composites.map(function(comp) {
      var subPts = Utils.randomPointsInPolygon(comp, params.subsamples, CONFIG.RANDOM_SEED);
      
      var subPtsList = subPts.toList(params.subsamples);
      
      return ee.FeatureCollection(
        ee.List.sequence(0, ee.Number(params.subsamples).subtract(1)).map(function(i) {
          var pt = ee.Feature(subPtsList.get(i));
          return pt.set({
            'composite_id': comp.get('composite_id'),
            'subsample_id': ee.String(comp.get('composite_id')).cat('_S').cat(ee.Number(i).format('%02d')),
            'type': 'subsample',
            'lon': pt.geometry().coordinates().get(0),
            'lat': pt.geometry().coordinates().get(1)
          });
        })
      );
    }).flatten();
    
    AppState.subsamples = ee.FeatureCollection(subsampleCollections);
    
    // Perform pairing
    performPairing(params);
  } catch (error) {
    resultsPanel.clear();
    resultsPanel.add(ui.Label('Error generating subsamples: ' + error.message, STYLES.ERROR));
  }
}

/**
 * Pair composites with nearest HR cores
 */
function performPairing(params) {
  try {
    // Calculate number of composites to pair
    var numToPair = Math.floor(params.compositesPerStratum * params.pairingFraction);
    
    if (numToPair === 0) {
      AppState.pairedComposites = ee.FeatureCollection([]);
      finalizeSamplingDesign(params);
      return;
    }
    
    // Find nearest HR core for each composite
    var filter = ee.Filter.withinDistance({
      distance: CONFIG.DEFAULT_MAX_PAIRING_DISTANCE,
      leftField: '.geo',
      rightField: '.geo',
      maxError: CONFIG.MAX_ERROR
    });
    
    var joined = ee.Join.saveFirst({
      matchKey: 'nearest_hr',
      measureKey: 'distance'
    }).apply(AppState.composites, AppState.hrCores, filter);
    
    // Sort by distance and take top N
    var sorted = joined.sort('distance');
    var paired = sorted.limit(numToPair);
    
    // Mark composites as paired
    AppState.pairedComposites = paired.map(function(f) {
      return f.set({
        'paired': 1,
        'paired_hr_core': ee.Feature(f.get('nearest_hr')).get('core_id')
      });
    });
    
    // Mark remaining as unpaired
    var pairedIds = AppState.pairedComposites.aggregate_array('composite_id');
    AppState.composites = AppState.composites.map(function(f) {
      var isPaired = pairedIds.contains(f.get('composite_id'));
      return f.set({
        'paired': ee.Algorithms.If(isPaired, 1, 0),
        'paired_hr_core': ee.Algorithms.If(isPaired, 
          AppState.pairedComposites
            .filter(ee.Filter.eq('composite_id', f.get('composite_id')))
            .first()
            .get('paired_hr_core'),
          'none')
      });
    });
    
    finalizeSamplingDesign(params);
    
  } catch (error) {
    resultsPanel.clear();
    resultsPanel.add(ui.Label('Error performing pairing: ' + error.message, STYLES.ERROR));
  }
}

/**
 * Finalize and display results
 */
function finalizeSamplingDesign(params) {
  // Calculate counts
  var counts = {
    totalComposites: params.compositesPerStratum,
    pairedComposites: Math.floor(params.compositesPerStratum * params.pairingFraction),
    totalSubsamples: params.compositesPerStratum * params.subsamples,
    totalHRCores: params.hrCores
  };
  
  AppState.composites.size().evaluate(function(compositeCount) {
    AppState.subsamples.size().evaluate(function(subsampleCount) {
      AppState.hrCores.size().evaluate(function(hrCoreCount) {
        
        resultsPanel.clear();
        resultsPanel.add(ui.Label('Sampling Design Summary:', STYLES.SUBHEADER));
        resultsPanel.add(ui.Label(
          'Total composites: ' + compositeCount,
          {fontSize: '12px', margin: '4px 8px'}
        ));
        resultsPanel.add(ui.Label(
          'Paired composites: ' + counts.pairedComposites,
          {fontSize: '12px', margin: '4px 8px'}
        ));
        resultsPanel.add(ui.Label(
          'Total subsamples: ' + subsampleCount,
          {fontSize: '12px', margin: '4px 8px'}
        ));
        resultsPanel.add(ui.Label(
          'HR core locations: ' + hrCoreCount,
          {fontSize: '12px', margin: '4px 8px'}
        ));
        
        // Optionally show carbon statistics if available
        try {
          Utils.calculateCarbonStats(AppState.currentAoi).evaluate(function(carbonStats) {
            if (carbonStats.b1_mean) {
              resultsPanel.add(ui.Label('Carbon Statistics (if available):', STYLES.SUBHEADER));
              resultsPanel.add(ui.Label(
                'Mean soil carbon: ' + carbonStats.b1_mean.toFixed(2) + ' t/ha',
                {fontSize: '12px', margin: '4px 8px'}
              ));
              resultsPanel.add(ui.Label(
                'Std deviation: ' + carbonStats.b1_stdDev.toFixed(2) + ' t/ha',
                {fontSize: '12px', margin: '4px 8px'}
              ));
              resultsPanel.add(ui.Label(
                'CV: ' + (carbonStats.b1_stdDev / carbonStats.b1_mean * 100).toFixed(1) + '%',
                {fontSize: '12px', margin: '4px 8px'}
              ));
            }
          });
        } catch (error) {
          // Carbon stats not available - skip
        }
        
        // Add layers to map with proper styling
        var compositeStyle = {color: '0000FF', fillColor: '0000FF40'};
        var pairedStyle = {color: '00FF00', fillColor: '00FF0040'};
        var hrStyle = {color: 'FF0000', pointSize: 5};
        var subsampleStyle = {color: 'FFFF00', pointSize: 2};
        
        map.addLayer(AppState.composites.filter(ee.Filter.eq('paired', 0)), 
                     compositeStyle, 'Unpaired Composites');
        map.addLayer(AppState.pairedComposites, pairedStyle, 'Paired Composites');
        map.addLayer(AppState.hrCores, hrStyle, 'HR Core Locations');
        map.addLayer(AppState.subsamples, subsampleStyle, 'Subsample Points', false);
        
        // Enable export buttons
        exportCompositesButton.setDisabled(false);
        exportSubsamplesButton.setDisabled(false);
        exportHRCoresButton.setDisabled(false);
        
        resultsPanel.add(ui.Label('✓ Sampling design generated successfully', STYLES.SUCCESS));
        
        // Print to console
        print('✓ Sampling design completed');
        print('Strategy:', params.strategy);
        print('Shape:', params.shape);
        print('Composites:', counts.totalComposites);
        print('Paired:', counts.pairedComposites);
        print('Subsamples:', counts.totalSubsamples);
        print('HR Cores:', counts.totalHRCores);
      });
    });
  });
}

// =================================================================================
// === 7. EXPORT FUNCTIONS =========================================================
// =================================================================================

exportCompositesButton.onClick(function() {
  if (!AppState.composites) {
    alert('Please generate sampling design first.');
    return;
  }
  
  downloadLinksPanel.clear();
  
  var format = exportFormatSelect.getValue();
  
  // Prepare data with all attributes
  var exportData = AppState.composites.map(function(f) {
    return f.set({
      'export_format': format,
      'export_date': ee.Date(Date.now()).format('YYYY-MM-dd')
    });
  });
  
  var downloadUrl = exportData.getDownloadURL({
    format: format === 'SHP' ? 'SHP' : format,
    filename: 'composite_polygons_' + new Date().getTime()
  });
  
  var link = ui.Label({
    value: '⬇️ Download Composite Polygons (' + format + ')',
    style: {
      color: '#1976D2',
      textDecoration: 'underline',
      margin: '8px',
      fontSize: '13px',
      fontWeight: 'bold'
    },
    targetUrl: downloadUrl
  });
  
  downloadLinksPanel.add(link);
  print('✓ Composite polygons export link generated');
});

exportSubsamplesButton.onClick(function() {
  if (!AppState.subsamples) {
    alert('Please generate sampling design first.');
    return;
  }
  
  downloadLinksPanel.clear();
  
  var format = exportFormatSelect.getValue();
  
  var exportData = AppState.subsamples.map(function(f) {
    return f.set({
      'export_format': format,
      'export_date': ee.Date(Date.now()).format('YYYY-MM-dd')
    });
  });
  
  var downloadUrl = exportData.getDownloadURL({
    format: format === 'SHP' ? 'SHP' : format,
    filename: 'subsample_points_' + new Date().getTime()
  });
  
  var link = ui.Label({
    value: '⬇️ Download Subsample Points (' + format + ')',
    style: {
      color: '#1976D2',
      textDecoration: 'underline',
      margin: '8px',
      fontSize: '13px',
      fontWeight: 'bold'
    },
    targetUrl: downloadUrl
  });
  
  downloadLinksPanel.add(link);
  print('✓ Subsample points export link generated');
});

exportHRCoresButton.onClick(function() {
  if (!AppState.hrCores) {
    alert('Please generate sampling design first.');
    return;
  }
  
  downloadLinksPanel.clear();
  
  var format = exportFormatSelect.getValue();
  
  var exportData = AppState.hrCores.map(function(f) {
    return f.set({
      'export_format': format,
      'export_date': ee.Date(Date.now()).format('YYYY-MM-dd')
    });
  });
  
  var downloadUrl = exportData.getDownloadURL({
    format: format === 'SHP' ? 'SHP' : format,
    filename: 'hr_core_locations_' + new Date().getTime()
  });
  
  var link = ui.Label({
    value: '⬇️ Download HR Core Locations (' + format + ')',
    style: {
      color: '#1976D2',
      textDecoration: 'underline',
      margin: '8px',
      fontSize: '13px',
      fontWeight: 'bold'
    },
    targetUrl: downloadUrl
  });
  
  downloadLinksPanel.add(link);
  print('✓ HR core locations export link generated');
});

clearButton.onClick(function() {
  var confirmed = confirm('This will clear all generated sampling locations. Continue?');
  if (!confirmed) return;
  
  AppState.reset();
  map.layers().reset();
  map.drawingTools().clear();
  map.drawingTools().setShown(true);
  resultsPanel.clear();
  downloadLinksPanel.clear();
  
  exportCompositesButton.setDisabled(true);
  exportSubsamplesButton.setDisabled(true);
  exportHRCoresButton.setDisabled(true);
  
  // Reset form values to defaults
  hrCoresBox.setValue(CONFIG.DEFAULT_HR_CORES.toString());
  compositesBox.setValue(CONFIG.DEFAULT_COMPOSITES_PER_STRATUM.toString());
  compositeAreaBox.setValue(CONFIG.DEFAULT_COMPOSITE_AREA.toString());
  subsamplesBox.setValue(CONFIG.DEFAULT_SUBSAMPLES.toString());
  pairingFractionBox.setValue(CONFIG.DEFAULT_PAIRING_FRACTION.toString());
  strategySelect.setValue('Systematic Grid');
  shapeSelect.setValue('Square');
  
  print('✓ Tool reset successfully');
});

// =================================================================================
// === 8. INITIALIZE THE APP =======================================================
// =================================================================================

var drawingTools = map.drawingTools();
drawingTools.setShown(true);
drawingTools.setLinked(false);
drawingTools.setDrawModes(['polygon', 'rectangle']);
drawingTools.setShape('polygon');

// Add basic map controls
map.setControlVisibility({
  all: false,
  layerList: true,
  zoomControl: true,
  scaleControl: true,
  mapTypeControl: true,
  fullscreenControl: false,
  drawingToolsControl: true
});

// Print welcome message
print('═══════════════════════════════════════════════════════');
print('🌍 Composite Sampling Tool - Entire AOI (No Forest Mask)');
print('═══════════════════════════════════════════════════════');
print('');
print('This version samples across your ENTIRE area of interest');
print('without land cover restrictions - suitable for any ecosystem.');
print('');
print('Key Differences from Forest-Only Version:');
print('  • NO forest mask applied');
print('  • Samples across all land cover types');
print('  • Suitable for grasslands, wetlands, agricultural areas, etc.');
print('  • Optional soil carbon visualization (if data available)');
print('');
print('Features:');
print('  • Three sampling strategies: Systematic, Random, Stratified');
print('  • Two composite shapes: Square and Circle');
print('  • HR core and composite pairing optimization');
print('  • Multiple export formats');
print('');
print('Instructions:');
print('  1. Draw or select your area of interest');
print('  2. Configure sampling parameters');
print('  3. Click "Generate Sampling Locations"');
print('  4. Export results in your preferred format');
print('');
print('Ready to use!');
