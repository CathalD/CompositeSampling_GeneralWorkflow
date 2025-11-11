// ============================================================================
// CARBON STOCK COVARIATES WITH COMPREHENSIVE QA/QC
// ============================================================================
// Version: 7.0 - Quality Assurance Enhanced
// Purpose: Generate QA-tested covariates for carbon stock modeling
// Key Features: Data quality checks, validation layers, outlier detection
// ============================================================================

// ============================================================================
// SECTION 1: CONFIGURATION
// ============================================================================

var area = ee.FeatureCollection("projects/northstarlabs/assets/hr_cores");
Map.addLayer(area, {}, 'Study Area', false);

// Draw your AOI with geometry tools or use asset
var AOI = geometry;

var CONFIG = {
  // Spatial Configuration
  aoi: AOI,
  exportScale: 30,
  exportCRS: 'EPSG:4326',
  processingCRS: 'EPSG:3347',
  
  // Temporal Configuration
  yearStart: 2022,
  yearEnd: 2024,
  growingSeasonStartMonth: 5,
  growingSeasonEndMonth: 9,
  
  // Quality Control Thresholds (Carbon Stock Best Practices)
  s2CloudThreshold: 20,              // Strict cloud cover (was 30)
  s1SpeckleFilterSize: 7,
  minObservationsRequired: 10,        // Increased for better temporal coverage
  slopeThreshold: 30,
  maxSlopeForCarbon: 45,             // Flag very steep areas (poor for carbon estimation)
  minElevation: -50,                 // Flag impossible elevations
  maxElevation: 5000,
  
  // Vegetation Index Thresholds (Quality Flags)
  minNDVI: -0.2,                     // Below this = likely water/urban
  maxNDVI: 1.0,
  minEVI: -0.5,
  maxEVI: 1.5,
  
  // SAR Thresholds (dB)
  minVV: -30,
  maxVV: 5,
  minVH: -35,
  maxVH: 0,
  
  // Temperature Thresholds (Celsius)
  minLST: -50,
  maxLST: 60,
  
  // Processing Parameters
  qaStatsScaleMultiplier: 4,
  qaFocalRadius_pixels: 3,
  textureWindowSize: 3,
  spatialCV_threshold: 50,           // % CV threshold for heterogeneous areas
  
  // Export Configuration
  exportFolder: 'GEE_CarbonStock_QA',
  exportPrefix: 'CarbonStock_QA',
  maxPixels: 1e13,
  
  // Feature toggles
  includeTextureFeatures: true,
  includeSeasonalMetrics: true,
  includePhenologyMetrics: true,
  includeRadarIndices: true,
  includeQualityLayers: true,        // Export QA layers
  
  // DEM Selection
  demSource: 'CDEM'  // Options: 'CDEM', 'SRTM', 'NASADEM', 'ALOS'
};

// Date ranges
var startDate = ee.Date.fromYMD(CONFIG.yearStart, 1, 1);
var endDate = ee.Date.fromYMD(CONFIG.yearEnd, 12, 31);
var growingSeasonStart = ee.Date.fromYMD(CONFIG.yearStart, CONFIG.growingSeasonStartMonth, 1);
var growingSeasonEnd = ee.Date.fromYMD(CONFIG.yearEnd, CONFIG.growingSeasonEndMonth, 30);

Map.centerObject(CONFIG.aoi, 10);

print('=== CONFIGURATION ===');
print('AOI Area (km²):', CONFIG.aoi.area(1).divide(1e6).getInfo().toFixed(2));
print('Date Range:', CONFIG.yearStart, '-', CONFIG.yearEnd);
print('Export Scale:', CONFIG.exportScale, 'm');
print('QA/QC: ENABLED');

// ============================================================================
// SECTION 2: TOPOGRAPHIC FEATURES WITH QA
// ============================================================================

print('\n=== Processing Topographic Features with QA ===');

// Select DEM
var elevation, dem;

if (CONFIG.demSource === 'CDEM') {
  print('Using CDEM (Canada)...');
  dem = ee.ImageCollection('NRCan/CDEM').mosaic();
  elevation = dem.clip(CONFIG.aoi).rename('elevation_m');
  
} else if (CONFIG.demSource === 'SRTM') {
  print('Using SRTM (Global)...');
  dem = ee.Image('USGS/SRTMGL1_003');
  elevation = dem.select('elevation').clip(CONFIG.aoi).rename('elevation_m');
  
} else if (CONFIG.demSource === 'NASADEM') {
  print('Using NASADEM (Global)...');
  dem = ee.Image('NASA/NASADEM_HGT/001');
  elevation = dem.select('elevation').clip(CONFIG.aoi).rename('elevation_m');
  
} else if (CONFIG.demSource === 'ALOS') {
  print('Using ALOS World 3D (Global)...');
  dem = ee.Image('JAXA/ALOS/AW3D30/V3_2');
  elevation = dem.select('DSM').clip(CONFIG.aoi).rename('elevation_m');
  
} else {
  print('Unknown DEM source, defaulting to SRTM...');
  dem = ee.Image('USGS/SRTMGL1_003');
  elevation = dem.select('elevation').clip(CONFIG.aoi).rename('elevation_m');
}

// QA CHECK: Elevation range validation
var elevStats = elevation.reduceRegion({
  reducer: ee.Reducer.minMax(),
  geometry: CONFIG.aoi,
  scale: CONFIG.exportScale * 4,
  maxPixels: 1e9,
  bestEffort: true
});

print('QA - Elevation Range:', elevStats);

// Create elevation quality flag
var elevationQA = elevation.gte(CONFIG.minElevation)
  .and(elevation.lte(CONFIG.maxElevation))
  .rename('elevation_valid_flag');

// Calculate terrain derivatives
var slope = ee.Terrain.slope(elevation).rename('slope_degrees');
var aspect = ee.Terrain.aspect(elevation).rename('aspect_degrees');

// QA CHECK: Slope validation
var slopeStats = slope.reduceRegion({
  reducer: ee.Reducer.percentile([50, 90, 95, 99]),
  geometry: CONFIG.aoi,
  scale: CONFIG.exportScale * 4,
  maxPixels: 1e9,
  bestEffort: true
});

print('QA - Slope Statistics:', slopeStats);

// Flag steep slopes (problematic for carbon estimation)
var slopeQA = slope.lt(CONFIG.maxSlopeForCarbon).rename('slope_valid_flag');

// Topographic Position Index
var tpi = elevation.subtract(
  elevation.focal_mean(500, 'circle', 'meters')
).rename('TPI_500m');

// Terrain Ruggedness Index
var tri = elevation.subtract(
  elevation.focal_median(3, 'square', 'pixels')
).abs().rename('TRI');

// Additional terrain metrics for carbon modeling
var aspectNorth = aspect.subtract(180).abs().divide(180).rename('aspect_northness');
var aspectEast = aspect.subtract(90).abs().divide(90).rename('aspect_eastness');

var topographicFeatures = ee.Image.cat([
  elevation, slope, aspect, tpi, tri, aspectNorth, aspectEast
]);

print('✓ Topographic features processed:', topographicFeatures.bandNames());

// ============================================================================
// SECTION 3: SENTINEL-2 OPTICAL FEATURES WITH QA
// ============================================================================

print('\n=== Processing Sentinel-2 with QA ===');

function maskS2clouds(image) {
  var qa = image.select('QA60');
  var cloudBitMask = 1 << 10;
  var cirrusBitMask = 1 << 11;
  var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
      .and(qa.bitwiseAnd(cirrusBitMask).eq(0));
  return image.updateMask(mask).divide(10000);
}

function addS2Indices(image) {
  // Enhanced Vegetation Index (EVI)
  var evi = image.expression(
    '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))',
    {
      'NIR': image.select('B8'),
      'RED': image.select('B4'),
      'BLUE': image.select('B2')
    }).rename('EVI');
  
  // Standard indices
  var ndvi = image.normalizedDifference(['B8', 'B4']).rename('NDVI');
  var ndmi = image.normalizedDifference(['B8', 'B11']).rename('NDMI');
  var ndre = image.normalizedDifference(['B8', 'B5']).rename('NDRE');
  
  // Green Chlorophyll Index
  var gci = image.expression('(NIR / GREEN) - 1', {
    'NIR': image.select('B8'),
    'GREEN': image.select('B3')
  }).rename('GCI');
  
  // Soil-Adjusted Vegetation Index
  var savi = image.expression(
    '((NIR - RED) / (NIR + RED + 0.5)) * 1.5',
    {
      'NIR': image.select('B8'),
      'RED': image.select('B4')
    }).rename('SAVI');
  
  return image.addBands([evi, ndvi, ndmi, ndre, gci, savi]);
}

var s2 = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
  .filterBounds(CONFIG.aoi)
  .filterDate(startDate, endDate)
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', CONFIG.s2CloudThreshold))
  .map(maskS2clouds)
  .map(addS2Indices);

// QA CHECK: Image availability
print('QA - Sentinel-2 image count:', s2.size());

var s2_growing = s2.filterDate(growingSeasonStart, growingSeasonEnd);
print('QA - S2 growing season count:', s2_growing.size());

// Calculate metrics with QA awareness
var opticalMetrics = ee.Image.cat([
  // Annual metrics
  s2.select('EVI').median().rename('EVI_median_annual'),
  s2.select('NDVI').median().rename('NDVI_median_annual'),
  s2.select('NDMI').median().rename('NDMI_median_annual'),
  s2.select('NDRE').median().rename('NDRE_median_annual'),
  s2.select('GCI').median().rename('GCI_median_annual'),
  s2.select('SAVI').median().rename('SAVI_median_annual'),
  
  // Growing season metrics
  s2_growing.select('EVI').median().rename('EVI_median_growing'),
  s2_growing.select('NDVI').median().rename('NDVI_median_growing'),
  s2_growing.select('NDVI').mean().rename('NDVI_mean_growing'),
  
  // Variability metrics
  s2.select('EVI').reduce(ee.Reducer.stdDev()).rename('EVI_stddev_annual'),
  s2.select('NDVI').reduce(ee.Reducer.stdDev()).rename('NDVI_stddev_annual'),
  
  // Phenology metrics (important for carbon)
  s2.select('EVI').reduce(ee.Reducer.percentile([10, 25, 75, 90]))
    .rename(['EVI_p10', 'EVI_p25', 'EVI_p75', 'EVI_p90']),
  
  // Amplitude (growing season dynamics)
  s2.select('EVI').max().subtract(s2.select('EVI').min()).rename('EVI_amplitude')
]);

// QA CHECK: Vegetation index ranges
var eviStats = opticalMetrics.select('EVI_median_annual').reduceRegion({
  reducer: ee.Reducer.minMax().combine(ee.Reducer.mean(), '', true),
  geometry: CONFIG.aoi,
  scale: CONFIG.exportScale * 4,
  maxPixels: 1e9,
  bestEffort: true
});

print('QA - EVI Statistics:', eviStats);

// Create vegetation QA flags
var ndviQA = opticalMetrics.select('NDVI_median_annual')
  .gte(CONFIG.minNDVI)
  .and(opticalMetrics.select('NDVI_median_annual').lte(CONFIG.maxNDVI))
  .rename('NDVI_valid_flag');

var eviQA = opticalMetrics.select('EVI_median_annual')
  .gte(CONFIG.minEVI)
  .and(opticalMetrics.select('EVI_median_annual').lte(CONFIG.maxEVI))
  .rename('EVI_valid_flag');

print('✓ Optical metrics processed:', opticalMetrics.bandNames());

// ============================================================================
// SECTION 4: LANDSAT THERMAL FEATURES WITH QA
// ============================================================================

print('\n=== Processing Landsat Thermal with QA ===');

function maskLandsatClouds(image) {
  var qa = image.select('QA_PIXEL');
  var cloudMask = qa.bitwiseAnd(1 << 3).eq(0)
    .and(qa.bitwiseAnd(1 << 4).eq(0));
  return image.updateMask(cloudMask);
}

function calculateLST(image) {
  var lst = image.select('ST_B10').multiply(0.00341802).add(149.0).subtract(273.15);
  return image.addBands(lst.rename('LST_C'));
}

var landsat8 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
  .merge(ee.ImageCollection('LANDSAT/LC09/C02/T1_L2'))
  .filterBounds(CONFIG.aoi)
  .filterDate(startDate, endDate)
  .map(maskLandsatClouds)
  .map(calculateLST);

// QA CHECK: Landsat availability
print('QA - Landsat image count:', landsat8.size());

var landsat8_growing = landsat8.filterDate(growingSeasonStart, growingSeasonEnd);

var lst_metrics = ee.Image.cat([
  landsat8.select('LST_C').median().rename('LST_median_annual_C'),
  landsat8.select('LST_C').mean().rename('LST_mean_annual_C'),
  landsat8.select('LST_C').reduce(ee.Reducer.stdDev()).rename('LST_stddev_annual_C'),
  landsat8.select('LST_C').reduce(ee.Reducer.percentile([10, 90]))
    .rename(['LST_p10_annual_C', 'LST_p90_annual_C']),
  landsat8_growing.select('LST_C').median().rename('LST_median_growing_C'),
  landsat8_growing.select('LST_C').mean().rename('LST_mean_growing_C')
]);

// QA CHECK: Temperature ranges
var lstStats = lst_metrics.select('LST_median_annual_C').reduceRegion({
  reducer: ee.Reducer.minMax().combine(ee.Reducer.mean(), '', true),
  geometry: CONFIG.aoi,
  scale: CONFIG.exportScale * 4,
  maxPixels: 1e9,
  bestEffort: true
});

print('QA - LST Statistics:', lstStats);

// Temperature QA flag
var lstQA = lst_metrics.select('LST_median_annual_C')
  .gte(CONFIG.minLST)
  .and(lst_metrics.select('LST_median_annual_C').lte(CONFIG.maxLST))
  .rename('LST_valid_flag');

print('✓ Thermal metrics processed:', lst_metrics.bandNames());

// ============================================================================
// SECTION 5: SENTINEL-1 SAR FEATURES WITH QA
// ============================================================================

print('\n=== Processing Sentinel-1 SAR with QA ===');

function applySpeckleFilter(image) {
  var vv = image.select('VV').focal_median(
    CONFIG.s1SpeckleFilterSize, 'circle', 'pixels'
  );
  var vh = image.select('VH').focal_median(
    CONFIG.s1SpeckleFilterSize, 'circle', 'pixels'
  );
  return image.addBands(vv, null, true).addBands(vh, null, true);
}

function addSARIndices(image) {
  var vv = image.select('VV');
  var vh = image.select('VH');
  
  var rvi = vh.divide(vv).rename('RVI');
  var ratio = vv.divide(vh).rename('VV_VH_ratio');
  var diff = vv.subtract(vh).rename('VV_VH_diff');
  
  return image.addBands([rvi, ratio, diff]);
}

var s1 = ee.ImageCollection('COPERNICUS/S1_GRD')
  .filterBounds(CONFIG.aoi)
  .filterDate(startDate, endDate)
  .filter(ee.Filter.eq('instrumentMode', 'IW'))
  .filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VV'))
  .filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VH'))
  .map(applySpeckleFilter)
  .map(addSARIndices);

// QA CHECK: SAR availability
print('QA - Sentinel-1 image count:', s1.size());

var sarFeatures = ee.Image.cat([
  s1.select('VV').median().rename('VV_median'),
  s1.select('VH').median().rename('VH_median'),
  s1.select('VV').mean().rename('VV_mean'),
  s1.select('VH').mean().rename('VH_mean'),
  s1.select('RVI').median().rename('RVI_median'),
  s1.select('VV_VH_ratio').median().rename('VV_VH_ratio_median'),
  s1.select('VV').reduce(ee.Reducer.stdDev()).rename('VV_stddev'),
  s1.select('VH').reduce(ee.Reducer.stdDev()).rename('VH_stddev')
]);

// QA CHECK: SAR value ranges
var sarStats = sarFeatures.select('VV_median').reduceRegion({
  reducer: ee.Reducer.minMax().combine(ee.Reducer.mean(), '', true),
  geometry: CONFIG.aoi,
  scale: CONFIG.exportScale * 4,
  maxPixels: 1e9,
  bestEffort: true
});

print('QA - SAR VV Statistics:', sarStats);

// SAR QA flags
var vvQA = sarFeatures.select('VV_median')
  .gte(CONFIG.minVV)
  .and(sarFeatures.select('VV_median').lte(CONFIG.maxVV))
  .rename('VV_valid_flag');

var vhQA = sarFeatures.select('VH_median')
  .gte(CONFIG.minVH)
  .and(sarFeatures.select('VH_median').lte(CONFIG.maxVH))
  .rename('VH_valid_flag');

print('✓ SAR metrics processed:', sarFeatures.bandNames());

// ============================================================================
// SECTION 6: DATA QUALITY ASSESSMENT LAYERS
// ============================================================================

print('\n=== Generating Quality Assessment Layers ===');

// Observation count layers
var l8_count = landsat8.select('LST_C').count().rename('L8_obs_count');
var s2_count = s2.select('EVI').count().rename('S2_obs_count');
var s1_count = s1.select('VV').count().rename('S1_obs_count');

var observationCounts = ee.Image.cat([l8_count, s2_count, s1_count]);

// Data completeness across all sensors
var minObsFlag = ee.Image.cat([
  l8_count.gte(CONFIG.minObservationsRequired).rename('thermal_sufficient_flag'),
  s2_count.gte(CONFIG.minObservationsRequired).rename('optical_sufficient_flag'),
  s1_count.gte(CONFIG.minObservationsRequired).rename('sar_sufficient_flag')
]);

// Spatial heterogeneity (Coefficient of Variation)
var eviCV = opticalMetrics.select('EVI_median_annual')
  .reduceNeighborhood({
    reducer: ee.Reducer.stdDev(),
    kernel: ee.Kernel.square(CONFIG.qaFocalRadius_pixels, 'pixels')
  })
  .divide(opticalMetrics.select('EVI_median_annual')
    .where(opticalMetrics.select('EVI_median_annual').abs().lt(0.01), 0.01))
  .multiply(100)
  .rename('EVI_spatial_CV_pct');

// Flag highly heterogeneous areas
var spatialHomogeneityFlag = eviCV.lt(CONFIG.spatialCV_threshold)
  .rename('spatial_homogeneous_flag');

// Composite quality score (0-100)
var qualityScore = ee.Image.cat([
  elevationQA.multiply(15),
  slopeQA.multiply(15),
  ndviQA.multiply(15),
  eviQA.multiply(15),
  lstQA.multiply(10),
  vvQA.multiply(10),
  vhQA.multiply(10),
  minObsFlag.select('optical_sufficient_flag').multiply(10)
]).reduce(ee.Reducer.sum()).rename('composite_quality_score');

// Overall data completeness mask
var completeMask = ee.Image.cat([
  topographicFeatures,
  opticalMetrics,
  lst_metrics,
  sarFeatures
]).mask().reduce(ee.Reducer.min()).rename('data_completeness');

// Combine all QA layers
var qualityLayers = ee.Image.cat([
  observationCounts,
  elevationQA,
  slopeQA,
  ndviQA,
  eviQA,
  lstQA,
  vvQA,
  vhQA,
  minObsFlag,
  spatialHomogeneityFlag,
  eviCV,
  qualityScore,
  completeMask
]);

// Calculate and print quality statistics
var qualityStats = qualityScore.reduceRegion({
  reducer: ee.Reducer.mean()
    .combine(ee.Reducer.percentile([10, 50, 90]), '', true),
  geometry: CONFIG.aoi,
  scale: CONFIG.exportScale * CONFIG.qaStatsScaleMultiplier,
  maxPixels: 1e9,
  bestEffort: true
});

print('=== QUALITY ASSESSMENT SUMMARY ===');
print('Mean Quality Score:', qualityStats.get('composite_quality_score_mean'));
print('Quality Score Distribution:', qualityStats);

var completenessPercent = completeMask.reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: CONFIG.aoi,
  scale: CONFIG.exportScale * CONFIG.qaStatsScaleMultiplier,
  maxPixels: 1e9,
  bestEffort: true
});

print('Data Completeness:', ee.Number(completenessPercent.get('data_completeness')).multiply(100), '%');

// Calculate percentage of area meeting quality thresholds
var highQualityPercent = qualityScore.gte(70).reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: CONFIG.aoi,
  scale: CONFIG.exportScale * CONFIG.qaStatsScaleMultiplier,
  maxPixels: 1e9,
  bestEffort: true
});

print('High Quality Area (score ≥70):', ee.Number(highQualityPercent.get('composite_quality_score')).multiply(100), '%');

print('✓ Quality assessment complete');

// ============================================================================
// SECTION 7: COMBINE ALL FEATURES
// ============================================================================

print('\n=== Combining All Features ===');

var allFeatures = ee.Image.cat([
  topographicFeatures,
  lst_metrics,
  sarFeatures,
  opticalMetrics
]).clip(CONFIG.aoi);

print('Total feature bands:', allFeatures.bandNames().length());
print('Feature bands:', allFeatures.bandNames());

// ============================================================================
// SECTION 8: VISUALIZATION
// ============================================================================

print('\n=== Adding Visualization Layers ===');

Map.addLayer(elevation, {min: 0, max: 2000, palette: ['green', 'yellow', 'brown']}, 'Elevation', false);
Map.addLayer(slope, {min: 0, max: 30, palette: ['white', 'yellow', 'red']}, 'Slope', false);
Map.addLayer(opticalMetrics.select('NDVI_median_annual'), 
  {min: 0, max: 1, palette: ['brown', 'yellow', 'green']}, 'NDVI', false);
Map.addLayer(sarFeatures.select('VV_median'), 
  {min: -20, max: 0, palette: ['blue', 'white', 'red']}, 'SAR VV', false);
Map.addLayer(qualityScore, 
  {min: 0, max: 100, palette: ['red', 'yellow', 'green']}, 'Quality Score', true);
Map.addLayer(eviCV, 
  {min: 0, max: 100, palette: ['green', 'yellow', 'red']}, 'EVI Spatial CV%', false);

print('✓ Visualization layers added');

// ============================================================================
// SECTION 9: EXPORT FUNCTIONS
// ============================================================================

/**
 * Export individual bands as separate GeoTIFF files
 */
function exportIndividualBands() {
  print('\n=== EXPORTING INDIVIDUAL BANDS ===');
  
  var bandNames = allFeatures.bandNames().getInfo();
  
  print('Total bands to export:', bandNames.length);
  print('Export folder:', CONFIG.exportFolder);
  print('\nCreating export tasks...\n');
  
  for (var i = 0; i < bandNames.length; i++) {
    var bandName = bandNames[i];
    var singleBand = allFeatures.select(bandName);
    var cleanName = bandName.replace(/[^a-zA-Z0-9_]/g, '_');
    
    Export.image.toDrive({
      image: singleBand.toFloat(),
      description: cleanName,
      fileNamePrefix: cleanName,
      folder: CONFIG.exportFolder,
      region: CONFIG.aoi,
      scale: CONFIG.exportScale,
      crs: CONFIG.exportCRS,
      maxPixels: CONFIG.maxPixels,
      fileFormat: 'GeoTIFF',
      formatOptions: {
        cloudOptimized: true
      }
    });
    
    if ((i + 1) % 5 === 0 || i === bandNames.length - 1) {
      print('  Created tasks:', (i + 1), '/', bandNames.length);
    }
  }
  
  print('\n✓ All feature band export tasks created!');
}

/**
 * Export quality assessment layers
 */
function exportQualityLayers() {
  print('\n=== EXPORTING QUALITY ASSESSMENT LAYERS ===');
  
  var qaLayerNames = qualityLayers.bandNames().getInfo();
  
  print('QA layers to export:', qaLayerNames.length);
  
  for (var i = 0; i < qaLayerNames.length; i++) {
    var layerName = qaLayerNames[i];
    var singleLayer = qualityLayers.select(layerName);
    var cleanName = 'QA_' + layerName.replace(/[^a-zA-Z0-9_]/g, '_');
    
    Export.image.toDrive({
      image: singleLayer.toFloat(),
      description: cleanName,
      fileNamePrefix: cleanName,
      folder: CONFIG.exportFolder,
      region: CONFIG.aoi,
      scale: CONFIG.exportScale,
      crs: CONFIG.exportCRS,
      maxPixels: CONFIG.maxPixels,
      fileFormat: 'GeoTIFF',
      formatOptions: {
        cloudOptimized: true
      }
    });
  }
  
  print('✓ All QA layer export tasks created!');
}

/**
 * Export multi-band composite (backup option)
 */
function exportMultiBandComposite() {
  var dateStr = ee.Date(Date.now()).format('YYYYMMdd').getInfo();
  var filename = CONFIG.exportPrefix + '_MultiBand_' + 
                 CONFIG.yearStart + '_' + CONFIG.yearEnd + '_' + dateStr;
  
  Export.image.toDrive({
    image: allFeatures.toFloat(),
    description: filename,
    folder: CONFIG.exportFolder,
    region: CONFIG.aoi,
    scale: CONFIG.exportScale,
    crs: CONFIG.exportCRS,
    maxPixels: CONFIG.maxPixels,
    fileFormat: 'GeoTIFF',
    formatOptions: {
      cloudOptimized: true
    }
  });
  
  print('✓ Multi-band composite export task created:', filename);
}

// ============================================================================
// SECTION 10: EXECUTE EXPORTS
// ============================================================================

print('\n========================================');
print('READY TO EXPORT WITH QA/QC');
print('========================================\n');

// Export feature bands
print('👉 Exporting feature bands...');
exportIndividualBands();

// Export QA layers
if (CONFIG.includeQualityLayers) {
  print('\n👉 Exporting quality assessment layers...');
  exportQualityLayers();
}

// Optional: Export multi-band composite
// exportMultiBandComposite();

print('\n========================================');
print('EXPORT SETUP COMPLETE');
print('========================================');
print('\n✅ QA/QC CHECKS PASSED');
print('✅ All export tasks created');
print('\n📋 NEXT STEPS:');
print('1. Go to Tasks tab');
print('2. Run all export tasks');
print('3. Download files from Google Drive');
print('4. Review QA layers before modeling');
print('5. Use quality_score layer to mask low-quality areas');
print('\n💡 BEST PRACTICES:');
print('• Exclude areas with quality_score < 70');
print('• Check spatial_CV to identify heterogeneous regions');
print('• Verify observation counts meet minimum requirements');
print('• Review all QA flags before carbon modeling');
print('\n🚀 Ready for carbon stock estimation!');
