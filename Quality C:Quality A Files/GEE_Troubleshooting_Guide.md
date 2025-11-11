# GEE Script Troubleshooting Guide

## ✅ FIXED: "Asset 'NRCan/CDEM' is not an Image" Error

### Problem
The original script tried to use `ee.Image('NRCan/CDEM')`, but CDEM is an **ImageCollection**, not a single Image.

### Solution
The fixed script now:
1. Properly mosaics the CDEM ImageCollection
2. Provides multiple DEM options with easy switching
3. Adds visual map layers to verify elevation data

### How to Use the Fixed Script

**For Canada:** (Default)
```javascript
var DEM_SOURCE = 'CDEM';  // Keep this
```

**For Other Regions:**
```javascript
var DEM_SOURCE = 'SRTM';  // Global coverage (most common)
```

**Other Options:**
```javascript
var DEM_SOURCE = 'NASADEM';  // Newer global DEM
var DEM_SOURCE = 'ALOS';     // JAXA's World 3D DEM
```

---

## Common GEE Errors & Solutions

### Error 1: "Computation timed out"

**Cause:** Area too large or computation too complex

**Solutions:**
```javascript
// Option A: Increase scale (coarser resolution)
CONFIG.exportScale = 100;  // Was 30

// Option B: Reduce date range
CONFIG.yearStart = 2023;  // Instead of 2020
CONFIG.yearEnd = 2024;

// Option C: Simplify AOI
var simpleAOI = CONFIG.aoi.simplify(100);  // Simplify polygon
CONFIG.aoi = simpleAOI;

// Option D: Split into tiles
var tile1 = ee.Geometry.Rectangle([-120, 49, -119, 50]);
var tile2 = ee.Geometry.Rectangle([-121, 49, -120, 50]);
// Export each tile separately
```

### Error 2: "Image.clipToBoundsAndScale: Parameter 'input' is required"

**Cause:** Empty ImageCollection (no images found for date range/location)

**Solutions:**
```javascript
// Add error handling
var s2 = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
  .filterBounds(CONFIG.aoi)
  .filterDate(startDate, endDate);

print('Sentinel-2 image count:', s2.size());  // Check if >0

// If count is 0, adjust filters:
CONFIG.s2CloudThreshold = 80;  // Less strict (was 30)
CONFIG.yearStart = 2022;  // More recent data
```

### Error 3: "Collection.mosaic: No bands in collection"

**Cause:** ImageCollection filtering removed all images

**Solutions:**
```javascript
// Check collection size before using
var collection = ee.ImageCollection('...')
  .filterBounds(CONFIG.aoi)
  .filterDate(startDate, endDate);

print('Collection size:', collection.size());

// Only process if images exist
var result = ee.Algorithms.If(
  collection.size().gt(0),
  collection.median(),
  ee.Image.constant(0)  // Fallback
);
```

### Error 4: "User memory limit exceeded"

**Cause:** Too many operations or large exports

**Solutions:**
```javascript
// Reduce spatial operations
CONFIG.qaFocalRadius_pixels = 1;  // Was 3
CONFIG.includeTextureFeatures = false;  // Disable GLCM

// Export in smaller chunks
var bbox = CONFIG.aoi.bounds();
var grid = bbox.coveringGrid(CONFIG.exportCRS, 50000);  // 50km tiles
```

### Error 5: "EEException: Output of image computation is too large"

**Cause:** Export exceeds GEE limits (10,000 x 10,000 pixels at 30m = ~94GB)

**Solutions:**
```javascript
// Check export size BEFORE running
var exportSize = CONFIG.aoi.area().divide(CONFIG.exportScale * CONFIG.exportScale);
print('Approximate pixels:', exportSize);

// If >1e8 (100 million), try:
CONFIG.exportScale = 60;  // Double the scale
CONFIG.maxPixels = 1e9;   // Increase limit (if allowed)
```

### Error 6: "Asset not found"

**Cause:** Incorrect asset path or no access permissions

**Solutions:**
```javascript
// Check asset exists
var testAsset = ee.FeatureCollection('users/yourusername/study_area');
print('Asset info:', testAsset.size());

// If error, verify path:
// - Click Assets tab (left side)
// - Find your asset
// - Copy exact path
// - Must include 'users/yourusername/' or 'projects/projectname/'
```

---

## Debugging Workflow

### Step 1: Check Data Availability
```javascript
// Print collection sizes
print('S2 images:', s2.size());
print('S1 images:', s1.size());
print('Landsat images:', landsat8.size());

// If any are 0, adjust filters
```

### Step 2: Visualize Intermediate Steps
```javascript
// Add layers to map
Map.addLayer(elevation, {min: 0, max: 1000}, 'Elevation');
Map.addLayer(opticalMetrics.select('NDVI_median_annual'), 
  {min: 0, max: 1, palette: ['brown', 'green']}, 'NDVI');

// Check if layers show correctly
```

### Step 3: Test Small Export First
```javascript
// Create tiny test area
var testArea = CONFIG.aoi.centroid().buffer(1000);  // 1km buffer

// Test export
Export.image.toDrive({
  image: allFeatures.select('elevation_m'),
  description: 'TEST_elevation',
  folder: 'GEE_Test',
  region: testArea,
  scale: CONFIG.exportScale,
  crs: CONFIG.exportCRS,
  maxPixels: 1e9
});

// If this works, the full export should too
```

### Step 4: Monitor Task Progress
```javascript
// Tasks tab shows:
// 🔵 Blue = Ready to run
// 🟡 Yellow = Running
// 🟢 Green = Completed
// 🔴 Red = Failed (click for error message)
```

---

## Performance Optimization Tips

### 1. Batch Export Efficiently
```javascript
// Instead of 30 individual bands:
// Export feature groups (fewer tasks)

var groupedExports = {
  'Topographic': topographicFeatures,
  'Optical': opticalMetrics,
  'SAR': sarFeatures,
  'Thermal': lst_metrics
};

// Then split multi-band files in R
```

### 2. Use Optimal Scale
```javascript
// Match sensor resolution:
CONFIG.exportScale = 10;   // Sentinel-2 bands 2-4, 8
CONFIG.exportScale = 30;   // Landsat, most applications
CONFIG.exportScale = 100;  // Regional studies
```

### 3. Pre-filter Before Processing
```javascript
// Filter early to reduce computation
var s2 = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
  .filterBounds(CONFIG.aoi)  // Spatial first
  .filterDate(startDate, endDate)  // Temporal second
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 30))  // Quality third
  .map(maskS2clouds);  // Then mask
```

### 4. Simplify Complex Geometries
```javascript
// If AOI has many vertices
var simpleAOI = CONFIG.aoi.simplify({
  maxError: 100  // Meters
});

CONFIG.aoi = simpleAOI;
print('Vertices reduced:', CONFIG.aoi.coordinates().length());
```

---

## Region-Specific Notes

### Canada
- ✅ Use CDEM (best quality)
- ✅ Sentinel-1/2 available
- ⚠️ Limited Landsat coverage in north

### USA
- ✅ Use SRTM or NASADEM
- ✅ Excellent Landsat coverage
- ✅ All sensors available

### Europe
- ✅ Use SRTM or EU-DEM
- ✅ Dense Sentinel coverage
- ✅ High data quality

### Tropics
- ⚠️ High cloud cover (increase cloud threshold)
- ⚠️ Use longer date ranges
- ✅ SRTM available

### High Latitudes (>60°N/S)
- ⚠️ SRTM not available (use ALOS or NASADEM)
- ⚠️ Limited growing season data
- ⚠️ Fewer satellite passes

---

## Quick Fixes Checklist

When script fails, try in order:

1. **Check console for error message**
   - Red text = error
   - Look for line number

2. **Verify data availability**
   - `print(collection.size())`
   - Must be >0

3. **Simplify configuration**
   - Increase scale to 100m
   - Reduce date range
   - Smaller AOI

4. **Test with single band**
   - Export just elevation
   - If works, issue is with other bands

5. **Check task limits**
   - Max 3000 tasks queued
   - Max 10 running simultaneously

6. **Review asset permissions**
   - Assets must be shared or owned by you

7. **Update script**
   - Copy latest version
   - Clear cache (Ctrl+Shift+R)

---

## Getting Help

**GEE Documentation:** https://developers.google.com/earth-engine  
**GEE Forum:** https://groups.google.com/g/google-earth-engine-developers  
**Data Catalog:** https://developers.google.com/earth-engine/datasets

**Common search terms:**
- "GEE [error message]"
- "Earth Engine mosaic ImageCollection"
- "GEE export memory limit"

---

## Summary: Key Points

✅ **DEM Fixed:** Use `DEM_SOURCE` variable to select appropriate DEM  
✅ **Test Small:** Always test with tiny area first  
✅ **Check Sizes:** Print collection sizes before processing  
✅ **Monitor Tasks:** Watch Tasks tab for errors  
✅ **Be Patient:** Large exports can take hours  

**Remember:** If individual band exports create too many tasks, export feature groups instead, then use the R conversion script to split them!
