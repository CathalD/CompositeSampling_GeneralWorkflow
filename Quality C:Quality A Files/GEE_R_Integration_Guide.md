# GEE to R Covariate Export Guide

## Problem Solved
The original GEE script exports **one multi-band GeoTIFF** with all covariates, but the R workflow expects **individual single-band .tif files** (e.g., `NDVI.tif`, `elevation.tif`).

## Two Solutions

---

## ✅ Solution 1: Export Individual Bands from GEE (RECOMMENDED)

### Step 1: Use the Updated GEE Script

1. Open Google Earth Engine Code Editor: https://code.earthengine.google.com
2. Copy and paste the contents of `Carbon_Covariate_Canada_Individual_Bands.js`
3. Update the AOI (Area of Interest):
   ```javascript
   // Option A: Draw your AOI using the geometry tools
   var AOI = geometry;
   
   // Option B: Use an existing asset
   var area = ee.FeatureCollection("your/asset/path");
   var AOI = area.geometry();
   ```

4. Adjust configuration if needed:
   ```javascript
   var CONFIG = {
     yearStart: 2022,        // Change dates
     yearEnd: 2024,
     exportScale: 30,        // Resolution in meters
     exportFolder: 'GEE_CarbonStock_IndividualBands'  // Google Drive folder
   };
   ```

5. Run the script (click ▶️ at top)

### Step 2: Run Export Tasks

1. Click on the **Tasks** tab (top-right corner, orange icon)
2. You'll see ~20-30 export tasks (one per band)
3. Click **RUN** on each task
   - **Tip**: If you have many tasks, use the "Run all" button if available
   - Or use this Chrome extension: [GEE Task Manager](https://chrome.google.com/webstore)

4. Wait for all tasks to complete (🟢 green checkmark)
   - Small areas: 5-15 minutes per band
   - Large areas: 30-60 minutes per band

### Step 3: Download and Organize Files

1. Go to your Google Drive
2. Find the folder: `GEE_CarbonStock_IndividualBands/`
3. Download all `.tif` files
4. Move them to your R project's `covariates/` folder

**File naming:** Each file is named exactly as the band:
- `elevation_m.tif`
- `NDVI_median_annual.tif`
- `LST_median_annual_C.tif`
- `VV_median.tif`
- etc.

### Step 4: Verify in R

```r
# Check your covariates
list.files("covariates", pattern = ".tif$")

# Load one to test
library(terra)
elev <- rast("covariates/elevation_m.tif")
plot(elev)
```

### ✅ Done! Run Module 04

```r
source("04_spatial_modeling.R")
```

---

## 🔄 Solution 2: Convert Multi-Band to Individual Files (If Already Exported)

If you already exported the multi-band composite from GEE, use the R conversion script instead of re-exporting.

### Step 1: Use the Conversion Script

1. Open `convert_multiband_to_individual.R`
2. Update the input file path:
   ```r
   MULTIBAND_FILE <- "path/to/CarbonStock_MultiBand_2022_2024.tif"
   ```

3. Run the script:
   ```r
   source("convert_multiband_to_individual.R")
   ```

### Step 2: Verify Output

The script will:
- Extract all bands from the multi-band file
- Save each as an individual `.tif` file in `covariates/`
- Print a summary of created files

### Step 3: Check Results

```r
# Verify extracted files
verify_covariates("covariates")
```

---

## 📋 Expected Covariate Files

After export, you should have approximately these files in `covariates/`:

**Topographic (5 files):**
- `elevation_m.tif`
- `slope_degrees.tif`
- `aspect_degrees.tif`
- `TPI_500m.tif`
- `TRI.tif`

**Thermal/Landsat (3 files):**
- `LST_median_annual_C.tif`
- `LST_stddev_annual_C.tif`
- `LST_median_growing_C.tif`

**SAR/Sentinel-1 (5 files):**
- `VV_median.tif`
- `VH_median.tif`
- `RVI_median.tif`
- `VV_stddev.tif`
- `VH_stddev.tif`

**Optical/Sentinel-2 (8+ files):**
- `EVI_median_annual.tif`
- `NDVI_median_annual.tif`
- `NDMI_median_annual.tif`
- `GCI_median_annual.tif`
- `EVI_median_growing.tif`
- `NDVI_median_growing.tif`
- `EVI_stddev_annual.tif`
- `EVI_p10.tif`
- `EVI_p90.tif`

**Total: ~20-25 individual .tif files**

---

## 🐛 Troubleshooting

### Issue 1: "Too many bands to export"

**Cause:** GEE has task limits  
**Solution:** The individual band export creates ~20-30 tasks. This is normal. Run them all.

### Issue 2: "Export failed - memory limit"

**Cause:** Area too large or resolution too fine  
**Solutions:**
- Increase `exportScale` (e.g., 30m → 100m)
- Reduce AOI size
- Split AOI into tiles

```javascript
// In GEE config:
CONFIG.exportScale = 100  // Use coarser resolution
```

### Issue 3: R can't find covariates

**Cause:** Files in wrong location  
**Solution:**
```r
# Check working directory
getwd()

# Create covariates folder
dir.create("covariates", showWarnings = FALSE)

# List files
list.files("covariates")
```

### Issue 4: Band names have special characters

**Cause:** GEE export includes characters R doesn't like  
**Solution:** The scripts automatically clean names (e.g., `LST median annual C` → `LST_median_annual_C`)

### Issue 5: Files are huge

**Cause:** Uncompressed exports  
**Solutions:**
- GEE script uses cloud-optimized GeoTIFFs (already compressed)
- If still large, use the conversion script which adds compression:
  ```r
  # In convert_multiband_to_individual.R
  # Already includes: COMPRESS=DEFLATE
  ```

---

## 💡 Tips for Efficient Workflows

### 1. Test with Small AOI First
```javascript
// Draw a small test area
var testAOI = ee.Geometry.Rectangle([-120, 49, -119.9, 49.1]);
CONFIG.aoi = testAOI;
```

### 2. Use Assets for Reusable AOIs
```javascript
// Upload your study area shapefile to GEE assets
// Then reference it:
var studyArea = ee.FeatureCollection('users/yourusername/study_area');
CONFIG.aoi = studyArea.geometry();
```

### 3. Organize Exports by Project
```javascript
CONFIG.exportFolder = 'Project_SiteA_Covariates'  // Descriptive name
```

### 4. Check Band Statistics Before Export
```javascript
// In GEE console, check if values are reasonable:
print('EVI range:', opticalMetrics.select('EVI_median_annual').reduceRegion({
  reducer: ee.Reducer.minMax(),
  geometry: CONFIG.aoi,
  scale: 100
}));
```

---

## 🔗 Quick Reference Links

- [Google Earth Engine](https://code.earthengine.google.com)
- [GEE Data Catalog](https://developers.google.com/earth-engine/datasets)
- [terra R package docs](https://rspatial.github.io/terra/)
- [sf R package docs](https://r-spatial.github.io/sf/)

---

## 📝 Workflow Checklist

- [ ] Updated GEE script with your AOI
- [ ] Ran GEE script successfully
- [ ] All export tasks completed in Tasks tab
- [ ] Downloaded all .tif files from Google Drive
- [ ] Placed files in R project `covariates/` folder
- [ ] Verified files load in R (`terra::rast()`)
- [ ] Checked file dimensions and CRS match
- [ ] Ready to run Module 04!

---

## 🎯 Summary

**Before:** One multi-band .tif file → R workflow broken ❌  
**After:** Individual .tif files per band → R workflow works ✅

**Key Files:**
1. `Carbon_Covariate_Canada_Individual_Bands.js` - Updated GEE script
2. `convert_multiband_to_individual.R` - Backup conversion tool
3. This guide - Complete workflow documentation

**Result:** Seamless integration between GEE exports and R spatial modeling!
