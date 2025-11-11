# Quality Control & Assurance for Carbon Stock Covariates

## 📊 Overview

This guide details the QA/QC checks implemented in the enhanced GEE script for generating remote sensing covariates for carbon stock estimation. These checks follow best practices from the carbon modeling literature and ensure your covariates meet data quality standards.

---

## 🎯 Why QA/QC Matters for Carbon Stock Estimation

**Poor quality covariates → Unreliable predictions → Incorrect carbon estimates**

Key risks of skipping QA/QC:
- **Outliers** (clouds, shadows) inflate prediction errors
- **Data gaps** create spatial bias in model training
- **Heterogeneous areas** violate spatial autocorrelation assumptions
- **Sensor artifacts** introduce systematic errors

**With QA/QC:** You can quantify and communicate prediction confidence to stakeholders.

---

## ✅ Implemented QA/QC Checks

### 1. **Elevation Quality Control**

#### Checks:
- **Range validation**: Elevation between -50m and 5000m
- **Slope thresholds**: Flags areas >30° (carbon estimates less reliable)
- **Extreme slopes**: Identifies >45° (exclude from modeling)

#### Why Important for Carbon:
- Steep slopes → soil erosion → unreliable carbon measurements
- Topography affects moisture, temperature, vegetation
- DEM artifacts (e.g., sink holes) create false patterns

#### QA Outputs:
- `elevation_valid_flag` (1 = valid, 0 = suspicious)
- `slope_valid_flag` (1 = slope OK for carbon)
- Printed statistics: min/max/mean elevation

#### Best Practices:
```javascript
// Exclude very steep areas from carbon modeling
var modelingMask = slope.lt(30);  // Keep areas <30° slope
```

---

### 2. **Vegetation Index Quality Control**

#### Checks:
- **NDVI range**: -0.2 to 1.0 (outside = water/urban/clouds)
- **EVI range**: -0.5 to 1.5 (more sensitive to high biomass)
- **Temporal consistency**: Standard deviation flags unstable pixels
- **Spatial heterogeneity**: CV >50% flags mixed pixels

#### Why Important for Carbon:
- Vegetation indices are primary predictors of aboveground carbon
- Outliers often represent clouds/shadows/water bodies
- High spatial CV suggests mixed land cover (less predictable)
- Temporal consistency indicates stable ecosystem (better for carbon)

#### QA Outputs:
- `NDVI_valid_flag` (1 = within valid range)
- `EVI_valid_flag` (1 = within valid range)
- `EVI_spatial_CV_pct` (% coefficient of variation)
- `spatial_homogeneous_flag` (1 = CV <50%)

#### Best Practices:
```javascript
// Mask invalid vegetation indices
var validVeg = ndviQA.and(eviQA).and(spatialHomogeneityFlag);
```

---

### 3. **Land Surface Temperature QA**

#### Checks:
- **Temperature range**: -50°C to 60°C (physical limits)
- **Growing season focus**: Separate annual and growing season metrics
- **Temporal variability**: Standard deviation indicates climate stability

#### Why Important for Carbon:
- Temperature drives decomposition rates
- Extreme temperatures indicate sensor errors or unusual conditions
- Growing season temp better predictor than annual mean
- Temperature variability affects soil respiration

#### QA Outputs:
- `LST_valid_flag` (1 = within physical limits)
- `L8_obs_count` (number of clear observations)
- Printed statistics: temp range and distribution

#### Best Practices:
```javascript
// Require minimum observations for reliable LST
var reliableLST = l8_count.gte(10).and(lstQA);
```

---

### 4. **SAR Backscatter Quality Control**

#### Checks:
- **VV range**: -30 to 5 dB (typical vegetation backscatter)
- **VH range**: -35 to 0 dB (cross-polarization)
- **Speckle filtering**: 7x7 median filter applied
- **Temporal stability**: Standard deviation flags unstable areas

#### Why Important for Carbon:
- SAR penetrates clouds → more observations in tropics
- Backscatter relates to biomass/structure
- Extreme values often indicate water, urban, or artifacts
- VV/VH ratio sensitive to vegetation structure

#### QA Outputs:
- `VV_valid_flag` (1 = within valid range)
- `VH_valid_flag` (1 = within valid range)
- `S1_obs_count` (temporal coverage)

#### Best Practices:
```javascript
// Use both polarizations for quality check
var validSAR = vvQA.and(vhQA).and(s1_count.gte(10));
```

---

### 5. **Observation Count Requirements**

#### Checks:
- **Minimum observations**: 10+ images per pixel (increased from 5)
- **Per-sensor tracking**: Separate counts for Landsat, Sentinel-2, Sentinel-1
- **Temporal gaps**: Identifies areas with sparse coverage

#### Why Important for Carbon:
- More observations → more robust temporal composites
- Reduces influence of single outlier images
- Essential for phenological metrics (EVI amplitude, percentiles)
- Sparse data → higher prediction uncertainty

#### QA Outputs:
- `L8_obs_count` (Landsat thermal observations)
- `S2_obs_count` (Sentinel-2 optical observations)
- `S1_obs_count` (Sentinel-1 SAR observations)
- `thermal_sufficient_flag`, `optical_sufficient_flag`, `sar_sufficient_flag`

#### Best Practices:
```javascript
// Require sufficient data from all sensors
var sufficientData = minObsFlag.reduce(ee.Reducer.min());
```

---

### 6. **Spatial Heterogeneity Assessment**

#### Checks:
- **Focal window CV**: 3x3 pixel coefficient of variation
- **Threshold**: CV >50% flagged as heterogeneous
- **Interpretation**: High CV = mixed pixels or edge effects

#### Why Important for Carbon:
- Heterogeneous pixels mix multiple land covers
- Mixed pixels violate "pure pixel" assumption in regression
- Edge effects create spatial artifacts
- Homogeneous areas have more predictable carbon

#### QA Outputs:
- `EVI_spatial_CV_pct` (0-100+ %, higher = more heterogeneous)
- `spatial_homogeneous_flag` (1 = suitable for modeling)

#### Best Practices:
```javascript
// Prioritize homogeneous areas for model training
var trainingAreas = eviCV.lt(30);  // Very homogeneous
```

---

### 7. **Composite Quality Score**

#### Calculation:
```
Quality Score = 
  elevation_valid (15 points) +
  slope_valid (15 points) +
  NDVI_valid (15 points) +
  EVI_valid (15 points) +
  LST_valid (10 points) +
  VV_valid (10 points) +
  VH_valid (10 points) +
  optical_sufficient (10 points)
= 100 points maximum
```

#### Interpretation:
- **90-100**: Excellent quality, use for model training
- **70-89**: Good quality, suitable for prediction
- **50-69**: Fair quality, use with caution
- **<50**: Poor quality, exclude from analysis

#### Why Important for Carbon:
- Single metric to assess overall data quality
- Facilitates spatial masking decisions
- Enables stratified validation (test on high-quality areas first)
- Transparent communication with stakeholders

#### QA Outputs:
- `composite_quality_score` (0-100)
- Printed statistics: mean, 10th, 50th, 90th percentiles
- Percentage of high-quality area (score ≥70)

#### Best Practices:
```javascript
// Create quality-stratified masks
var highQuality = qualityScore.gte(90);
var mediumQuality = qualityScore.gte(70).and(qualityScore.lt(90));
var lowQuality = qualityScore.lt(70);

// Use high quality for training, medium for prediction
var trainingMask = highQuality;
var predictionMask = mediumQuality.or(highQuality);
```

---

### 8. **Data Completeness Tracking**

#### Checks:
- **Band-level completeness**: All bands present at each pixel
- **Spatial coverage**: % of AOI with complete data
- **Multi-sensor completeness**: All sensors have valid data

#### Why Important for Carbon:
- Random Forest requires complete cases (no NAs)
- Missing bands → reduced sample size → biased models
- Spatial gaps create prediction holes
- Completeness affects uncertainty estimates

#### QA Outputs:
- `data_completeness` (0 or 1 binary mask)
- Printed statistic: % area with complete data
- Per-band availability maps

#### Best Practices:
```javascript
// Only predict where all covariates available
var predictionMask = completeMask.eq(1).and(qualityScore.gte(70));
```

---

## 🛠️ How to Use QA Layers in Your Workflow

### Step 1: Export and Download

Run the QA-enhanced script, which creates:
- **Feature bands** (elevation, NDVI, LST, etc.)
- **QA layers** (quality_score, observation counts, validity flags)

### Step 2: Load QA Layers in R

```r
library(terra)

# Load quality score
quality <- rast("covariates/QA_composite_quality_score.tif")

# Load observation counts
s2_count <- rast("covariates/QA_S2_obs_count.tif")

# Load validity flags
ndvi_valid <- rast("covariates/QA_NDVI_valid_flag.tif")
```

### Step 3: Create Quality Masks

```r
# High quality mask (score ≥70, sufficient observations)
high_quality_mask <- (quality >= 70) & (s2_count >= 10)

# Apply mask to covariates
elevation_masked <- elevation * high_quality_mask

# Count valid pixels
n_valid <- sum(values(high_quality_mask), na.rm = TRUE)
cat("High quality pixels:", n_valid, "\n")
```

### Step 4: Extract Training Data with QA

```r
# Load your field data
field_data <- readRDS("outputs/processed_cores.rds")

# Extract covariates AND quality score
covariates <- terra::extract(
  c(elevation, ndvi, quality), 
  cbind(field_data$longitude, field_data$latitude)
)

# Filter to high-quality training data only
training_data <- field_data[covariates$composite_quality_score >= 70, ]

cat("Samples retained:", nrow(training_data), "/", nrow(field_data), "\n")
```

### Step 5: Stratified Model Validation

```r
# Split by quality for validation
high_q <- training_data[training_data$quality_score >= 90, ]
med_q <- training_data[training_data$quality_score >= 70 & 
                       training_data$quality_score < 90, ]

# Train on high quality
model_train <- high_q

# Validate on medium quality (independent test)
model_test <- med_q
```

### Step 6: Report Uncertainty by Quality

```r
# Predict with quality stratification
predictions$quality_class <- cut(
  predictions$quality_score,
  breaks = c(0, 50, 70, 90, 100),
  labels = c("Low", "Fair", "Good", "Excellent")
)

# Report prediction uncertainty by quality
library(dplyr)
uncertainty_by_quality <- predictions %>%
  group_by(quality_class) %>%
  summarize(
    mean_prediction = mean(carbon_stock),
    sd_prediction = sd(carbon_stock),
    cv_percent = (sd_prediction / mean_prediction) * 100,
    n_pixels = n()
  )

print(uncertainty_by_quality)
```

---

## 📋 QA/QC Checklist Before Carbon Modeling

### ✅ Pre-Processing Checks

- [ ] **Printed QA statistics reviewed** (elevation range, temp range, etc.)
- [ ] **Image counts adequate** (S2 ≥10, S1 ≥10, L8 ≥10)
- [ ] **Quality score distribution acceptable** (mean ≥70)
- [ ] **Data completeness >80%** of AOI
- [ ] **No systematic patterns in QA flags** (e.g., all north flagged as low quality)

### ✅ Covariate-Specific Checks

- [ ] **Elevation**: No extreme outliers, reasonable range for study area
- [ ] **Slope**: <30° dominates (if modeling vegetation carbon)
- [ ] **NDVI/EVI**: Range is [0, 0.9] for most vegetated areas
- [ ] **LST**: Temperatures match local climate expectations
- [ ] **SAR**: No large areas with extreme backscatter (check for water)

### ✅ Spatial Pattern Checks

- [ ] **Visual inspection**: Load quality_score in QGIS, check for artifacts
- [ ] **No stripes/gaps**: Regular patterns indicate sensor/processing issues
- [ ] **Spatial CV map**: Heterogeneous areas align with land cover edges
- [ ] **Observation count map**: Gaps don't align with sampling locations

### ✅ Final Validation

- [ ] **Compare field locations to quality score**: Are samples in high-quality areas?
- [ ] **Check spatial coverage**: Do predictions cover entire AOI at acceptable quality?
- [ ] **Document quality thresholds used**: Record in metadata for reproducibility
- [ ] **Plan stratified reporting**: Separate results by quality class

---

## 🔬 Advanced QA: Detecting Systematic Issues

### Issue 1: Temporal Mismatch

**Symptom:** Field data from 2023, but Sentinel-2 data from 2020-2021

**Solution:**
```javascript
// In GEE, match temporal extent to field data
CONFIG.yearStart = 2023;
CONFIG.yearEnd = 2023;
```

**Check in R:**
```r
# Verify covariate dates match field campaign
print(paste("Field data:", range(field_data$date)))
print(paste("Covariate years:", CONFIG$yearStart, "-", CONFIG$yearEnd))
```

### Issue 2: Spatial Misregistration

**Symptom:** Field GPS coordinates don't align with high-quality areas

**Detection:**
```r
# Extract quality at field locations
field_quality <- terra::extract(
  quality, 
  cbind(field_data$longitude, field_data$latitude)
)

# Check if field sites are in low-quality areas
mean(field_quality$composite_quality_score < 70)  # Should be <20%
```

**Solution:** Check GPS accuracy, consider spatial buffer

### Issue 3: Cloud Contamination

**Symptom:** Low observation counts despite long time period

**Detection:**
```r
# Check observation count distribution
summary(s2_count)

# Areas with <5 observations suspicious
low_obs_pct <- sum(values(s2_count) < 5, na.rm = TRUE) / 
               sum(!is.na(values(s2_count)))
```

**Solution:** Increase date range or lower cloud threshold in GEE

### Issue 4: Seasonal Bias

**Symptom:** All observations from winter (for growing season model)

**Detection:** Check `growing_season_obs_count` vs `annual_obs_count`

**Solution:**
```javascript
// In GEE, ensure growing season has sufficient data
print('Growing season observations:', s2_growing.size());
// Should be at least 30% of annual total
```

---

## 📊 Reporting QA/QC to Stakeholders

### Example Report Section:

> **Data Quality Assessment**
> 
> We implemented comprehensive quality control following best practices for carbon stock modeling:
> 
> - **Temporal Coverage**: 2022-2024 (3 years), minimum 10 observations per pixel
> - **Quality Score**: 78.5% of study area achieved "good" quality (score ≥70/100)
> - **Data Completeness**: 92.3% of area has all covariate layers present
> - **Spatial Heterogeneity**: 85.1% of area is spatially homogeneous (CV <50%)
> - **Observation Density**: Median 15 Sentinel-2 images per pixel (range: 10-24)
> 
> **Quality Stratification**:
> - Excellent (≥90): 34% of area, used for model training
> - Good (70-89): 45% of area, used for prediction with confidence
> - Fair (50-69): 15% of area, predictions reported with uncertainty
> - Poor (<50): 6% of area, excluded from analysis
> 
> All predictions are accompanied by quality scores, enabling transparent communication of spatial confidence in carbon stock estimates.

---

## 🚀 Summary: Key Takeaways

1. **Always export and use QA layers** - They're essential for reliable predictions
2. **Quality score ≥70 is the minimum** for carbon modeling
3. **Stratify by quality** - Report results separately for high/medium/low quality
4. **Document thresholds** - Record all QA decisions for reproducibility
5. **Visual inspection is critical** - Don't rely on statistics alone
6. **Communicate uncertainty** - Use quality layers to show prediction confidence

---

## 📚 References & Best Practices

**Key Papers on Remote Sensing QA for Carbon:**
- Pflugmacher et al. (2012): Using Landsat Time Series for Monitoring
- Zhu & Woodcock (2014): Cloud and Shadow Detection
- Foga et al. (2017): Cloud Detection Algorithm Comparison
- Kennedy et al. (2018): Implementation of the LandTrendr Algorithm

**Best Practice Guidelines:**
- GOFC-GOLD Carbon Monitoring Guide
- IPCC Guidelines for National Greenhouse Gas Inventories
- UN-REDD Technical Guidelines

**Quality Thresholds Used:**
- Minimum observations: 10 (Fassnacht et al. 2016)
- Spatial CV threshold: 50% (Chen et al. 2020)
- Quality score: 70/100 (this framework, conservative)

---

## 💡 Next Steps

1. **Run the QA-enhanced GEE script**
2. **Download ALL outputs** (features + QA layers)
3. **Perform visual QA** in QGIS or R
4. **Apply quality masks** before modeling
5. **Report results stratified by quality**

Your carbon stock estimates will be more reliable, defensible, and publishable! 🎯
