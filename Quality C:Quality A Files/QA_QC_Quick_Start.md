# QA/QC Enhanced Carbon Covariate Workflow - Quick Start

## 🎯 What's New

Your GEE script now includes **comprehensive quality control checks** specifically designed for carbon stock estimation. Every covariate comes with quality assessment layers that tell you where predictions are reliable.

---

## 📦 Files You Have

### 1. **Carbon_Covariate_QA_Enhanced.js** (The Script)
- ✅ Fixed CDEM error
- ✅ Individual band exports
- ✅ **8 comprehensive QA/QC checks**
- ✅ Quality score (0-100) for every pixel
- ✅ Automatic validation and reporting

### 2. **Carbon_Covariate_QA_Guide.md** (Complete Documentation)
- Detailed explanation of all QA checks
- Why each check matters for carbon modeling
- How to use QA layers in R
- Reporting templates for stakeholders

---

## ⚡ Quick Start (3 Steps)

### Step 1: Run Enhanced GEE Script

1. Open Google Earth Engine: https://code.earthengine.google.com
2. Paste `Carbon_Covariate_QA_Enhanced.js`
3. Set your DEM source (top of script):
   ```javascript
   demSource: 'CDEM'  // For Canada
   // OR
   demSource: 'SRTM'  // For other regions
   ```
4. Click ▶️ Run

**Watch the console** - You'll see QA statistics printed:
```
QA - Elevation Range: {min: 45, max: 1240}
QA - Sentinel-2 image count: 18
Mean Quality Score: 82.5
High Quality Area (score ≥70): 87.3%
```

### Step 2: Export Tasks

Go to **Tasks tab**, you'll see ~40-50 tasks:
- **Feature bands** (20-25 tasks): elevation, NDVI, LST, etc.
- **QA layers** (15-20 tasks): quality_score, validity flags, etc.

Click "RUN" on all tasks. They'll export to Google Drive folder: `GEE_CarbonStock_QA/`

### Step 3: Use QA Layers in R

```r
library(terra)

# Load a feature
ndvi <- rast("covariates/NDVI_median_annual.tif")

# Load its quality score
quality <- rast("covariates/QA_composite_quality_score.tif")

# Create high-quality mask
high_q_mask <- quality >= 70

# Apply mask
ndvi_clean <- ndvi * high_q_mask

# Extract at field sites
field_data$quality <- extract(quality, cbind(field_data$lon, field_data$lat))

# Filter to high quality only
clean_data <- field_data[field_data$quality >= 70, ]
```

---

## 🔍 What QA Checks Are Included?

| Check | What It Does | Output Layer |
|-------|--------------|--------------|
| **Elevation Range** | Flags impossible elevations | `QA_elevation_valid_flag.tif` |
| **Slope Validation** | Flags steep areas (>30°) | `QA_slope_valid_flag.tif` |
| **Vegetation Index** | Checks NDVI/EVI are valid | `QA_NDVI_valid_flag.tif` |
| **Temperature Range** | Validates LST (-50 to 60°C) | `QA_LST_valid_flag.tif` |
| **SAR Backscatter** | Checks VV/VH in valid range | `QA_VV_valid_flag.tif` |
| **Observation Count** | Ensures ≥10 images per pixel | `QA_S2_obs_count.tif` |
| **Spatial Heterogeneity** | Flags mixed pixels (CV >50%) | `QA_EVI_spatial_CV_pct.tif` |
| **Composite Score** | Overall quality (0-100) | `QA_composite_quality_score.tif` |

---

## 📊 Understanding the Quality Score

**The `composite_quality_score` layer is your most important QA output.**

### Score Interpretation:
- **90-100**: 🟢 Excellent - Use for model training
- **70-89**: 🟡 Good - Use for prediction with confidence
- **50-69**: 🟠 Fair - Use with caution, report uncertainty
- **<50**: 🔴 Poor - Exclude from analysis

### How It's Calculated:
```
Quality Score = Sum of:
  ✓ Elevation valid (15 points)
  ✓ Slope valid (15 points)
  ✓ NDVI valid (15 points)
  ✓ EVI valid (15 points)
  ✓ LST valid (10 points)
  ✓ VV valid (10 points)
  ✓ VH valid (10 points)
  ✓ Sufficient observations (10 points)
= Maximum 100 points
```

---

## 💼 Reporting to Stakeholders

### Example Summary Table:

| Quality Class | Score Range | % of Area | Use Case |
|--------------|-------------|-----------|----------|
| Excellent | 90-100 | 34% | Model training |
| Good | 70-89 | 45% | Confident predictions |
| Fair | 50-69 | 15% | Uncertain predictions |
| Poor | <50 | 6% | Excluded |

### Key Metrics to Report:

```r
# Calculate and report these:
mean_quality <- mean(values(quality), na.rm = TRUE)
pct_high_quality <- sum(values(quality) >= 70, na.rm = TRUE) / 
                    sum(!is.na(values(quality))) * 100
median_obs_count <- median(values(s2_count), na.rm = TRUE)

cat("Mean Quality Score:", round(mean_quality, 1), "\n")
cat("High Quality Area:", round(pct_high_quality, 1), "%\n")
cat("Median S2 Observations:", median_obs_count, "\n")
```

---

## 🎯 Best Practices Checklist

### ✅ Before Modeling:
- [ ] Exported ALL layers (features + QA)
- [ ] Loaded `composite_quality_score` in R/QGIS
- [ ] Visually inspected quality map for patterns
- [ ] Checked >70% of area has quality ≥70
- [ ] Verified field sites are in high-quality areas

### ✅ During Modeling:
- [ ] Filtered training data to quality ≥90
- [ ] Applied quality mask to predictions
- [ ] Used observation counts to weight samples
- [ ] Excluded areas with quality <70

### ✅ When Reporting:
- [ ] Report % area by quality class
- [ ] Show quality map in figures
- [ ] Stratify results by quality
- [ ] Acknowledge excluded low-quality areas
- [ ] Provide uncertainty estimates by quality

---

## 🐛 Quick Troubleshooting

### "Console shows low observation counts"
**Fix:** Increase date range or lower cloud threshold
```javascript
CONFIG.yearStart = 2020;  // More years
CONFIG.s2CloudThreshold = 30;  // Less strict
```

### "Quality score is low across entire area"
**Check:** Are you in a cloudy region? Try longer time period.
**Check:** Is AOI very heterogeneous? This is expected.

### "Some field sites have low quality scores"
**This is normal!** Document which sites, consider:
1. Excluding low-quality sites from analysis
2. Reporting results separately by site quality
3. Using weighted regression (weight by quality)

### "Too many export tasks"
**Alternative:** Export feature groups instead of individual bands, then split in R using `convert_multiband_to_individual.R`

---

## 📈 Improvements Over Original Script

| Feature | Original | QA-Enhanced |
|---------|----------|-------------|
| DEM handling | ❌ Crashed | ✅ Fixed with multiple options |
| Export format | Multi-band | ✅ Individual files |
| Quality checks | Basic | ✅ 8 comprehensive checks |
| Quality scoring | None | ✅ 0-100 composite score |
| Validation | Manual | ✅ Automatic with flags |
| Reporting | None | ✅ Printed statistics |
| Stakeholder-ready | No | ✅ Yes |

---

## 🎓 Key Concepts

### Why QA Matters:
**Without QA:** "Our model predicts 45 Mg/ha carbon stock"
**With QA:** "Our model predicts 45 Mg/ha (95% CI: 38-52) in high-quality areas (78% of study region), with lower confidence in remaining areas due to cloud contamination and steep slopes"

### What Makes Good Covariates:
1. **Sufficient observations** (≥10 images)
2. **Valid value ranges** (no outliers)
3. **Spatial homogeneity** (not mixed pixels)
4. **Temporal consistency** (stable over time)
5. **Complete coverage** (all bands present)

---

## 📚 Next Steps

1. ✅ **Run QA-enhanced script** - Get both features and QA layers
2. ✅ **Visual inspection** - Load quality_score in QGIS
3. ✅ **Apply masks in R** - Filter to quality ≥70
4. ✅ **Stratified analysis** - Separate high/med/low quality
5. ✅ **Report transparently** - Show stakeholders the quality map

---

## 🚀 You're Ready!

Your covariates now come with **built-in quality assurance**. You can:
- ✅ Trust your predictions in high-quality areas
- ✅ Quantify uncertainty spatially
- ✅ Communicate confidence to partners
- ✅ Publish with robust QA documentation

**The enhanced workflow gives you carbon stock estimates that are:**
- **Reliable** (QA-filtered)
- **Transparent** (quality documented)
- **Defensible** (best practices followed)
- **Publishable** (stakeholder-ready)

Happy carbon modeling! 🌱📊
