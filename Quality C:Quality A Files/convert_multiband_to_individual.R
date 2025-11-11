# ============================================================================
# Helper Script: Convert Multi-Band GeoTIFF to Individual Band Files
# ============================================================================
# Purpose: Extract individual bands from GEE multi-band export
# Use this if you already exported a multi-band composite from GEE
# ============================================================================

cat("========================================\n")
cat("MULTI-BAND TIFF CONVERTER\n")
cat("========================================\n\n")

# Load required packages
if (!require("terra")) install.packages("terra")
if (!require("raster")) install.packages("raster")
library(terra)
library(raster)

# ============================================================================
# CONFIGURATION
# ============================================================================

# Input file - CHANGE THIS to your multi-band GeoTIFF path
MULTIBAND_FILE <- "path/to/your/CarbonStock_MultiBand_20220101_20241231_20241108.tif"

# Output directory - where individual bands will be saved
OUTPUT_DIR <- "covariates"

# Create output directory if it doesn't exist
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# FUNCTION: Split Multi-Band Raster
# ============================================================================

#' Split multi-band raster into individual single-band files
#' @param input_file Path to multi-band GeoTIFF
#' @param output_dir Directory to save individual bands
#' @return Vector of created file paths
split_multiband_raster <- function(input_file, output_dir = "covariates") {
  
  cat("Reading multi-band raster...\n")
  cat("  Input file:", input_file, "\n")
  
  # Check if file exists
  if (!file.exists(input_file)) {
    stop("Error: Input file not found: ", input_file)
  }
  
  # Try using terra first (faster)
  tryCatch({
    # Read using terra
    r <- terra::rast(input_file)
    
    cat("  Successfully loaded using terra package\n")
    cat("  Number of bands:", terra::nlyr(r), "\n")
    cat("  Band names:", paste(names(r), collapse = ", "), "\n")
    cat("  Dimensions:", paste(dim(r), collapse = " x "), "\n")
    cat("  CRS:", terra::crs(r, describe = TRUE)$name, "\n\n")
    
    # Get band names
    band_names <- names(r)
    
    if (length(band_names) == 0 || all(band_names == "")) {
      warning("Band names are missing. Using default names: band_1, band_2, ...")
      band_names <- paste0("band_", 1:terra::nlyr(r))
      names(r) <- band_names
    }
    
    # Export each band
    cat("Exporting individual bands...\n")
    output_files <- character(length(band_names))
    
    for (i in seq_along(band_names)) {
      band_name <- band_names[i]
      
      # Clean band name for filename
      clean_name <- gsub("[^a-zA-Z0-9_]", "_", band_name)
      output_file <- file.path(output_dir, paste0(clean_name, ".tif"))
      
      # Extract single band
      single_band <- r[[i]]
      
      # Write to file
      terra::writeRaster(
        single_band, 
        output_file, 
        overwrite = TRUE,
        gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")
      )
      
      output_files[i] <- output_file
      
      # Progress
      cat(sprintf("  [%d/%d] %s -> %s\n", 
                  i, length(band_names), band_name, basename(output_file)))
    }
    
    return(output_files)
    
  }, error = function(e) {
    cat("\nTerra failed, trying raster package...\n")
    
    # Fallback to raster package
    r <- raster::brick(input_file)
    
    cat("  Successfully loaded using raster package\n")
    cat("  Number of bands:", raster::nlayers(r), "\n")
    cat("  Band names:", paste(names(r), collapse = ", "), "\n\n")
    
    # Get band names
    band_names <- names(r)
    
    if (length(band_names) == 0 || all(band_names == "")) {
      warning("Band names are missing. Using default names: band_1, band_2, ...")
      band_names <- paste0("band_", 1:raster::nlayers(r))
      names(r) <- band_names
    }
    
    # Export each band
    cat("Exporting individual bands...\n")
    output_files <- character(length(band_names))
    
    for (i in seq_along(band_names)) {
      band_name <- band_names[i]
      
      # Clean band name for filename
      clean_name <- gsub("[^a-zA-Z0-9_]", "_", band_name)
      output_file <- file.path(output_dir, paste0(clean_name, ".tif"))
      
      # Extract single band
      single_band <- raster::raster(r, layer = i)
      
      # Write to file
      raster::writeRaster(
        single_band, 
        output_file, 
        format = "GTiff",
        overwrite = TRUE,
        options = c("COMPRESS=DEFLATE")
      )
      
      output_files[i] <- output_file
      
      # Progress
      cat(sprintf("  [%d/%d] %s -> %s\n", 
                  i, length(band_names), band_name, basename(output_file)))
    }
    
    return(output_files)
  })
}

# ============================================================================
# EXECUTE CONVERSION
# ============================================================================

cat("========================================\n")
cat("STARTING CONVERSION\n")
cat("========================================\n\n")

# Check if input file is specified
if (MULTIBAND_FILE == "path/to/your/CarbonStock_MultiBand_20220101_20241231_20241108.tif") {
  cat("⚠️  ERROR: You need to update MULTIBAND_FILE variable!\n\n")
  cat("Instructions:\n")
  cat("1. Open this script in a text editor\n")
  cat("2. Change MULTIBAND_FILE to your actual file path\n")
  cat("   Example: MULTIBAND_FILE <- 'data/my_covariates.tif'\n")
  cat("3. Run the script again\n\n")
  
  cat("Alternative: Run interactively:\n")
  cat("  file <- file.choose()  # Select your file\n")
  cat("  split_multiband_raster(file, 'covariates')\n\n")
  
} else {
  
  # Run conversion
  result_files <- split_multiband_raster(MULTIBAND_FILE, OUTPUT_DIR)
  
  # Summary
  cat("\n========================================\n")
  cat("CONVERSION COMPLETE\n")
  cat("========================================\n\n")
  
  cat("✓ Extracted", length(result_files), "individual band files\n")
  cat("✓ Output directory:", normalizePath(OUTPUT_DIR), "\n\n")
  
  cat("Files created:\n")
  for (f in result_files) {
    file_size <- file.info(f)$size / 1024 / 1024  # MB
    cat(sprintf("  • %s (%.2f MB)\n", basename(f), file_size))
  }
  
  cat("\n✓ Ready for use in Module 04 spatial modeling!\n\n")
}

# ============================================================================
# VERIFICATION FUNCTION
# ============================================================================

#' Verify extracted bands are valid
#' @param covariate_dir Directory containing band files
verify_covariates <- function(covariate_dir = "covariates") {
  
  cat("\n========================================\n")
  cat("COVARIATE VERIFICATION\n")
  cat("========================================\n\n")
  
  # List all .tif files
  tif_files <- list.files(covariate_dir, pattern = "\\.tif$", full.names = TRUE)
  
  if (length(tif_files) == 0) {
    cat("✗ No .tif files found in", covariate_dir, "\n")
    return(invisible(NULL))
  }
  
  cat("Found", length(tif_files), "GeoTIFF files\n\n")
  
  # Check each file
  results <- data.frame(
    file = character(),
    bands = integer(),
    rows = integer(),
    cols = integer(),
    crs = character(),
    valid = logical(),
    stringsAsFactors = FALSE
  )
  
  for (f in tif_files) {
    tryCatch({
      r <- terra::rast(f)
      
      results <- rbind(results, data.frame(
        file = basename(f),
        bands = terra::nlyr(r),
        rows = nrow(r),
        cols = ncol(r),
        crs = terra::crs(r, describe = TRUE)$code,
        valid = TRUE,
        stringsAsFactors = FALSE
      ))
      
    }, error = function(e) {
      results <- rbind(results, data.frame(
        file = basename(f),
        bands = NA,
        rows = NA,
        cols = NA,
        crs = NA,
        valid = FALSE,
        stringsAsFactors = FALSE
      ))
    })
  }
  
  # Print summary
  cat("Verification Results:\n")
  cat("---------------------\n")
  print(results)
  
  # Check consistency
  cat("\n")
  if (all(results$valid)) {
    cat("✓ All files are valid rasters\n")
  } else {
    cat("✗ Some files failed to load:\n")
    print(results[!results$valid, ])
  }
  
  # Check spatial consistency
  if (length(unique(results$rows[results$valid])) == 1 &&
      length(unique(results$cols[results$valid])) == 1) {
    cat("✓ All rasters have consistent dimensions\n")
  } else {
    cat("⚠️  Warning: Rasters have different dimensions\n")
  }
  
  if (length(unique(results$crs[results$valid])) == 1) {
    cat("✓ All rasters have the same CRS:", unique(results$crs[results$valid]), "\n")
  } else {
    cat("⚠️  Warning: Rasters have different CRS\n")
  }
  
  cat("\n")
  return(results)
}

# ============================================================================
# USAGE EXAMPLES
# ============================================================================

cat("\n========================================\n")
cat("USAGE EXAMPLES\n")
cat("========================================\n\n")

cat("# Convert a multi-band file:\n")
cat("split_multiband_raster('data/my_composite.tif', 'covariates')\n\n")

cat("# Verify extracted files:\n")
cat("verify_covariates('covariates')\n\n")

cat("# Interactive file selection:\n")
cat("file <- file.choose()\n")
cat("split_multiband_raster(file, 'covariates')\n\n")

cat("========================================\n\n")
