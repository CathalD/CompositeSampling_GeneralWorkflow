# ============================================================================
# Diagnostic Script: Find GeoPackage and Fix Export Issues
# ============================================================================
# Purpose: Locate GeoPackage files and re-export if needed
# ============================================================================

cat("========================================\n")
cat("GEOPACKAGE DIAGNOSTIC & RECOVERY\n")
cat("========================================\n\n")

# ============================================================================
# 1. SEARCH FOR EXISTING GEOPACKAGE FILES
# ============================================================================

cat("Searching for GeoPackage files...\n")
cat("----------------------------------\n\n")

# Define search locations
search_dirs <- c(
  "outputs",
  "outputs/spatial",
  "data_processed",
  ".",
  "logs"
)

# Search for .gpkg files
found_files <- c()

for (dir in search_dirs) {
  if (dir.exists(dir)) {
    files <- list.files(dir, pattern = "\\.gpkg$", full.names = TRUE, recursive = TRUE)
    if (length(files) > 0) {
      found_files <- c(found_files, files)
    }
  }
}

if (length(found_files) > 0) {
  cat("✓ Found GeoPackage files:\n")
  for (f in found_files) {
    size <- file.info(f)$size
    modified <- file.info(f)$mtime
    cat(sprintf("  - %s\n", f))
    cat(sprintf("    Size: %.2f KB\n", size / 1024))
    cat(sprintf("    Modified: %s\n\n", modified))
  }
} else {
  cat("✗ No GeoPackage files found in standard locations\n\n")
}

# ============================================================================
# 2. CHECK LOG FILE FOR EXPORT MESSAGES
# ============================================================================

cat("Checking log files for export information...\n")
cat("----------------------------------------------\n\n")

log_files <- list.files("logs", pattern = "spatial_modeling.*\\.log$", full.names = TRUE)

if (length(log_files) > 0) {
  # Read most recent log
  latest_log <- log_files[which.max(file.info(log_files)$mtime)]
  cat("Reading log:", latest_log, "\n\n")
  
  log_content <- readLines(latest_log)
  export_lines <- grep("GeoPackage|gpkg|Exported spatial", log_content, value = TRUE)
  
  if (length(export_lines) > 0) {
    cat("Export-related log entries:\n")
    for (line in export_lines) {
      cat("  ", line, "\n")
    }
    cat("\n")
  } else {
    cat("No GeoPackage export messages found in log\n\n")
  }
} else {
  cat("No log files found\n\n")
}

# ============================================================================
# 3. CHECK IF SF PACKAGE IS AVAILABLE
# ============================================================================

cat("Checking spatial package availability...\n")
cat("------------------------------------------\n\n")

sf_available <- require("sf", quietly = TRUE)
cat("sf package:", ifelse(sf_available, "✓ Installed", "✗ Not installed"), "\n")

if (!sf_available) {
  cat("\n⚠️  WARNING: The 'sf' package is required for GeoPackage export\n")
  cat("   Install it with: install.packages('sf')\n\n")
}

# ============================================================================
# 4. CHECK FOR PROCESSED DATA
# ============================================================================

cat("\nChecking for processed data...\n")
cat("--------------------------------\n\n")

data_files <- c(
  "outputs/processed_cores.rds",
  "data_processed/combined_cores.rds",
  "outputs/hr_cores_processed.rds",
  "outputs/composite_cores_processed.rds"
)

available_data <- NULL

for (f in data_files) {
  if (file.exists(f)) {
    cat("✓ Found:", f, "\n")
    if (is.null(available_data)) {
      available_data <- f
    }
  } else {
    cat("✗ Not found:", f, "\n")
  }
}

cat("\n")

# ============================================================================
# 5. RE-EXPORT TO GEOPACKAGE (IF DATA EXISTS)
# ============================================================================

if (!is.null(available_data) && sf_available) {
  
  cat("========================================\n")
  cat("RE-EXPORTING TO GEOPACKAGE\n")
  cat("========================================\n\n")
  
  # Load the data
  cat("Loading data from:", available_data, "\n")
  core_data <- readRDS(available_data)
  
  cat("Data loaded:", nrow(core_data), "rows\n")
  cat("Columns:", paste(names(core_data), collapse = ", "), "\n\n")
  
  # Check for coordinate columns
  if (!all(c("latitude", "longitude") %in% names(core_data))) {
    cat("✗ ERROR: Data missing latitude/longitude columns\n")
    cat("Available columns:", paste(names(core_data), collapse = ", "), "\n")
  } else {
    
    # Aggregate to core locations (one point per core)
    cat("Aggregating to unique core locations...\n")
    
    if ("core_id" %in% names(core_data)) {
      core_locations <- core_data %>%
        group_by(core_id, latitude, longitude) %>%
        summarize(
          n_samples = n(),
          mean_soc = mean(soc, na.rm = TRUE),
          total_stock = sum(soc_stock, na.rm = TRUE),
          mean_bd = mean(bd, na.rm = TRUE),
          max_depth = max(depth_bottom, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      core_locations <- core_data %>%
        group_by(latitude, longitude) %>%
        summarize(
          n_samples = n(),
          mean_soc = mean(soc, na.rm = TRUE),
          total_stock = ifelse("soc_stock" %in% names(.), 
                               sum(soc_stock, na.rm = TRUE), NA),
          mean_bd = mean(bd, na.rm = TRUE),
          max_depth = max(depth_bottom, na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    cat("  Unique locations:", nrow(core_locations), "\n\n")
    
    # Create sf object
    cat("Converting to spatial format...\n")
    library(sf)
    
    core_sf <- st_as_sf(
      core_locations,
      coords = c("longitude", "latitude"),
      crs = 4326,  # WGS84
      remove = FALSE  # Keep original lat/lon columns
    )
    
    # Create outputs directory if needed
    dir.create("outputs", showWarnings = FALSE)
    
    # Export to GeoPackage
    output_file <- "outputs/core_locations.gpkg"
    
    cat("Exporting to:", output_file, "\n")
    
    tryCatch({
      st_write(core_sf, output_file, delete_dsn = TRUE, quiet = FALSE)
      
      # Verify export
      file_size <- file.info(output_file)$size
      cat("\n✓ SUCCESS! GeoPackage exported\n")
      cat("  Location:", normalizePath(output_file), "\n")
      cat("  Size:", round(file_size / 1024, 2), "KB\n")
      cat("  Features:", nrow(core_sf), "\n")
      cat("  CRS: WGS84 (EPSG:4326)\n\n")
      
      # Test reading it back
      cat("Testing file integrity...\n")
      test_read <- st_read(output_file, quiet = TRUE)
      cat("✓ File can be read successfully\n")
      cat("  Layers:", st_layers(output_file)$name, "\n\n")
      
      cat("You can now open this file in:\n")
      cat("  - QGIS\n")
      cat("  - ArcGIS\n")
      cat("  - R (using sf::st_read())\n")
      cat("  - Python (using geopandas)\n\n")
      
    }, error = function(e) {
      cat("\n✗ ERROR during export:\n")
      cat("  ", e$message, "\n\n")
      cat("Troubleshooting tips:\n")
      cat("  1. Check disk space\n")
      cat("  2. Ensure outputs/ directory is writable\n")
      cat("  3. Close file if open in another program\n")
      cat("  4. Update sf package: install.packages('sf')\n\n")
    })
  }
  
} else {
  cat("========================================\n")
  cat("CANNOT RE-EXPORT\n")
  cat("========================================\n\n")
  
  if (is.null(available_data)) {
    cat("Reason: No processed data files found\n")
    cat("Solution: Run Module 01 (01_data_prep.R) first\n\n")
  }
  
  if (!sf_available) {
    cat("Reason: sf package not installed\n")
    cat("Solution: Install with install.packages('sf')\n\n")
  }
}

# ============================================================================
# 6. EXPORT ALTERNATIVE FORMATS
# ============================================================================

if (!is.null(available_data) && sf_available && exists("core_sf")) {
  
  cat("========================================\n")
  cat("EXPORTING ALTERNATIVE FORMATS\n")
  cat("========================================\n\n")
  
  # Shapefile
  cat("Exporting to Shapefile...\n")
  tryCatch({
    st_write(core_sf, "outputs/core_locations.shp", delete_dsn = TRUE, quiet = TRUE)
    cat("✓ Shapefile: outputs/core_locations.shp\n")
  }, error = function(e) cat("✗ Shapefile export failed\n"))
  
  # GeoJSON
  cat("Exporting to GeoJSON...\n")
  tryCatch({
    st_write(core_sf, "outputs/core_locations.geojson", delete_dsn = TRUE, quiet = TRUE)
    cat("✓ GeoJSON: outputs/core_locations.geojson\n")
  }, error = function(e) cat("✗ GeoJSON export failed\n"))
  
  # KML
  cat("Exporting to KML...\n")
  tryCatch({
    st_write(core_sf, "outputs/core_locations.kml", delete_dsn = TRUE, quiet = TRUE)
    cat("✓ KML: outputs/core_locations.kml\n")
  }, error = function(e) cat("✗ KML export failed\n"))
  
  # CSV with coordinates
  cat("Exporting to CSV...\n")
  tryCatch({
    csv_data <- core_locations  # Data frame before sf conversion
    write.csv(csv_data, "outputs/core_locations.csv", row.names = FALSE)
    cat("✓ CSV: outputs/core_locations.csv\n")
  }, error = function(e) cat("✗ CSV export failed\n"))
  
  cat("\n")
}

# ============================================================================
# 7. SUMMARY
# ============================================================================

cat("========================================\n")
cat("SUMMARY\n")
cat("========================================\n\n")

cat("Files created in outputs/ directory:\n")
output_files <- list.files("outputs", pattern = "core_locations\\.", full.names = FALSE)
if (length(output_files) > 0) {
  for (f in output_files) {
    cat("  ✓", f, "\n")
  }
} else {
  cat("  ✗ No output files created\n")
}

cat("\n")
cat("To use the GeoPackage in R:\n")
cat("  library(sf)\n")
cat("  cores <- st_read('outputs/core_locations.gpkg')\n")
cat("  plot(cores)\n\n")

cat("Script complete!\n")
cat("========================================\n\n")
