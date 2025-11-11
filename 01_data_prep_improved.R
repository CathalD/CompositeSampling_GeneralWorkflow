# ============================================================================
# Module 01: Data Preparation and Harmonization - IMPROVED VERSION
# VERSION: 2.0 - Production-ready with error handling and validation
# ============================================================================
# Purpose: Load, clean, and prepare high-resolution and composite core data
# Inputs: hr_cores.csv, composite_cores.csv
# Outputs: cleaned data frames, summary statistics
# Author: [Your name]
# Date: Modified on 2024
# ============================================================================

# ============================================================================
# SETUP AND CONFIGURATION
# ============================================================================

# Initialize logging
log_file <- file.path("logs", paste0("data_prep_", Sys.Date(), ".log"))
if (!dir.exists("logs")) dir.create("logs")

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

log_message("Starting Module 01: Data Preparation")

# ============================================================================
# LOAD PACKAGES WITH ERROR HANDLING
# ============================================================================

load_required_package <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required but not installed. Install with: install.packages('%s')", 
                 pkg, pkg))
  }
  log_message(sprintf("Loaded package: %s", pkg))
}

# Load required packages
tryCatch({
  load_required_package("dplyr")
  load_required_package("tidyr")
  load_required_package("ggplot2")
}, error = function(e) {
  log_message(sprintf("Failed to load required packages: %s", e$message), "ERROR")
  stop("Cannot proceed without required packages")
})

# Source helper functions with error handling
tryCatch({
  if (file.exists("helper_functions_improved.R")) {
    source("helper_functions_improved.R")
  } else if (file.exists("helper_functions.R")) {
    source("helper_functions.R")
    log_message("Using original helper_functions.R - consider upgrading to improved version", "WARNING")
  } else {
    stop("Helper functions file not found")
  }
  log_message("Helper functions loaded successfully")
}, error = function(e) {
  log_message(sprintf("Error loading helper functions: %s", e$message), "ERROR")
  stop("Cannot proceed without helper functions")
})

# ============================================================================
# 1. SET PARAMETERS WITH VALIDATION
# ============================================================================

log_message("Setting analysis parameters")

# Define SOC units in your data
SOC_UNITS <- "g_kg"  # Change to "percent" if your data is in %
if (!SOC_UNITS %in% c("g_kg", "percent")) {
  stop("SOC_UNITS must be 'g_kg' or 'percent'")
}

# Define depth ranges with validation
MAX_DEPTH <- 100  # Maximum depth to consider (cm)
if (!is.numeric(MAX_DEPTH) || MAX_DEPTH <= 0) {
  stop("MAX_DEPTH must be a positive number")
}

COMPOSITE_DEPTH_BREAKS <- seq(0, 100, 20)  # Composite depth increments
if (!is.numeric(COMPOSITE_DEPTH_BREAKS) || length(COMPOSITE_DEPTH_BREAKS) < 2) {
  stop("COMPOSITE_DEPTH_BREAKS must be a numeric vector with at least 2 values")
}

log_message(sprintf("Parameters set: SOC_UNITS=%s, MAX_DEPTH=%d", SOC_UNITS, MAX_DEPTH))

# ============================================================================
# 2. LOAD DATA WITH ERROR HANDLING
# ============================================================================

log_message("Loading core data files")

# Function to safely load CSV with validation
load_csv_safe <- function(filename) {
  if (!file.exists(filename)) {
    stop(sprintf("File not found: %s", filename))
  }
  
  tryCatch({
    data <- read.csv(filename, stringsAsFactors = FALSE)
    log_message(sprintf("Loaded %s: %d rows, %d columns", 
                       filename, nrow(data), ncol(data)))
    
    # Standardize column names to lowercase
    names(data) <- tolower(names(data))
    
    # Check for required columns
    required_cols <- c("core_id", "latitude", "longitude", 
                      "depth_top", "depth_bottom", "soc", "bulk_density")
    missing_cols <- setdiff(required_cols, names(data))
    
    if (length(missing_cols) > 0) {
      stop(sprintf("Missing required columns in %s: %s", 
                  filename, paste(missing_cols, collapse = ", ")))
    }
    
    return(data)
    
  }, error = function(e) {
    log_message(sprintf("Error loading %s: %s", filename, e$message), "ERROR")
    stop(sprintf("Failed to load %s", filename))
  })
}

# Load both datasets
hr_cores <- load_csv_safe("hr_cores.csv")
composite_cores <- load_csv_safe("composite_cores.csv")

cat("\nData loaded successfully!\n")
cat("High-resolution cores:", nrow(hr_cores), "samples from", 
    length(unique(hr_cores$core_id)), "cores\n")
cat("Composite cores:", nrow(composite_cores), "samples from", 
    length(unique(composite_cores$core_id)), "cores\n\n")

# ============================================================================
# 3. DATA QUALITY CHECKS WITH DETAILED REPORTING
# ============================================================================

log_message("Running data quality checks")

# Enhanced quality check function
perform_quality_check <- function(data, data_name) {
  log_message(sprintf("Checking quality for %s", data_name))
  
  # Use the enhanced check_data_quality function
  quality_report <- check_data_quality(data, data_name, verbose = TRUE)
  
  # Log critical issues
  if (sum(quality_report$missing_values) > 0) {
    log_message(sprintf("%s has missing values in some columns", data_name), "WARNING")
  }
  
  if (quality_report$negative_values$soc > 0 || quality_report$negative_values$bulk_density > 0) {
    log_message(sprintf("%s has negative values that will be removed", data_name), "WARNING")
  }
  
  return(quality_report)
}

# Check both datasets
hr_quality <- perform_quality_check(hr_cores, "High-Resolution Cores")
comp_quality <- perform_quality_check(composite_cores, "Composite Cores")

# ============================================================================
# 4. DATA CLEANING WITH VALIDATION
# ============================================================================

log_message("Cleaning data")

# Enhanced cleaning function with logging
clean_core_data <- function(data, data_name, max_depth = MAX_DEPTH) {
  
  initial_rows <- nrow(data)
  log_message(sprintf("Cleaning %s: %d initial rows", data_name, initial_rows))
  
  # Validate numeric columns
  numeric_cols <- c("soc", "bulk_density", "depth_top", "depth_bottom", 
                   "latitude", "longitude")
  for (col in numeric_cols) {
    if (col %in% names(data)) {
      data[[col]] <- as.numeric(data[[col]])
      if (sum(is.na(data[[col]])) > 0) {
        log_message(sprintf("Converting %s to numeric created %d NA values", 
                           col, sum(is.na(data[[col]]))), "WARNING")
      }
    }
  }
  
  data_clean <- data %>%
    # Remove missing values in critical columns
    filter(!is.na(soc) & !is.na(bulk_density)) %>%
    # Remove negative values
    filter(soc >= 0 & bulk_density > 0) %>%
    # Remove unrealistic values
    filter(bulk_density <= 3.0) %>%  # Max realistic BD
    filter(soc <= 500) %>%  # Max realistic SOC (500 g/kg = 50%)
    # Filter to maximum depth
    filter(depth_bottom <= max_depth) %>%
    # Ensure core_id is character
    mutate(core_id = as.character(core_id)) %>%
    # Add core type identifier based on naming convention
    mutate(core_type = case_when(
      grepl("HR|hr", core_id) ~ "high_res",
      grepl("COMP|comp", core_id) ~ "composite",
      TRUE ~ "unknown"
    ))
  
  # Check for depth consistency
  depth_issues <- data_clean %>%
    filter(depth_bottom <= depth_top)
  
  if (nrow(depth_issues) > 0) {
    log_message(sprintf("Removing %d rows with depth_bottom <= depth_top", 
                       nrow(depth_issues)), "WARNING")
    data_clean <- data_clean %>%
      filter(depth_bottom > depth_top)
  }
  
  final_rows <- nrow(data_clean)
  removed_rows <- initial_rows - final_rows
  removal_pct <- round(removed_rows / initial_rows * 100, 1)
  
  log_message(sprintf("%s: Removed %d rows (%.1f%%)", 
                     data_name, removed_rows, removal_pct))
  
  if (removal_pct > 20) {
    log_message(sprintf("High data removal rate for %s - please review data quality", 
                       data_name), "WARNING")
  }
  
  return(data_clean)
}

# Clean both datasets
hr_cores_clean <- clean_core_data(hr_cores, "HR cores")
composite_cores_clean <- clean_core_data(composite_cores, "Composite cores")

cat("\nCleaning complete!\n")
cat("High-resolution cores: removed", 
    nrow(hr_cores) - nrow(hr_cores_clean), "samples\n")
cat("Composite cores: removed", 
    nrow(composite_cores) - nrow(composite_cores_clean), "samples\n\n")

# ============================================================================
# 5. CALCULATE DERIVED VARIABLES WITH VALIDATION
# ============================================================================

log_message("Calculating derived variables")

# Enhanced function with error handling
add_derived_variables <- function(data, soc_units = SOC_UNITS) {
  
  log_message(sprintf("Adding derived variables with SOC units: %s", soc_units))
  
  tryCatch({
    data_with_derived <- data %>%
      mutate(
        # Depth midpoint with validation
        depth_mid = calculate_depth_midpoint(depth_top, depth_bottom),
        
        # Depth increment
        depth_increment = depth_bottom - depth_top,
        
        # SOC stock (Mg/ha) with error handling for each row
        soc_stock = mapply(
          function(s, bd, dt, db) {
            tryCatch(
              calculate_soc_stock(s, bd, dt, db, soc_units),
              error = function(e) {
                log_message(sprintf("Error calculating stock for one sample: %s", 
                                   e$message), "WARNING")
                return(NA)
              }
            )
          },
          soc, bulk_density, depth_top, depth_bottom
        )
      )
    
    # Check for calculation issues
    na_stocks <- sum(is.na(data_with_derived$soc_stock))
    if (na_stocks > 0) {
      log_message(sprintf("Failed to calculate SOC stock for %d samples", 
                         na_stocks), "WARNING")
    }
    
    return(data_with_derived)
    
  }, error = function(e) {
    log_message(sprintf("Error adding derived variables: %s", e$message), "ERROR")
    stop("Failed to calculate derived variables")
  })
}

# Add derived variables to both datasets
hr_cores_clean <- add_derived_variables(hr_cores_clean)
composite_cores_clean <- add_derived_variables(composite_cores_clean)

log_message("Derived variables calculated successfully")

# ============================================================================
# 6. AGGREGATE HIGH-RES DATA TO COMPOSITE RESOLUTION
# ============================================================================

log_message("Aggregating high-resolution data to composite resolution")

# Aggregate with error handling
tryCatch({
  hr_cores_aggregated <- aggregate_hr_to_composite(
    hr_cores_clean, 
    composite_depths = COMPOSITE_DEPTH_BREAKS
  )
  
  log_message(sprintf("Aggregation complete: %d original samples -> %d aggregated samples",
                     nrow(hr_cores_clean), nrow(hr_cores_aggregated)))
  
  cat("\nAggregation complete!\n")
  cat("Original HR samples:", nrow(hr_cores_clean), "\n")
  cat("Aggregated HR samples:", nrow(hr_cores_aggregated), "\n\n")
  
}, error = function(e) {
  log_message(sprintf("Error during aggregation: %s", e$message), "ERROR")
  stop("Failed to aggregate high-resolution data")
})

# ============================================================================
# 7. COMBINE DATASETS FOR SPATIAL ANALYSIS
# ============================================================================

log_message("Creating combined dataset for spatial modeling")

tryCatch({
  # Prepare high-res aggregated data with consistent column names
  # NEW CODE (explicit namespace):
  hr_for_spatial <- hr_cores_aggregated %>%
    mutate(
      core_type = "high_res",
      resolution = "aggregated"
    ) %>%
    dplyr::select(  # <-- Add dplyr:: prefix
      core_id, latitude, longitude, 
      depth_top, depth_bottom, depth_mid,
      soc = soc_mean, 
      bulk_density = bd_mean, 
      soc_stock,
      core_type, resolution
    )
  
  # Prepare composite data with consistent structure
  comp_for_spatial <- composite_cores_clean %>%
    mutate(
      core_type = "composite",
      resolution = "original"
    ) %>%
    dplyr::select(  # <-- Add dplyr:: prefix
      core_id, latitude, longitude,
      depth_top, depth_bottom, depth_mid,
      soc, bulk_density, soc_stock,
      core_type, resolution
    )
  
  # Combine datasets
  combined_cores <- bind_rows(hr_for_spatial, comp_for_spatial)
  
  # Add depth_increment for combined dataset
  combined_cores <- combined_cores %>%
    mutate(depth_increment = depth_bottom - depth_top)
  
  log_message(sprintf("Combined dataset created: %d cores, %d samples",
                     length(unique(combined_cores$core_id)), 
                     nrow(combined_cores)))
  
  cat("Combined dataset created!\n")
  cat("Total cores:", length(unique(combined_cores$core_id)), "\n")
  cat("Total samples:", nrow(combined_cores), "\n\n")
  
}, error = function(e) {
  log_message(sprintf("Error creating combined dataset: %s", e$message), "ERROR")
  stop("Failed to create combined dataset")
})

# ============================================================================
# 8. CALCULATE CUMULATIVE SOC STOCKS
# ============================================================================

log_message("Calculating cumulative SOC stocks")

tryCatch({
  hr_cores_clean <- calculate_cumulative_stock(hr_cores_clean)
  composite_cores_clean <- calculate_cumulative_stock(composite_cores_clean)
  combined_cores <- calculate_cumulative_stock(combined_cores)
  
  log_message("Cumulative stocks calculated successfully")
  cat("Cumulative stocks calculated!\n\n")
  
}, error = function(e) {
  log_message(sprintf("Error calculating cumulative stocks: %s", e$message), "WARNING")
  # Non-fatal error - continue without cumulative stocks
})

# ============================================================================
# 9. CREATE SUMMARY STATISTICS WITH VALIDATION
# ============================================================================

log_message("Generating summary statistics")

# Enhanced summary with additional metrics
create_summary_statistics <- function(data) {
  
  # Summary by core type and depth
  summary_by_type <- data %>%
    group_by(core_type, depth_top, depth_bottom) %>%
    summarise(
      n_cores = n_distinct(core_id),
      n_samples = n(),
      soc_mean = mean(soc, na.rm = TRUE),
      soc_median = median(soc, na.rm = TRUE),
      soc_sd = sd(soc, na.rm = TRUE),
      soc_cv = sd(soc, na.rm = TRUE) / mean(soc, na.rm = TRUE) * 100,
      soc_min = min(soc, na.rm = TRUE),
      soc_max = max(soc, na.rm = TRUE),
      bd_mean = mean(bulk_density, na.rm = TRUE),
      bd_sd = sd(bulk_density, na.rm = TRUE),
      stock_mean = mean(soc_stock, na.rm = TRUE),
      stock_sd = sd(soc_stock, na.rm = TRUE),
      stock_total = sum(soc_stock, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  
  # Summary by depth only
  summary_by_depth <- data %>%
    group_by(depth_top, depth_bottom) %>%
    summarise(
      n_samples = n(),
      n_cores = n_distinct(core_id),
      soc_mean = mean(soc, na.rm = TRUE),
      soc_sd = sd(soc, na.rm = TRUE),
      bd_mean = mean(bulk_density, na.rm = TRUE),
      bd_sd = sd(bulk_density, na.rm = TRUE),
      stock_mean = mean(soc_stock, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  
  # Overall summary
  overall_summary <- data %>%
    summarise(
      total_cores = n_distinct(core_id),
      total_samples = n(),
      soc_mean_overall = mean(soc, na.rm = TRUE),
      soc_sd_overall = sd(soc, na.rm = TRUE),
      bd_mean_overall = mean(bulk_density, na.rm = TRUE),
      bd_sd_overall = sd(bulk_density, na.rm = TRUE),
      total_stock_mean = mean(soc_stock, na.rm = TRUE)
    ) %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  
  return(list(
    by_type = summary_by_type,
    by_depth = summary_by_depth,
    overall = overall_summary
  ))
}

# Generate summaries
summaries <- create_summary_statistics(combined_cores)

cat("Summary by core type and depth:\n")
print(summaries$by_type)
cat("\nSummary by depth:\n")
print(summaries$by_depth)
cat("\nOverall summary:\n")
print(summaries$overall)

# ============================================================================
# 10. SAVE PROCESSED DATA WITH VALIDATION
# ============================================================================

log_message("Saving processed data and summaries")

# Function to safely save RDS files
save_rds_safe <- function(object, filename) {
  filepath <- file.path("data_processed", filename)
  tryCatch({
    saveRDS(object, filepath)
    log_message(sprintf("Saved: %s", filepath))
    return(TRUE)
  }, error = function(e) {
    log_message(sprintf("Failed to save %s: %s", filepath, e$message), "ERROR")
    return(FALSE)
  })
}

# Function to safely save CSV files
save_csv_safe <- function(data, filename) {
  filepath <- file.path("outputs", filename)
  tryCatch({
    write.csv(data, filepath, row.names = FALSE)
    log_message(sprintf("Saved: %s", filepath))
    return(TRUE)
  }, error = function(e) {
    log_message(sprintf("Failed to save %s: %s", filepath, e$message), "ERROR")
    return(FALSE)
  })
}

# Save cleaned datasets
save_success <- all(
  save_rds_safe(hr_cores_clean, "hr_cores_clean.rds"),
  save_rds_safe(composite_cores_clean, "composite_cores_clean.rds"),
  save_rds_safe(hr_cores_aggregated, "hr_cores_aggregated.rds"),
  save_rds_safe(combined_cores, "combined_cores.rds"),
  save_csv_safe(summaries$by_type, "summary_by_type.csv"),
  save_csv_safe(summaries$by_depth, "summary_by_depth.csv"),
  save_csv_safe(summaries$overall, "summary_overall.csv")
)

if (!save_success) {
  log_message("Some files failed to save", "WARNING")
}

# ============================================================================
# 11. CREATE SPATIAL DATA FOR GEE
# ============================================================================

log_message("Creating spatial data files")

tryCatch({
  # Create unique location data for each core
  core_locations <- combined_cores %>%
    group_by(core_id, latitude, longitude, core_type) %>%
    summarise(
      total_stock_0_100 = sum(soc_stock, na.rm = TRUE),
      mean_soc = mean(soc, na.rm = TRUE),
      mean_bd = mean(bulk_density, na.rm = TRUE),
      n_samples = n(),
      .groups = "drop"
    ) %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  
  # Validate coordinates
  invalid_coords <- core_locations %>%
    filter(latitude < -90 | latitude > 90 | 
           longitude < -180 | longitude > 180)
  
  if (nrow(invalid_coords) > 0) {
    log_message(sprintf("Found %d cores with invalid coordinates", 
                       nrow(invalid_coords)), "WARNING")
  }
  
  # Save as CSV for import to GEE
  save_csv_safe(core_locations, "core_locations.csv")
  
  cat("\nSpatial data files created!\n")
  cat("Core locations saved to 'outputs/core_locations.csv'\n\n")
  
}, error = function(e) {
  log_message(sprintf("Error creating spatial data: %s", e$message), "WARNING")
})

# ============================================================================
# 12. CREATE DATA QUALITY REPORT
# ============================================================================

log_message("Generating data quality report")

# Create comprehensive quality report
quality_report <- list(
  timestamp = Sys.time(),
  parameters = list(
    soc_units = SOC_UNITS,
    max_depth = MAX_DEPTH,
    composite_breaks = COMPOSITE_DEPTH_BREAKS
  ),
  input_data = list(
    hr_cores = list(
      original_rows = nrow(hr_cores),
      cleaned_rows = nrow(hr_cores_clean),
      removal_rate = round((nrow(hr_cores) - nrow(hr_cores_clean)) / 
                          nrow(hr_cores) * 100, 1)
    ),
    composite_cores = list(
      original_rows = nrow(composite_cores),
      cleaned_rows = nrow(composite_cores_clean),
      removal_rate = round((nrow(composite_cores) - nrow(composite_cores_clean)) / 
                          nrow(composite_cores) * 100, 1)
    )
  ),
  output_data = list(
    combined_cores = nrow(combined_cores),
    unique_cores = length(unique(combined_cores$core_id)),
    depth_range = range(combined_cores$depth_bottom)
  ),
  summaries = summaries
)

saveRDS(quality_report, file.path("outputs", "data_quality_report.rds"))

# ============================================================================
# 13. CLEANUP AND FINAL REPORTING
# ============================================================================

# Clear large objects from memory
rm(hr_cores, composite_cores)  # Remove raw data
gc()  # Force garbage collection

log_message("Memory cleanup completed")

# Print final summary
cat("========================================\n")
cat("MODULE 01 COMPLETE\n")
cat("========================================\n\n")

cat("Processing Summary:\n")
cat("-------------------\n")
cat("✓ High-res cores processed:", nrow(hr_cores_clean), "samples\n")
cat("✓ Composite cores processed:", nrow(composite_cores_clean), "samples\n")
cat("✓ Combined dataset:", nrow(combined_cores), "samples\n")
cat("✓ Unique core locations:", length(unique(combined_cores$core_id)), "\n")
cat("✓ Files saved to 'data_processed/' and 'outputs/'\n")
cat("✓ Log saved to:", log_file, "\n\n")

cat("Next steps:\n")
cat("1. Review data quality report: outputs/data_quality_report.rds\n")
cat("2. Check summary statistics in outputs/\n")
cat("3. Run Module 02: source('02_exploratory_analysis.R')\n\n")

# Save session info for reproducibility
sessionInfo <- sessionInfo()
saveRDS(sessionInfo, file.path("outputs", "session_info_module01.rds"))

log_message("Module 01 completed successfully")
