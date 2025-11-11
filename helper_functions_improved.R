# ============================================================================
# Helper Functions for Composite Sampling Analysis - IMPROVED VERSION
# VERSION: 2.0 - Production-ready with error handling and validation
# ============================================================================

# Load required packages with error handling
load_required_package <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required but not installed. Install with: install.packages('%s')", 
                 pkg, pkg))
  }
}

#' Calculate SOC stock for a depth increment
#'
#' @param soc Soil organic carbon concentration (g/kg or %)
#' @param bd Bulk density (g/cm3), must be between 0.1 and 3.0
#' @param depth_top Top depth of increment (cm), must be >= 0
#' @param depth_bottom Bottom depth of increment (cm), must be > depth_top
#' @param soc_units Units of SOC: "g_kg" (default) or "percent"
#' 
#' @return SOC stock in Mg/ha
#' 
#' @examples
#' calculate_soc_stock(30, 1.2, 0, 10, "g_kg")
#' calculate_soc_stock(3, 1.2, 0, 10, "percent")
#' 
#' @export
calculate_soc_stock <- function(soc, bd, depth_top, depth_bottom, 
                               soc_units = "g_kg") {
  # Input validation
  if (!is.numeric(soc) || any(soc < 0, na.rm = TRUE)) {
    stop("SOC must be non-negative numeric value(s)")
  }
  if (!is.numeric(bd) || any(bd <= 0 | bd > 3, na.rm = TRUE)) {
    stop("Bulk density must be numeric between 0 and 3 g/cm³")
  }
  if (!is.numeric(depth_top) || any(depth_top < 0, na.rm = TRUE)) {
    stop("depth_top must be non-negative numeric value(s)")
  }
  if (!is.numeric(depth_bottom) || any(depth_bottom <= depth_top, na.rm = TRUE)) {
    stop("depth_bottom must be numeric and greater than depth_top")
  }
  if (!soc_units %in% c("g_kg", "percent")) {
    stop("soc_units must be 'g_kg' or 'percent'")
  }
  
  # Convert SOC to proportion
  soc_prop <- if (soc_units == "g_kg") {
    soc / 1000
  } else {
    soc / 100
  }
  
  # Calculate depth increment
  depth_increment <- depth_bottom - depth_top
  
  # SOC stock (Mg/ha) = SOC (g/g) × BD (g/cm3) × depth (cm) × 100
  soc_stock <- soc_prop * bd * depth_increment * 100
  
  return(soc_stock)
}

#' Calculate depth midpoint with validation
#'
#' @param depth_top Top depth (cm)
#' @param depth_bottom Bottom depth (cm)
#' @return Midpoint depth (cm)
#' @export
calculate_depth_midpoint <- function(depth_top, depth_bottom) {
  if (!is.numeric(depth_top) || !is.numeric(depth_bottom)) {
    stop("Depths must be numeric")
  }
  if (any(depth_bottom <= depth_top, na.rm = TRUE)) {
    stop("depth_bottom must be greater than depth_top")
  }
  return((depth_top + depth_bottom) / 2)
}

#' Aggregate high-resolution data to match composite depth increments
#'
#' @param hr_data High-resolution core data frame
#' @param composite_depths Vector of depth breaks for aggregation
#' @return Aggregated data frame matching composite resolution
#' @export
aggregate_hr_to_composite <- function(hr_data, composite_depths = seq(0, 100, 20)) {
  
  # Load dplyr with error handling
  load_required_package("dplyr")
  
  # Validate input
  if (!is.data.frame(hr_data)) {
    stop("hr_data must be a data frame")
  }
  
  required_cols <- c("core_id", "latitude", "longitude", "depth_top", 
                    "depth_bottom", "soc", "bulk_density", "depth_mid")
  missing_cols <- setdiff(required_cols, names(hr_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  tryCatch({
    # Add depth bin assignment
    hr_data$depth_bin <- cut(hr_data$depth_mid, 
                             breaks = composite_depths,
                             include.lowest = TRUE,
                             right = FALSE)
    
    # Calculate depth increment as weight BEFORE aggregating
    hr_data <- hr_data %>%
      mutate(depth_increment = depth_bottom - depth_top)
    
    # Aggregate by core and depth bin with error handling
    aggregated <- hr_data %>%
      group_by(core_id, latitude, longitude, depth_bin) %>%
      summarise(
        depth_top = min(depth_top),
        depth_bottom = max(depth_bottom),
        depth_mid = mean(depth_mid),
        soc_mean = weighted.mean(soc, depth_increment, na.rm = TRUE),
        bd_mean = weighted.mean(bulk_density, depth_increment, na.rm = TRUE),
        soc_stock = sum(soc_stock, na.rm = TRUE),
        n_increments = n(),
        .groups = "drop"
      )
    
    # Check for any NA values in critical columns
    if (any(is.na(aggregated$soc_mean)) || any(is.na(aggregated$bd_mean))) {
      warning("NA values generated during aggregation. Check input data.")
    }
    
    return(aggregated)
    
  }, error = function(e) {
    stop(paste("Error during aggregation:", e$message))
  })
}

#' Enhanced data quality check with detailed reporting
#'
#' @param data Data frame to check
#' @param core_type Character string: "high_res" or "composite"
#' @param verbose Logical: print results to console
#' @return List with detailed data quality metrics
#' @export
check_data_quality <- function(data, core_type = "unknown", verbose = TRUE) {
  
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  quality_report <- list(
    core_type = core_type,
    n_cores = length(unique(data$core_id)),
    n_samples = nrow(data),
    depth_range = if ("depth_top" %in% names(data) && "depth_bottom" %in% names(data)) {
      range(c(data$depth_top, data$depth_bottom), na.rm = TRUE)
    } else {
      c(NA, NA)
    },
    missing_values = colSums(is.na(data)),
    negative_values = list(
      soc = if ("soc" %in% names(data)) sum(data$soc < 0, na.rm = TRUE) else NA,
      bulk_density = if ("bulk_density" %in% names(data)) {
        sum(data$bulk_density < 0, na.rm = TRUE)
      } else NA
    ),
    unrealistic_values = list(
      soc_high = if ("soc" %in% names(data)) {
        sum(data$soc > 200, na.rm = TRUE)  # SOC > 200 g/kg unusual
      } else NA,
      bd_low = if ("bulk_density" %in% names(data)) {
        sum(data$bulk_density < 0.5, na.rm = TRUE)
      } else NA,
      bd_high = if ("bulk_density" %in% names(data)) {
        sum(data$bulk_density > 2.0, na.rm = TRUE)
      } else NA
    ),
    duplicate_samples = if (all(c("core_id", "depth_top") %in% names(data))) {
      sum(duplicated(data[, c("core_id", "depth_top")]))
    } else NA
  )
  
  if (verbose) {
    print_quality_report(quality_report)
  }
  
  return(quality_report)
}

#' Create depth profile plot with error handling
#'
#' @param data Data frame with depth and SOC data
#' @param core_id_val Core ID to plot
#' @param variable Variable to plot: "soc" or "bulk_density"
#' @param save_plot Logical: save plot to file
#' @param output_dir Directory for saving plots
#' @return ggplot object or NULL if error
#' @export
plot_depth_profile <- function(data, core_id_val, variable = "soc",
                              save_plot = FALSE, output_dir = "outputs/plots") {
  
  # Load ggplot2 with error handling
  load_required_package("ggplot2")
  
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  if (!core_id_val %in% data$core_id) {
    stop(paste("Core ID", core_id_val, "not found in data"))
  }
  
  if (!variable %in% c("soc", "bulk_density")) {
    stop("variable must be 'soc' or 'bulk_density'")
  }
  
  tryCatch({
    core_data <- data[data$core_id == core_id_val, ]
    
    if (variable == "soc") {
      if (!"soc" %in% names(core_data)) {
        stop("Column 'soc' not found in data")
      }
      x_var <- core_data$soc
      x_lab <- "SOC (g/kg)"
    } else {
      if (!"bulk_density" %in% names(core_data)) {
        stop("Column 'bulk_density' not found in data")
      }
      x_var <- core_data$bulk_density
      x_lab <- "Bulk Density (g/cm³)"
    }
    
    p <- ggplot(core_data, aes(x = x_var, y = -depth_mid)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        title = paste("Core", core_id_val),
        x = x_lab,
        y = "Depth (cm)"
      ) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11, face = "bold")
      )
    
    if (save_plot) {
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      filename <- file.path(output_dir, 
                           paste0("profile_", core_id_val, "_", variable, ".png"))
      ggsave(filename, p, width = 6, height = 8, dpi = 150)
      message(paste("Plot saved to", filename))
    }
    
    return(p)
    
  }, error = function(e) {
    warning(paste("Error creating plot:", e$message))
    return(NULL)
  })
}

#' Calculate cumulative SOC stock by depth with validation
#'
#' @param data Data frame with depth and soc_stock columns
#' @return Data frame with cumulative SOC stock
#' @export
calculate_cumulative_stock <- function(data) {
  
  load_required_package("dplyr")
  
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  required_cols <- c("core_id", "depth_top", "soc_stock")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  tryCatch({
    result <- data %>%
      arrange(core_id, depth_top) %>%
      group_by(core_id) %>%
      mutate(
        cumulative_stock = cumsum(soc_stock),
        cumulative_depth = depth_bottom
      ) %>%
      ungroup()
    
    return(result)
    
  }, error = function(e) {
    stop(paste("Error calculating cumulative stock:", e$message))
  })
}

#' Export spatial data as shapefile with validation
#'
#' @param data Data frame with latitude, longitude columns
#' @param filename Output filename (without extension)
#' @param crs Coordinate reference system (default: WGS84)
#' @param format Output format: "shp", "gpkg", or "both"
#' @return TRUE if successful, FALSE otherwise
#' @export
export_to_shapefile <- function(data, filename, crs = 4326, format = "shp") {
  
  load_required_package("sf")
  
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (!all(c("latitude", "longitude") %in% names(data))) {
    stop("Data must contain 'latitude' and 'longitude' columns")
  }
  
  # Check for valid coordinates
  if (any(data$latitude < -90 | data$latitude > 90, na.rm = TRUE)) {
    stop("Invalid latitude values detected")
  }
  
  if (any(data$longitude < -180 | data$longitude > 180, na.rm = TRUE)) {
    stop("Invalid longitude values detected")
  }
  
  tryCatch({
    # Create sf object
    sf_data <- st_as_sf(data, 
                        coords = c("longitude", "latitude"),
                        crs = crs)
    
    # Export based on format
    success <- FALSE
    
    if (format %in% c("shp", "both")) {
      st_write(sf_data, paste0(filename, ".shp"), delete_dsn = TRUE, quiet = TRUE)
      message(paste("Shapefile exported:", filename, ".shp"))
      success <- TRUE
    }
    
    if (format %in% c("gpkg", "both")) {
      st_write(sf_data, paste0(filename, ".gpkg"), delete_dsn = TRUE, quiet = TRUE)
      message(paste("GeoPackage exported:", filename, ".gpkg"))
      success <- TRUE
    }
    
    return(success)
    
  }, error = function(e) {
    warning(paste("Error exporting spatial data:", e$message))
    return(FALSE)
  })
}

#' Enhanced print function for quality reports
#'
#' @param quality_report Output from check_data_quality()
#' @param output_file Optional file to write report to
#' @export
print_quality_report <- function(quality_report, output_file = NULL) {
  
  report_text <- c(
    "\n===========================================",
    paste("DATA QUALITY REPORT:", quality_report$core_type),
    "===========================================\n",
    paste("Number of cores:", quality_report$n_cores),
    paste("Number of samples:", quality_report$n_samples),
    paste("Depth range:", quality_report$depth_range[1], "to", 
          quality_report$depth_range[2], "cm\n"),
    "\nMissing values:",
    capture.output(print(quality_report$missing_values)),
    "\nNegative values:",
    capture.output(print(quality_report$negative_values)),
    "\nPotentially unrealistic values:",
    capture.output(print(quality_report$unrealistic_values)),
    paste("\nDuplicate samples:", quality_report$duplicate_samples),
    "\n==========================================="
  )
  
  # Print to console
  cat(paste(report_text, collapse = "\n"), "\n")
  
  # Optionally write to file
  if (!is.null(output_file)) {
    writeLines(report_text, output_file)
    message(paste("Report saved to", output_file))
  }
}

#' Validate core data structure and content
#'
#' @param data Data frame to validate
#' @param stop_on_error Logical: stop execution on validation failure
#' @return TRUE if valid, FALSE or error otherwise
#' @export
validate_core_data <- function(data, stop_on_error = TRUE) {
  
  errors <- character()
  warnings <- character()
  
  # Check if it's a data frame
  if (!is.data.frame(data)) {
    errors <- c(errors, "Input must be a data frame")
  }
  
  # Check required columns
  required_cols <- c("core_id", "latitude", "longitude", 
                    "depth_top", "depth_bottom", "soc", "bulk_density")
  missing_cols <- setdiff(tolower(required_cols), tolower(names(data)))
  if (length(missing_cols) > 0) {
    errors <- c(errors, paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # If basic structure is wrong, return early
  if (length(errors) > 0) {
    if (stop_on_error) {
      stop(paste(errors, collapse = "\n"))
    } else {
      warning(paste(errors, collapse = "\n"))
      return(FALSE)
    }
  }
  
  # Check data types
  if (!is.numeric(data$latitude) || !is.numeric(data$longitude)) {
    errors <- c(errors, "Coordinates must be numeric")
  }
  
  # Check coordinate ranges
  if (any(data$latitude < -90 | data$latitude > 90, na.rm = TRUE)) {
    errors <- c(errors, "Invalid latitude values (outside -90 to 90)")
  }
  
  if (any(data$longitude < -180 | data$longitude > 180, na.rm = TRUE)) {
    errors <- c(errors, "Invalid longitude values (outside -180 to 180)")
  }
  
  # Check depth logic
  if (any(data$depth_bottom <= data$depth_top, na.rm = TRUE)) {
    errors <- c(errors, "depth_bottom must be greater than depth_top")
  }
  
  # Check for negative depths
  if (any(data$depth_top < 0, na.rm = TRUE)) {
    warnings <- c(warnings, "Negative depth_top values found")
  }
  
  # Check SOC and BD ranges
  if (any(data$soc < 0, na.rm = TRUE)) {
    errors <- c(errors, "Negative SOC values found")
  }
  
  if (any(data$bulk_density <= 0, na.rm = TRUE)) {
    errors <- c(errors, "Bulk density must be positive")
  }
  
  if (any(data$bulk_density > 3, na.rm = TRUE)) {
    warnings <- c(warnings, "Unusually high bulk density values (>3 g/cm³)")
  }
  
  # Report results
  if (length(warnings) > 0) {
    warning(paste("Validation warnings:\n", paste(warnings, collapse = "\n")))
  }
  
  if (length(errors) > 0) {
    if (stop_on_error) {
      stop(paste("Validation errors:\n", paste(errors, collapse = "\n")))
    } else {
      warning(paste("Validation errors:\n", paste(errors, collapse = "\n")))
      return(FALSE)
    }
  }
  
  message("Data validation passed ✓")
  return(TRUE)
}

#' Create a session log for reproducibility
#'
#' @param log_file Path to log file
#' @export
create_session_log <- function(log_file = "session_log.txt") {
  log_content <- c(
    paste("Analysis Date:", Sys.Date()),
    paste("R Version:", R.version.string),
    paste("Platform:", R.version$platform),
    paste("Working Directory:", getwd()),
    "\nLoaded Packages:",
    capture.output(sessionInfo()),
    "\n========================================\n"
  )
  
  writeLines(log_content, log_file)
  message(paste("Session log saved to", log_file))
}

# Print message when loaded
message("Helper functions v2.0 loaded successfully")
message("Use validate_core_data() to check your data before processing")
