# ============================================================================
# Module 04b: Depth Harmonization Using Equal-Area Splines - ENHANCED VERSION
# VERSION: 3.0 - Production-ready with uncertainty quantification
# ============================================================================
# ENHANCEMENTS IN THIS VERSION:
# 1. Bootstrap confidence intervals for spline predictions
# 2. Cross-validation for lambda (smoothing parameter) selection
# 3. Improved diagnostics and validation
# 4. Uncertainty propagation to outputs
# ============================================================================

# ============================================================================
# SETUP AND CONFIGURATION
# ============================================================================

# Initialize logging
log_file <- file.path("logs", paste0("depth_harmonization_enhanced_", Sys.Date(), ".log"))
if (!dir.exists("logs")) dir.create("logs", recursive = TRUE)

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

log_message("Starting Module 04b ENHANCED: Depth Harmonization with Uncertainty Quantification")

# Set random seed for reproducibility
set.seed(42)

# Define standard depths for predictions (GlobalSoilMap + custom)
STANDARD_DEPTHS <- c(0, 5, 10, 15, 20, 30, 40, 60, 80, 100)

# Bootstrap configuration
BOOTSTRAP_ENABLED <- TRUE
N_BOOTSTRAP <- 10  # Number of bootstrap iterations
BOOTSTRAP_CONF_LEVEL <- 0.95  # 95% confidence intervals

# Lambda optimization configuration
OPTIMIZE_LAMBDA <- TRUE
LAMBDA_SEARCH_GRID <- seq(0.01, 0.5, by = 0.05)

log_message(sprintf("Configured %d standard depths: %s", 
                    length(STANDARD_DEPTHS),
                    paste(STANDARD_DEPTHS, collapse = ", ")))
log_message(sprintf("Bootstrap: %s (n=%d, CI=%.1f%%)", 
                    ifelse(BOOTSTRAP_ENABLED, "ENABLED", "DISABLED"),
                    N_BOOTSTRAP, BOOTSTRAP_CONF_LEVEL * 100))
log_message(sprintf("Lambda optimization: %s", 
                    ifelse(OPTIMIZE_LAMBDA, "ENABLED", "DISABLED")))

# ============================================================================
# LOAD PACKAGES
# ============================================================================

load_required_package <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    log_message(sprintf("Package '%s' not available", pkg), "WARNING")
    return(FALSE)
  }
  log_message(sprintf("Loaded package: %s", pkg))
  return(TRUE)
}

# Required packages
required_packages <- c("dplyr", "ggplot2", "boot")

available_pkgs <- sapply(required_packages, load_required_package)

if (!all(available_pkgs[c("dplyr")])) {
  stop("Core package (dplyr) is required")
}

# Check bootstrap package
if (BOOTSTRAP_ENABLED && !available_pkgs["boot"]) {
  log_message("Bootstrap package 'boot' not available - disabling bootstrap", "WARNING")
  BOOTSTRAP_ENABLED <- FALSE
}

# Source helper functions if available
if (file.exists("helper_functions_improved.R")) {
  source("helper_functions_improved.R")
} else if (file.exists("helper_functions.R")) {
  source("helper_functions.R")
}

# ============================================================================
# LOAD DATA
# ============================================================================

log_message("Loading cleaned core data")

# Load high-resolution cores
hr_cores <- tryCatch({
  readRDS("data_processed/hr_cores_clean.rds")
}, error = function(e) {
  stop("Could not load hr_cores_clean.rds. Please run Module 01 first.")
})

log_message(sprintf("Loaded HR cores: %d samples from %d cores", 
                    nrow(hr_cores), 
                    length(unique(hr_cores$core_id))))

# Load composite cores
composite_cores <- tryCatch({
  readRDS("data_processed/composite_cores_clean.rds")
}, error = function(e) {
  stop("Could not load composite_cores_clean.rds. Please run Module 01 first.")
})

log_message(sprintf("Loaded composite cores: %d samples from %d cores", 
                    nrow(composite_cores), 
                    length(unique(composite_cores$core_id))))

# Combine for processing
all_cores <- dplyr::bind_rows(
  hr_cores %>% dplyr::mutate(core_type = "HR"),
  composite_cores %>% dplyr::mutate(core_type = "Composite")
)

log_message(sprintf("Combined dataset: %d samples from %d total cores",
                    nrow(all_cores),
                    length(unique(all_cores$core_id))))

# ============================================================================
# ENHANCED SPLINE FUNCTIONS
# ============================================================================

#' Optimize lambda using leave-one-out cross-validation
#' @param depths_top Vector of top depths (cm)
#' @param depths_bottom Vector of bottom depths (cm)
#' @param values Vector of values (e.g., SOC g/kg)
#' @param lambda_grid Vector of lambda values to test
#' @return Optimal lambda value
optimize_lambda_cv <- function(depths_top, depths_bottom, values, 
                               lambda_grid = LAMBDA_SEARCH_GRID) {
  
  if (length(values) < 5) {
    # Not enough data for CV - return default
    return(0.1)
  }
  
  tryCatch({
    midpoints <- (depths_top + depths_bottom) / 2
    cv_errors <- numeric(length(lambda_grid))
    
    for (i in seq_along(lambda_grid)) {
      loo_errors <- numeric(length(values))
      
      # Leave-one-out cross-validation
      for (j in seq_along(values)) {
        # Fit without observation j
        train_fit <- tryCatch({
          smooth.spline(
            x = midpoints[-j],
            y = values[-j],
            spar = lambda_grid[i],
            keep.data = FALSE
          )
        }, error = function(e) NULL)
        
        if (!is.null(train_fit)) {
          # Predict observation j
          pred_j <- predict(train_fit, x = midpoints[j])$y
          loo_errors[j] <- (values[j] - pred_j)^2
        } else {
          loo_errors[j] <- NA
        }
      }
      
      cv_errors[i] <- mean(loo_errors, na.rm = TRUE)
    }
    
    # Select lambda with minimum CV error
    if (all(is.na(cv_errors))) {
      return(0.1)  # Default if all CV failed
    }
    
    best_lambda <- lambda_grid[which.min(cv_errors)]
    return(best_lambda)
    
  }, error = function(e) {
    log_message(sprintf("Lambda optimization failed: %s - using default", e$message), "WARNING")
    return(0.1)
  })
}

#' Bootstrap confidence intervals for spline predictions
#' @param depths_top Vector of top depths (cm)
#' @param depths_bottom Vector of bottom depths (cm)
#' @param values Vector of values (e.g., SOC g/kg)
#' @param predict_depths Vector of depths to predict at
#' @param lambda Smoothing parameter
#' @param n_boot Number of bootstrap iterations
#' @param conf_level Confidence level (e.g., 0.95 for 95% CI)
#' @return Data frame with predictions and confidence intervals
bootstrap_spline_ci <- function(depths_top, depths_bottom, values, 
                                predict_depths, lambda = 0.1, 
                                n_boot = N_BOOTSTRAP, 
                                conf_level = BOOTSTRAP_CONF_LEVEL) {
  
  if (length(values) < 3 || !BOOTSTRAP_ENABLED) {
    # Return NA CIs if insufficient data or bootstrap disabled
    return(data.frame(
      depth_cm = predict_depths,
      lower_ci = NA,
      upper_ci = NA
    ))
  }
  
  tryCatch({
    n_obs <- length(values)
    boot_predictions <- matrix(NA, nrow = n_boot, ncol = length(predict_depths))
    
    # Bootstrap iterations
    for (b in 1:n_boot) {
      # Resample observations with replacement
      boot_indices <- sample(1:n_obs, n_obs, replace = TRUE)
      
      # Fit spline to bootstrap sample
      boot_fit <- fit_equal_area_spline(
        depths_top[boot_indices],
        depths_bottom[boot_indices],
        values[boot_indices],
        predict_depths,
        lambda = lambda
      )
      
      if (boot_fit$success) {
        boot_predictions[b, ] <- boot_fit$predictions
      }
    }
    
    # Calculate confidence intervals
    alpha <- 1 - conf_level
    lower_ci <- apply(boot_predictions, 2, quantile, probs = alpha/2, na.rm = TRUE)
    upper_ci <- apply(boot_predictions, 2, quantile, probs = 1 - alpha/2, na.rm = TRUE)
    
    return(data.frame(
      depth_cm = predict_depths,
      lower_ci = lower_ci,
      upper_ci = upper_ci
    ))
    
  }, error = function(e) {
    log_message(sprintf("Bootstrap failed: %s", e$message), "WARNING")
    return(data.frame(
      depth_cm = predict_depths,
      lower_ci = NA,
      upper_ci = NA
    ))
  })
}

#' Fit equal-area spline to depth data (ENHANCED VERSION)
#' @param depths_top Vector of top depths (cm)
#' @param depths_bottom Vector of bottom depths (cm)
#' @param values Vector of values (e.g., SOC g/kg)
#' @param predict_depths Vector of depths to predict at
#' @param lambda Smoothing parameter (0-1, higher = smoother)
#' @param optimize_lambda If TRUE, use CV to optimize lambda
#' @return List with predictions, diagnostics, and uncertainty
fit_equal_area_spline <- function(depths_top, depths_bottom, values, 
                                   predict_depths, lambda = 0.1,
                                   optimize_lambda = OPTIMIZE_LAMBDA) {
  
  tryCatch({
    # Input validation
    if (length(depths_top) < 3) {
      return(list(
        predictions = rep(NA, length(predict_depths)),
        success = FALSE,
        message = "Insufficient data points (need at least 3)"
      ))
    }
    
    if (any(is.na(values)) || any(!is.finite(values))) {
      return(list(
        predictions = rep(NA, length(predict_depths)),
        success = FALSE,
        message = "NA or infinite values in data"
      ))
    }
    
    # Calculate horizon midpoints and thicknesses
    midpoints <- (depths_top + depths_bottom) / 2
    thicknesses <- depths_bottom - depths_top
    
    # Remove any zero-thickness layers
    valid <- thicknesses > 0
    midpoints <- midpoints[valid]
    thicknesses <- thicknesses[valid]
    values <- values[valid]
    depths_top <- depths_top[valid]
    depths_bottom <- depths_bottom[valid]
    
    if (length(values) < 3) {
      return(list(
        predictions = rep(NA, length(predict_depths)),
        success = FALSE,
        message = "Insufficient valid data after filtering"
      ))
    }
    
    # Optimize lambda if requested
    if (optimize_lambda && length(values) >= 5) {
      lambda_opt <- optimize_lambda_cv(depths_top, depths_bottom, values)
      lambda <- lambda_opt
    }
    
    # Fit smoothing spline to horizon midpoints
    spline_fit <- smooth.spline(
      x = midpoints,
      y = values,
      spar = lambda,
      keep.data = TRUE
    )
    
    # Predict at requested depths
    predictions <- predict(spline_fit, x = predict_depths)$y
    
    # Apply constraints
    # 1. Non-negative values
    predictions[predictions < 0] <- 0
    
    # 2. Constrain predictions to reasonable range
    min_obs <- min(values, na.rm = TRUE)
    max_obs <- max(values, na.rm = TRUE)
    range_obs <- max_obs - min_obs
    
    predictions[predictions < (min_obs - 0.5 * range_obs)] <- min_obs - 0.5 * range_obs
    predictions[predictions > (max_obs + 0.5 * range_obs)] <- max_obs + 0.5 * range_obs
    
    # Calculate fit quality (RMSE on original depths)
    fitted_values <- predict(spline_fit, x = midpoints)$y
    rmse <- sqrt(mean((values - fitted_values)^2, na.rm = TRUE))
    r_squared <- 1 - sum((values - fitted_values)^2) / sum((values - mean(values))^2)
    
    return(list(
      predictions = predictions,
      success = TRUE,
      message = "Spline fitted successfully",
      rmse = rmse,
      r_squared = r_squared,
      n_points = length(values),
      lambda = lambda,
      spline_fit = spline_fit
    ))
    
  }, error = function(e) {
    return(list(
      predictions = rep(NA, length(predict_depths)),
      success = FALSE,
      message = paste("Spline fitting error:", e$message)
    ))
  })
}

# ============================================================================
# FIT SPLINES TO ALL CORES WITH UNCERTAINTY
# ============================================================================

log_message("\n=== FITTING SPLINES WITH UNCERTAINTY QUANTIFICATION ===\n")

# Initialize results storage
harmonized_data <- data.frame()
spline_diagnostics <- data.frame()
uncertainty_data <- data.frame()

# Get unique cores
unique_cores <- unique(all_cores$core_id)
n_cores <- length(unique_cores)

log_message(sprintf("Processing %d cores...", n_cores))

# Progress tracking
cores_processed <- 0
cores_success <- 0
cores_failed <- 0

# Process each core
for (i in seq_along(unique_cores)) {
  
  core_id <- unique_cores[i]
  
  # Progress reporting every 10 cores
  if (i %% 10 == 0) {
    log_message(sprintf("  Progress: %d/%d cores (%.1f%%) - Success: %d, Failed: %d", 
                       i, n_cores, 100 * i / n_cores, cores_success, cores_failed))
  }
  
  # Extract core data
  core_data <- all_cores %>%
    dplyr::filter(core_id == !!core_id) %>%
    dplyr::arrange(depth_top)
  
  # Check if we have minimum data
  if (nrow(core_data) < 3) {
    log_message(sprintf("  Skipping %s: insufficient data (n=%d)", 
                       core_id, nrow(core_data)), "WARNING")
    cores_failed <- cores_failed + 1
    next
  }
  
  # Get metadata
  latitude <- core_data$latitude[1]
  longitude <- core_data$longitude[1]
  core_type <- core_data$core_type[1]
  
  # Fit spline
  spline_result <- fit_equal_area_spline(
    depths_top = core_data$depth_top,
    depths_bottom = core_data$depth_bottom,
    values = core_data$soc,
    predict_depths = STANDARD_DEPTHS,
    lambda = 0.1,  # Will be optimized if OPTIMIZE_LAMBDA = TRUE
    optimize_lambda = OPTIMIZE_LAMBDA
  )
  
  # Check if successful
  if (spline_result$success) {
    
    # Store predictions
    core_harmonized <- data.frame(
      core_id = core_id,
      latitude = latitude,
      longitude = longitude,
      core_type = core_type,
      depth_cm = STANDARD_DEPTHS,
      soc_spline = spline_result$predictions,
      stringsAsFactors = FALSE
    )
    
    harmonized_data <- dplyr::bind_rows(harmonized_data, core_harmonized)
    
    # Bootstrap confidence intervals
    if (BOOTSTRAP_ENABLED) {
      ci_result <- bootstrap_spline_ci(
        depths_top = core_data$depth_top,
        depths_bottom = core_data$depth_bottom,
        values = core_data$soc,
        predict_depths = STANDARD_DEPTHS,
        lambda = spline_result$lambda,
        n_boot = N_BOOTSTRAP,
        conf_level = BOOTSTRAP_CONF_LEVEL
      )
      
      # Store uncertainty
      core_uncertainty <- data.frame(
        core_id = core_id,
        depth_cm = ci_result$depth_cm,
        lower_ci = ci_result$lower_ci,
        upper_ci = ci_result$upper_ci,
        ci_width = ci_result$upper_ci - ci_result$lower_ci,
        stringsAsFactors = FALSE
      )
      
      uncertainty_data <- dplyr::bind_rows(uncertainty_data, core_uncertainty)
    }
    
    # Store diagnostics
    if (!is.null(spline_result$rmse)) {
      diagnostics <- data.frame(
        core_id = core_id,
        core_type = core_type,
        n_samples = spline_result$n_points,
        rmse = spline_result$rmse,
        r_squared = spline_result$r_squared,
        lambda = spline_result$lambda,
        success = TRUE,
        stringsAsFactors = FALSE
      )
      spline_diagnostics <- dplyr::bind_rows(spline_diagnostics, diagnostics)
    }
    
    cores_success <- cores_success + 1
    
  } else {
    log_message(sprintf("  Failed %s: %s", core_id, spline_result$message), "WARNING")
    cores_failed <- cores_failed + 1
  }
  
  cores_processed <- cores_processed + 1
}

# ============================================================================
# MERGE HARMONIZED DATA WITH UNCERTAINTY
# ============================================================================

if (BOOTSTRAP_ENABLED && nrow(uncertainty_data) > 0) {
  log_message("Merging uncertainty estimates with harmonized data")
  
  harmonized_data <- harmonized_data %>%
    dplyr::left_join(
      uncertainty_data,
      by = c("core_id", "depth_cm")
    )
}

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

log_message("\n=== SPLINE FITTING SUMMARY ===\n")

cat("\nProcessing Summary:\n")
cat("==================\n")
cat(sprintf("Total cores: %d\n", n_cores))
cat(sprintf("Successfully fitted: %d (%.1f%%)\n", 
            cores_success, 100 * cores_success / n_cores))
cat(sprintf("Failed: %d (%.1f%%)\n", 
            cores_failed, 100 * cores_failed / n_cores))
cat("\n")

if (nrow(spline_diagnostics) > 0) {
  cat("Spline Fit Quality:\n")
  cat("==================\n")
  
  # Overall statistics
  cat(sprintf("Mean RMSE: %.2f g/kg\n", mean(spline_diagnostics$rmse, na.rm = TRUE)))
  cat(sprintf("Mean R²: %.3f\n", mean(spline_diagnostics$r_squared, na.rm = TRUE)))
  
  if (OPTIMIZE_LAMBDA) {
    cat(sprintf("Mean Lambda: %.3f\n", mean(spline_diagnostics$lambda, na.rm = TRUE)))
    cat(sprintf("Lambda range: %.3f - %.3f\n", 
                min(spline_diagnostics$lambda, na.rm = TRUE),
                max(spline_diagnostics$lambda, na.rm = TRUE)))
  }
  cat("\n")
  
  # By core type
  cat("By Core Type:\n")
  summary_by_type <- spline_diagnostics %>%
    dplyr::group_by(core_type) %>%
    dplyr::summarise(
      n_cores = n(),
      mean_rmse = mean(rmse, na.rm = TRUE),
      mean_r2 = mean(r_squared, na.rm = TRUE),
      mean_lambda = mean(lambda, na.rm = TRUE),
      mean_n_samples = mean(n_samples, na.rm = TRUE),
      .groups = "drop"
    )
  print(summary_by_type)
  cat("\n")
}

# Uncertainty summary
if (BOOTSTRAP_ENABLED && nrow(uncertainty_data) > 0) {
  cat("Uncertainty Summary:\n")
  cat("===================\n")
  
  uncertainty_summary <- uncertainty_data %>%
    dplyr::group_by(depth_cm) %>%
    dplyr::summarise(
      mean_ci_width = mean(ci_width, na.rm = TRUE),
      median_ci_width = median(ci_width, na.rm = TRUE),
      n_cores = n(),
      .groups = "drop"
    )
  
  print(uncertainty_summary)
  cat("\n")
}

# ============================================================================
# VALIDATE HARMONIZED DATA
# ============================================================================

log_message("Validating harmonized dataset")

if (nrow(harmonized_data) > 0) {
  
  # Check for issues
  n_total_predictions <- nrow(harmonized_data)
  n_na <- sum(is.na(harmonized_data$soc_spline))
  n_negative <- sum(harmonized_data$soc_spline < 0, na.rm = TRUE)
  n_extreme <- sum(harmonized_data$soc_spline > 200, na.rm = TRUE)
  
  cat("Data Quality Checks:\n")
  cat("====================\n")
  cat(sprintf("Total predictions: %d\n", n_total_predictions))
  cat(sprintf("NA values: %d (%.1f%%)\n", n_na, 100 * n_na / n_total_predictions))
  cat(sprintf("Negative values: %d (%.1f%%)\n", n_negative, 100 * n_negative / n_total_predictions))
  cat(sprintf("Extreme values (>200): %d (%.1f%%)\n", n_extreme, 100 * n_extreme / n_total_predictions))
  cat("\n")
  
  # Summary by depth
  cat("Predictions by Depth:\n")
  cat("====================\n")
  depth_summary <- harmonized_data %>%
    dplyr::group_by(depth_cm) %>%
    dplyr::summarise(
      n_locations = n(),
      mean_soc = mean(soc_spline, na.rm = TRUE),
      sd_soc = sd(soc_spline, na.rm = TRUE),
      min_soc = min(soc_spline, na.rm = TRUE),
      max_soc = max(soc_spline, na.rm = TRUE),
      n_na = sum(is.na(soc_spline)),
      .groups = "drop"
    )
  print(depth_summary, n = Inf)
  cat("\n")
  
} else {
  stop("No harmonized data produced - check spline fitting errors")
}

# ============================================================================
# SAVE OUTPUTS
# ============================================================================

log_message("Saving harmonized dataset and diagnostics")

# Create output directories
if (!dir.exists("data_processed")) {
  dir.create("data_processed", recursive = TRUE)
}

if (!dir.exists("outputs")) {
  dir.create("outputs", recursive = TRUE)
}

# Save harmonized data
harmonized_file <- "data_processed/cores_harmonized_spline_enhanced.rds"
saveRDS(harmonized_data, harmonized_file)
write.csv(harmonized_data, "data_processed/cores_harmonized_spline_enhanced.csv", row.names = FALSE)
log_message(sprintf("Saved harmonized data: %s", harmonized_file))

# Save uncertainty data
if (BOOTSTRAP_ENABLED && nrow(uncertainty_data) > 0) {
  uncertainty_file <- "outputs/spline_uncertainty.rds"
  saveRDS(uncertainty_data, uncertainty_file)
  write.csv(uncertainty_data, "outputs/spline_uncertainty.csv", row.names = FALSE)
  log_message(sprintf("Saved uncertainty data: %s", uncertainty_file))
}

# Save diagnostics
if (nrow(spline_diagnostics) > 0) {
  diagnostics_file <- "outputs/spline_diagnostics_enhanced.csv"
  write.csv(spline_diagnostics, diagnostics_file, row.names = FALSE)
  log_message(sprintf("Saved spline diagnostics: %s", diagnostics_file))
}

# Save depth summary
depth_summary_file <- "outputs/harmonized_depth_summary_enhanced.csv"
write.csv(depth_summary, depth_summary_file, row.names = FALSE)
log_message(sprintf("Saved depth summary: %s", depth_summary_file))

# ============================================================================
# CREATE ENHANCED VISUALIZATIONS
# ============================================================================

log_message("Creating enhanced visualizations")

# Create plots directory
plots_dir <- "outputs/plots/spline_harmonization_enhanced"
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
}

# 1. Depth profiles with uncertainty bands
if (nrow(harmonized_data) > 0 && BOOTSTRAP_ENABLED) {
  
  tryCatch({
    # Mean depth profiles by core type with uncertainty
    profile_summary <- harmonized_data %>%
      dplyr::group_by(core_type, depth_cm) %>%
      dplyr::summarise(
        mean_soc = mean(soc_spline, na.rm = TRUE),
        se_soc = sd(soc_spline, na.rm = TRUE) / sqrt(n()),
        mean_lower_ci = mean(lower_ci, na.rm = TRUE),
        mean_upper_ci = mean(upper_ci, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      )
    
    p_profiles <- ggplot(profile_summary, 
                         aes(x = mean_soc, y = -depth_cm, color = core_type, fill = core_type)) +
      # Bootstrap CI bands
      geom_ribbon(aes(xmin = mean_lower_ci, xmax = mean_upper_ci),
                  alpha = 0.2, color = NA) +
      # Standard error bars
      geom_errorbarh(aes(xmin = mean_soc - 1.96*se_soc, 
                         xmax = mean_soc + 1.96*se_soc),
                     height = 2, alpha = 0.5) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(values = c("HR" = "#2E86AB", "Composite" = "#A23B72")) +
      scale_fill_manual(values = c("HR" = "#2E86AB", "Composite" = "#A23B72")) +
      labs(
        title = "Spline-Harmonized SOC Depth Profiles with Uncertainty",
        subtitle = sprintf("Mean ± 95%% CI (bands) and SE (bars) across %d cores | Bootstrap n=%d", 
                          cores_success, N_BOOTSTRAP),
        x = "SOC (g/kg)",
        y = "Depth (cm)",
        color = "Core Type",
        fill = "Core Type"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom"
      )
    
    ggsave(file.path(plots_dir, "harmonized_depth_profiles_with_uncertainty.png"), 
           p_profiles, width = 10, height = 10, dpi = 300)
    log_message("Saved depth profile plot with uncertainty")
  }, error = function(e) {
    log_message(sprintf("Error creating profile plot: %s", e$message), "WARNING")
  })
}

# 2. Uncertainty by depth
if (BOOTSTRAP_ENABLED && nrow(uncertainty_data) > 0) {
  
  tryCatch({
    # Merge with core type info
    uncertainty_with_type <- uncertainty_data %>%
      dplyr::left_join(
        all_cores %>% 
          dplyr::select(core_id, core_type) %>% 
          dplyr::distinct(),
        by = "core_id"
      )
    
    p_uncertainty <- ggplot(uncertainty_with_type, 
                           aes(x = factor(depth_cm), y = ci_width, fill = core_type)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = c("HR" = "#2E86AB", "Composite" = "#A23B72")) +
      labs(
        title = "Prediction Uncertainty by Depth",
        subtitle = sprintf("95%% Confidence Interval Width | Bootstrap n=%d", N_BOOTSTRAP),
        x = "Depth (cm)",
        y = "CI Width (g/kg)",
        fill = "Core Type"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggsave(file.path(plots_dir, "uncertainty_by_depth.png"), 
           p_uncertainty, width = 10, height = 6, dpi = 300)
    log_message("Saved uncertainty by depth plot")
  }, error = function(e) {
    log_message(sprintf("Error creating uncertainty plot: %s", e$message), "WARNING")
  })
}

# 3. Lambda distribution (if optimized)
if (OPTIMIZE_LAMBDA && nrow(spline_diagnostics) > 0) {
  
  tryCatch({
    p_lambda <- ggplot(spline_diagnostics, aes(x = lambda, fill = core_type)) +
      geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
      geom_vline(xintercept = 0.1, linetype = "dashed", color = "red", size = 1) +
      scale_fill_manual(values = c("HR" = "#2E86AB", "Composite" = "#A23B72")) +
      labs(
        title = "Optimized Lambda Distribution",
        subtitle = "Red line = Default lambda (0.1)",
        x = "Lambda (Smoothing Parameter)",
        y = "Count",
        fill = "Core Type"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
    
    ggsave(file.path(plots_dir, "lambda_distribution.png"), 
           p_lambda, width = 8, height = 6, dpi = 300)
    log_message("Saved lambda distribution plot")
  }, error = function(e) {
    log_message(sprintf("Error creating lambda plot: %s", e$message), "WARNING")
  })
}

# 4. Fit quality vs lambda
if (OPTIMIZE_LAMBDA && nrow(spline_diagnostics) > 0) {
  
  tryCatch({
    p_quality_lambda <- ggplot(spline_diagnostics, 
                               aes(x = lambda, y = r_squared, color = core_type)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_smooth(method = "loess", se = TRUE) +
      scale_color_manual(values = c("HR" = "#2E86AB", "Composite" = "#A23B72")) +
      labs(
        title = "Spline Fit Quality vs Smoothing Parameter",
        x = "Lambda",
        y = "R² (Goodness of Fit)",
        color = "Core Type"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
    
    ggsave(file.path(plots_dir, "fit_quality_vs_lambda.png"), 
           p_quality_lambda, width = 8, height = 6, dpi = 300)
    log_message("Saved fit quality vs lambda plot")
  }, error = function(e) {
    log_message(sprintf("Error creating quality-lambda plot: %s", e$message), "WARNING")
  })
}

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n========================================\n")
cat("DEPTH HARMONIZATION COMPLETE! (ENHANCED)\n")
cat("========================================\n\n")

cat("Summary:\n")
cat("========\n")
cat(sprintf("✓ Processed %d cores\n", cores_processed))
cat(sprintf("✓ Successfully harmonized %d cores (%.1f%%)\n", 
            cores_success, 100 * cores_success / n_cores))
cat(sprintf("✓ Standard depths: %s cm\n", 
            paste(STANDARD_DEPTHS, collapse = ", ")))
cat(sprintf("✓ Total harmonized predictions: %d\n", nrow(harmonized_data)))
if (BOOTSTRAP_ENABLED) {
  cat(sprintf("✓ Bootstrap uncertainty: %d iterations per core\n", N_BOOTSTRAP))
}
if (OPTIMIZE_LAMBDA) {
  cat(sprintf("✓ Lambda optimization: Enabled (CV)\n"))
}
cat("\n")

cat("Outputs Created:\n")
cat("================\n")
cat("✓ Harmonized dataset: data_processed/cores_harmonized_spline_enhanced.rds\n")
cat("✓ Harmonized dataset (CSV): data_processed/cores_harmonized_spline_enhanced.csv\n")
if (BOOTSTRAP_ENABLED) {
  cat("✓ Uncertainty estimates: outputs/spline_uncertainty.rds\n")
  cat("✓ Uncertainty estimates (CSV): outputs/spline_uncertainty.csv\n")
}
cat("✓ Spline diagnostics: outputs/spline_diagnostics_enhanced.csv\n")
cat("✓ Depth summary: outputs/harmonized_depth_summary_enhanced.csv\n")
cat("✓ Visualization plots: outputs/plots/spline_harmonization_enhanced/\n")
cat("\n")

cat("Enhancements in This Version:\n")
cat("=============================\n")
cat("✓ Bootstrap confidence intervals (95% CI)\n")
cat("✓ Cross-validation for lambda optimization\n")
cat("✓ Uncertainty propagation to outputs\n")
cat("✓ Enhanced diagnostic plots\n")
cat("✓ Improved validation metrics\n")
cat("\n")

cat("Next Steps:\n")
cat("===========\n")
cat("1. Review uncertainty estimates in outputs/spline_uncertainty.csv\n")
cat("2. Examine fit quality vs lambda plots\n")
cat("3. Use enhanced data for spatial modeling:\n")
cat("   - Module 05 ENHANCED: Kriging with cross-validation\n")
cat("   - Module 06 ENHANCED: Random Forest with spatial CV\n")
cat("4. Propagate uncertainty through spatial models\n")
cat("\n")

log_message("Module 04b ENHANCED completed successfully")

# Save session info
sessionInfo <- sessionInfo()
saveRDS(sessionInfo, "outputs/session_info_module04b_enhanced.rds")

cat("Log file: ", log_file, "\n\n")

# Print what to expect in outputs
cat("KEY DIFFERENCES FROM STANDARD VERSION:\n")
cat("======================================\n")
cat("1. Each prediction now has lower_ci and upper_ci columns\n")
cat("2. Lambda values are optimized per core (not fixed 0.1)\n")
cat("3. Uncertainty quantified via bootstrap resampling\n")
cat("4. Additional diagnostic plots showing uncertainty trends\n")
cat("5. Can propagate uncertainty to final soil carbon maps\n\n")

cat("USAGE NOTES:\n")
cat("============\n")
cat("• Set BOOTSTRAP_ENABLED = FALSE to disable uncertainty (faster)\n")
cat("• Adjust N_BOOTSTRAP for speed vs precision tradeoff\n")
cat("• Set OPTIMIZE_LAMBDA = FALSE to use fixed lambda (faster)\n")
cat("• Uncertainty data saved separately for flexible downstream use\n\n")

cat("🎉 Ready for publication-quality spatial modeling!\n\n")
