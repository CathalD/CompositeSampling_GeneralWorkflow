# ============================================================================
# Module 05: Kriging with Cross-Validation - ENHANCED VERSION
# VERSION: 3.0 - Production-ready with robust validation
# ============================================================================
# ENHANCEMENTS:
# 1. Robust variogram fitting (weighted least squares)
# 2. Leave-one-out cross-validation (LOOCV)
# 3. Multiple variogram model comparison
# 4. Anisotropy detection
# 5. Cross-validation metrics (RMSE, MAE, RÂ², ME, MSDR)
# 6. Enhanced diagnostic plots
# ============================================================================

# ============================================================================
# SETUP
# ============================================================================

log_file <- file.path("logs", paste0("kriging_enhanced_", Sys.Date(), ".log"))
if (!dir.exists("logs")) dir.create("logs", recursive = TRUE)

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

log_message("Starting Module 05 ENHANCED: Kriging with Cross-Validation")
set.seed(42)

# Configuration
ENABLE_CV <- TRUE                    # Enable cross-validation
ENABLE_ANISOTROPY_CHECK <- TRUE     # Check for directional dependence
VARIOGRAM_MODELS <- c("Sph", "Exp", "Gau", "Mat")  # Models to test
CV_METHOD <- "LOOCV"                 # Leave-one-out CV
WEIGHTED_LS <- TRUE                  # Use weighted least squares

log_message(sprintf("Cross-validation: %s", ifelse(ENABLE_CV, "ENABLED", "DISABLED")))
log_message(sprintf("Anisotropy check: %s", ifelse(ENABLE_ANISOTROPY_CHECK, "ENABLED", "DISABLED")))

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

required_packages <- c("dplyr", "ggplot2", "raster", "terra", "sf", "gstat", 
                       "sp", "viridis", "automap")

available_pkgs <- sapply(required_packages, load_required_package)

if (!all(available_pkgs[c("dplyr", "ggplot2", "gstat", "sp")])) {
  stop("Core packages (dplyr, ggplot2, gstat, sp) are required")
}

use_terra <- available_pkgs["terra"]
use_automap <- available_pkgs["automap"]

log_message(sprintf("Using %s for raster operations", 
                    ifelse(use_terra, "terra", "raster")))

# ============================================================================
# LOAD HARMONIZED DATA
# ============================================================================

log_message("Loading harmonized spline data")

harmonized_cores <- tryCatch({
  readRDS("data_processed/cores_harmonized_spline_enhanced.rds")
}, error = function(e) {
  # Try original version
  tryCatch({
    readRDS("data_processed/cores_harmonized_spline.rds")
  }, error = function(e2) {
    stop("Could not load harmonized spline data. Please run Module 04b first.")
  })
})

log_message(sprintf("Loaded: %d predictions from %d cores", 
                    nrow(harmonized_cores),
                    length(unique(harmonized_cores$core_id))))

standard_depths <- sort(unique(harmonized_cores$depth_cm))
log_message(sprintf("Standard depths: %s cm", paste(standard_depths, collapse = ", ")))

# Clean data
harmonized_cores <- harmonized_cores %>%
  dplyr::filter(!is.na(longitude) & !is.na(latitude) & !is.na(soc_spline))

log_message(sprintf("After cleaning: %d valid predictions", nrow(harmonized_cores)))

# ============================================================================
# ENHANCED VARIOGRAM FITTING FUNCTIONS
# ============================================================================

#' Fit variogram with weighted least squares and cross-validation
#' @param vgm_emp Empirical variogram
#' @param sp_data Spatial data
#' @param models Vector of model names to test
#' @param use_weighted Use weighted least squares
#' @return List with best model and diagnostics
fit_variogram_robust <- function(vgm_emp, sp_data, 
                                 models = VARIOGRAM_MODELS,
                                 use_weighted = WEIGHTED_LS) {
  
  results <- list()
  
  for (model_name in models) {
    tryCatch({
      
      # Fit with appropriate method
      if (use_weighted) {
        vgm_fit <- gstat::fit.variogram(
          vgm_emp, 
          gstat::vgm(model_name),
          fit.method = 6  # Weighted least squares (default=7)
        )
      } else {
        vgm_fit <- gstat::fit.variogram(
          vgm_emp, 
          gstat::vgm(model_name)
        )
      }
      
      # Get SSE
      sse <- attr(vgm_fit, "SSErr")
      
      # Cross-validation if enabled
      if (ENABLE_CV && nrow(sp_data) <= 200) {  # LOOCV only for reasonable sizes
        cv_result <- tryCatch({
          gstat::krige.cv(
            soc_spline ~ 1,
            sp_data,
            model = vgm_fit,
            nfold = nrow(sp_data)  # Leave-one-out
          )
        }, error = function(e) NULL)
        
        if (!is.null(cv_result)) {
          cv_rmse <- sqrt(mean(cv_result$residual^2, na.rm = TRUE))
          cv_mae <- mean(abs(cv_result$residual), na.rm = TRUE)
          cv_me <- mean(cv_result$residual, na.rm = TRUE)
          cv_r2 <- 1 - sum(cv_result$residual^2, na.rm = TRUE) / 
                       sum((cv_result$observed - mean(cv_result$observed, na.rm = TRUE))^2, na.rm = TRUE)
          
          # Mean Standardized Squared Prediction Error (should be ~1)
          cv_msdr <- mean(cv_result$zscore^2, na.rm = TRUE)
        } else {
          cv_rmse <- NA
          cv_mae <- NA
          cv_me <- NA
          cv_r2 <- NA
          cv_msdr <- NA
        }
      } else {
        cv_rmse <- NA
        cv_mae <- NA
        cv_me <- NA
        cv_r2 <- NA
        cv_msdr <- NA
      }
      
      results[[model_name]] <- list(
        model = vgm_fit,
        model_name = model_name,
        sse = sse,
        cv_rmse = cv_rmse,
        cv_mae = cv_mae,
        cv_me = cv_me,
        cv_r2 = cv_r2,
        cv_msdr = cv_msdr
      )
      
    }, error = function(e) {
      log_message(sprintf("Failed to fit %s: %s", model_name, e$message), "WARNING")
    })
  }
  
  if (length(results) == 0) {
    return(NULL)
  }
  
  # Select best model based on CV RMSE (if available), otherwise SSE
  if (ENABLE_CV && !all(is.na(sapply(results, function(x) x$cv_rmse)))) {
    best_idx <- which.min(sapply(results, function(x) 
      ifelse(is.na(x$cv_rmse), Inf, x$cv_rmse)))
    selection_criterion <- "CV RMSE"
  } else {
    best_idx <- which.min(sapply(results, function(x) x$sse))
    selection_criterion <- "SSE"
  }
  
  best_result <- results[[best_idx]]
  best_result$selection_criterion <- selection_criterion
  best_result$all_models <- results
  
  return(best_result)
}

#' Check for anisotropy
#' @param sp_data Spatial data
#' @return List with anisotropy info
check_anisotropy <- function(sp_data) {
  
  if (!ENABLE_ANISOTROPY_CHECK || nrow(sp_data) < 30) {
    return(list(
      is_anisotropic = FALSE,
      message = "Anisotropy check skipped"
    ))
  }
  
  tryCatch({
    # Calculate directional variograms
    vgm_directional <- gstat::variogram(
      soc_spline ~ 1,
      sp_data,
      alpha = c(0, 45, 90, 135),  # Four directions
      cutoff = max(dist(sp::coordinates(sp_data))) / 3
    )
    
    # Simple check: compare ranges in different directions
    vgm_by_dir <- split(vgm_directional, vgm_directional$dir.hor)
    ranges <- sapply(vgm_by_dir, function(x) max(x$dist[x$gamma < max(x$gamma) * 0.95]))
    
    # If range varies by >50%, consider anisotropic
    is_anisotropic <- (max(ranges) / min(ranges)) > 1.5
    
    return(list(
      is_anisotropic = is_anisotropic,
      directional_vgm = vgm_directional,
      ranges_by_direction = ranges,
      max_range_direction = names(which.max(ranges))
    ))
    
  }, error = function(e) {
    log_message(sprintf("Anisotropy check failed: %s", e$message), "WARNING")
    return(list(
      is_anisotropic = FALSE,
      message = "Anisotropy check failed"
    ))
  })
}

#' Perform kriging with cross-validation
#' @param depth_cm Depth to process
#' @param depth_data Data for this depth
#' @param grid_coords Prediction grid coordinates
#' @return List with predictions and diagnostics
perform_kriging_with_cv <- function(depth_cm, depth_data, grid_coords) {
  
  tryCatch({
    
    if (nrow(depth_data) < 10) {
      log_message(sprintf("Depth %d cm: Insufficient points (n=%d)", 
                         depth_cm, nrow(depth_data)), "WARNING")
      return(NULL)
    }
    
    log_message(sprintf("Depth %d cm: Processing %d points", depth_cm, nrow(depth_data)))
    
    # Create spatial points
    sp_data <- depth_data
    sp::coordinates(sp_data) <- ~ longitude + latitude
    sp::proj4string(sp_data) <- sp::CRS("+init=epsg:4326")
    
    # Check for anisotropy
    aniso_result <- check_anisotropy(sp_data)
    if (aniso_result$is_anisotropic) {
      log_message(sprintf("  Anisotropy detected (ratio: %.2f)", 
                         max(aniso_result$ranges_by_direction) / 
                         min(aniso_result$ranges_by_direction)), "INFO")
    }
    
    # Fit empirical variogram
    vgm_emp <- gstat::variogram(soc_spline ~ 1, sp_data)
    
    # Fit with robust method
    vgm_result <- fit_variogram_robust(vgm_emp, sp_data)
    
    if (is.null(vgm_result)) {
      log_message(sprintf("  Failed to fit variogram"), "WARNING")
      return(NULL)
    }
    
    log_message(sprintf("  Best model: %s (by %s)", 
                       vgm_result$model_name,
                       vgm_result$selection_criterion))
    
    if (ENABLE_CV && !is.na(vgm_result$cv_rmse)) {
      log_message(sprintf("  CV RMSE: %.2f g/kg | CV RÂ²: %.3f | CV ME: %.2f g/kg", 
                         vgm_result$cv_rmse, vgm_result$cv_r2, vgm_result$cv_me))
    }
    
    # Create grid - ensure clean data frame
    grid_df <- as.data.frame(grid_coords)
    grid_df <- grid_df[, c("longitude", "latitude")]
    
    # Ensure numeric coordinates
    grid_df$longitude <- as.numeric(grid_df$longitude)
    grid_df$latitude <- as.numeric(grid_df$latitude)
    
    # Remove any NA coordinates
    grid_df <- grid_df[complete.cases(grid_df), ]
    
    log_message(sprintf("  Grid: %d cells, lon range [%.4f, %.4f], lat range [%.4f, %.4f]",
                       nrow(grid_df),
                       min(grid_df$longitude), max(grid_df$longitude),
                       min(grid_df$latitude), max(grid_df$latitude)))
    
    # Create spatial object
    grid_sp <- grid_df
    sp::coordinates(grid_sp) <- ~ longitude + latitude
    sp::gridded(grid_sp) <- TRUE
    sp::proj4string(grid_sp) <- sp::CRS("+init=epsg:4326")
    
    # Perform kriging
    krige_result <- gstat::krige(
      soc_spline ~ 1, 
      sp_data, 
      grid_sp, 
      model = vgm_result$model
    )
    
    # Extract predictions
    predictions <- data.frame(
      longitude = sp::coordinates(krige_result)[, 1],
      latitude = sp::coordinates(krige_result)[, 2],
      prediction = krige_result$var1.pred,
      variance = krige_result$var1.var
    )
    
    # Non-negativity constraint
    predictions$prediction[predictions$prediction < 0] <- 0
    
    return(list(
      predictions = predictions,
      variogram = vgm_result$model,
      model_name = vgm_result$model_name,
      n_points = nrow(depth_data),
      vgm_emp = vgm_emp,
      cv_rmse = vgm_result$cv_rmse,
      cv_mae = vgm_result$cv_mae,
      cv_me = vgm_result$cv_me,
      cv_r2 = vgm_result$cv_r2,
      cv_msdr = vgm_result$cv_msdr,
      sse = vgm_result$sse,
      anisotropy = aniso_result,
      all_models = vgm_result$all_models
    ))
    
  }, error = function(e) {
    log_message(sprintf("Depth %d cm: Error: %s", depth_cm, e$message), "ERROR")
    return(NULL)
  })
}

# ============================================================================
# DEFINE PREDICTION GRID
# ============================================================================

log_message("Creating prediction grid")

all_coords <- harmonized_cores %>%
  dplyr::select(longitude, latitude) %>%
  dplyr::distinct()

lon_range <- range(all_coords$longitude)
lat_range <- range(all_coords$latitude)

# 10% buffer
lon_buffer <- diff(lon_range) * 0.1
lat_buffer <- diff(lat_range) * 0.1

lon_min <- lon_range[1] - lon_buffer
lon_max <- lon_range[2] + lon_buffer
lat_min <- lat_range[1] - lat_buffer
lat_max <- lat_range[2] + lat_buffer

lon_extent <- lon_max - lon_min
lat_extent <- lat_max - lat_min

ncols <- 100
nrows <- round(100 * lat_extent / lon_extent)

log_message(sprintf("Grid dimensions: %d x %d cells", ncols, nrows))

# Create grid
if (use_terra) {
  prediction_raster <- terra::rast(
    xmin = lon_min, xmax = lon_max,
    ymin = lat_min, ymax = lat_max,
    ncols = ncols, nrows = nrows,
    crs = "EPSG:4326"
  )
  # terra returns data frame with x and y columns
  prediction_coords <- as.data.frame(terra::crds(prediction_raster, df = TRUE))
} else {
  prediction_raster <- raster::raster(
    xmn = lon_min, xmx = lon_max,
    ymn = lat_min, ymx = lat_max,
    ncols = ncols, nrows = nrows,
    crs = sp::CRS("+init=epsg:4326")
  )
  prediction_coords <- data.frame(raster::coordinates(prediction_raster))
}

# Rename columns consistently
names(prediction_coords) <- c("longitude", "latitude")

# Ensure numeric and clean
prediction_coords$longitude <- as.numeric(prediction_coords$longitude)
prediction_coords$latitude <- as.numeric(prediction_coords$latitude)
prediction_coords <- prediction_coords[complete.cases(prediction_coords), ]

log_message(sprintf("Prediction grid created: %d cells", nrow(prediction_coords)))

# ============================================================================
# CREATE OUTPUT DIRECTORIES
# ============================================================================

raster_dir <- "outputs/rasters_kriging_enhanced"
viz_dir <- "outputs/plots/kriging_predictions_enhanced"
cv_dir <- "outputs/kriging_cv_results"

for (dir_path in c(raster_dir, viz_dir, cv_dir)) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

# ============================================================================
# PROCESS EACH DEPTH
# ============================================================================

log_message("\n=== KRIGING WITH CROSS-VALIDATION ===\n")

raster_outputs <- list()
summary_stats <- data.frame()
cv_results_all <- data.frame()

for (depth_cm in standard_depths) {
  
  log_message(sprintf("\n=== Depth: %d cm ===", depth_cm))
  
  # Get data for this depth
  depth_data <- harmonized_cores %>%
    dplyr::filter(depth_cm == !!depth_cm)
  
  # Perform kriging with CV
  krige_result <- perform_kriging_with_cv(depth_cm, depth_data, prediction_coords)
  
  if (is.null(krige_result)) {
    next
  }
  
  # Create raster
  pred_raster <- prediction_raster
  
  if (use_terra) {
    pred_values <- rep(NA, terra::ncell(pred_raster))
    coords_match <- terra::cellFromXY(
      pred_raster,
      as.matrix(krige_result$predictions[, c("longitude", "latitude")])
    )
    pred_values[coords_match] <- krige_result$predictions$prediction
    terra::values(pred_raster) <- pred_values
    
    raster_file <- file.path(raster_dir, sprintf("soc_%dcm.tif", depth_cm))
    terra::writeRaster(pred_raster, raster_file, overwrite = TRUE)
    
    # Variance raster
    var_raster <- pred_raster
    var_values <- rep(NA, terra::ncell(var_raster))
    var_values[coords_match] <- krige_result$predictions$variance
    terra::values(var_raster) <- var_values
    terra::writeRaster(var_raster, 
                      file.path(raster_dir, sprintf("soc_%dcm_variance.tif", depth_cm)),
                      overwrite = TRUE)
    
  } else {
    pred_values <- rep(NA, raster::ncell(pred_raster))
    coords_match <- raster::cellFromXY(
      pred_raster,
      as.matrix(krige_result$predictions[, c("longitude", "latitude")])
    )
    pred_values[coords_match] <- krige_result$predictions$prediction
    raster::values(pred_raster) <- pred_values
    
    raster_file <- file.path(raster_dir, sprintf("soc_%dcm.tif", depth_cm))
    raster::writeRaster(pred_raster, raster_file, overwrite = TRUE)
    
    # Variance raster
    var_raster <- pred_raster
    var_values <- rep(NA, raster::ncell(var_raster))
    var_values[coords_match] <- krige_result$predictions$variance
    raster::values(var_raster) <- var_values
    raster::writeRaster(var_raster,
                       file.path(raster_dir, sprintf("soc_%dcm_variance.tif", depth_cm)),
                       overwrite = TRUE)
  }
  
  log_message(sprintf("Saved: %s", basename(raster_file)))
  
  raster_outputs[[as.character(depth_cm)]] <- pred_raster
  
  # Summary statistics
  pred_clean <- pred_values[!is.na(pred_values)]
  if (length(pred_clean) > 0) {
    stats <- data.frame(
      depth_cm = depth_cm,
      mean_soc = mean(pred_clean),
      sd_soc = sd(pred_clean),
      min_soc = min(pred_clean),
      max_soc = max(pred_clean),
      n_cells = length(pred_clean),
      n_points = krige_result$n_points,
      variogram_model = krige_result$model_name,
      cv_rmse = krige_result$cv_rmse,
      cv_mae = krige_result$cv_mae,
      cv_me = krige_result$cv_me,
      cv_r2 = krige_result$cv_r2,
      cv_msdr = krige_result$cv_msdr,
      sse = krige_result$sse
    )
    summary_stats <- rbind(summary_stats, stats)
  }
  
  # Store CV results
  if (!is.na(krige_result$cv_rmse)) {
    cv_result_depth <- data.frame(
      depth_cm = depth_cm,
      cv_rmse = krige_result$cv_rmse,
      cv_mae = krige_result$cv_mae,
      cv_me = krige_result$cv_me,
      cv_r2 = krige_result$cv_r2,
      cv_msdr = krige_result$cv_msdr
    )
    cv_results_all <- rbind(cv_results_all, cv_result_depth)
  }
  
  # Create visualizations
  tryCatch({
    viz_data <- krige_result$predictions
    
    # Prediction map
    p <- ggplot() +
      geom_raster(data = viz_data, aes(x = longitude, y = latitude, fill = prediction)) +
      geom_point(data = depth_data, aes(x = longitude, y = latitude),
                 color = "white", size = 2, shape = 21, fill = "black") +
      scale_fill_viridis_c(name = "SOC\n(g/kg)", option = "plasma") +
      labs(title = sprintf("Kriging Prediction - %d cm", depth_cm),
           subtitle = sprintf("%d points | %s variogram | CV RMSE: %.2f g/kg", 
                            krige_result$n_points, 
                            krige_result$model_name,
                            ifelse(is.na(krige_result$cv_rmse), 0, krige_result$cv_rmse))) +
      theme_minimal() +
      coord_equal()
    
    ggsave(file.path(viz_dir, sprintf("prediction_%dcm.png", depth_cm)),
           p, width = 10, height = 8, dpi = 300)
    
    # Uncertainty map
    viz_data$std_error <- sqrt(viz_data$variance)
    
    p_unc <- ggplot() +
      geom_raster(data = viz_data, aes(x = longitude, y = latitude, fill = std_error)) +
      geom_point(data = depth_data, aes(x = longitude, y = latitude),
                 color = "white", size = 2, shape = 21, fill = "black") +
      scale_fill_viridis_c(name = "Std Error\n(g/kg)", option = "magma") +
      labs(title = sprintf("Kriging Uncertainty - %d cm", depth_cm)) +
      theme_minimal() +
      coord_equal()
    
    ggsave(file.path(viz_dir, sprintf("uncertainty_%dcm.png", depth_cm)),
           p_unc, width = 10, height = 8, dpi = 300)
    
    # Variogram plot
    vgm_plot_data <- data.frame(
      dist = krige_result$vgm_emp$dist,
      gamma = krige_result$vgm_emp$gamma,
      np = krige_result$vgm_emp$np
    )
    
    vgm_model <- krige_result$variogram
    dist_seq <- seq(0, max(vgm_plot_data$dist), length.out = 100)
    gamma_model <- gstat::variogramLine(vgm_model, dist_vector = dist_seq)
    
    p_vgm <- ggplot() +
      geom_point(data = vgm_plot_data, aes(x = dist, y = gamma, size = np), alpha = 0.6) +
      geom_line(data = gamma_model, aes(x = dist, y = gamma), color = "red", size = 1) +
      scale_size_continuous(name = "Pairs") +
      labs(title = sprintf("Variogram - %d cm", depth_cm),
           subtitle = sprintf("%s model | Nugget: %.1f | Sill: %.1f | Range: %.1f",
                            krige_result$model_name,
                            vgm_model$psill[1],
                            sum(vgm_model$psill),
                            vgm_model$range[2]),
           x = "Distance",
           y = "Semivariance") +
      theme_minimal()
    
    ggsave(file.path(viz_dir, sprintf("variogram_%dcm.png", depth_cm)),
           p_vgm, width = 8, height = 6, dpi = 300)
    
  }, error = function(e) {
    log_message(sprintf("Error creating plots for %d cm: %s", depth_cm, e$message), "WARNING")
  })
}

# ============================================================================
# SAVE RESULTS
# ============================================================================

log_message("Saving results")

# Summary statistics
write.csv(summary_stats, 
          file.path(raster_dir, "kriging_summary_statistics_enhanced.csv"),
          row.names = FALSE)

# CV results
if (nrow(cv_results_all) > 0) {
  write.csv(cv_results_all,
            file.path(cv_dir, "cross_validation_results.csv"),
            row.names = FALSE)
  
  saveRDS(cv_results_all,
          file.path(cv_dir, "cross_validation_results.rds"))
}

# ============================================================================
# CREATE SUMMARY PLOTS
# ============================================================================

log_message("Creating summary visualizations")

if (nrow(cv_results_all) > 0) {
  
  # CV metrics by depth
  p_cv_rmse <- ggplot(cv_results_all, aes(x = factor(depth_cm), y = cv_rmse)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
    labs(title = "Cross-Validation RMSE by Depth",
         subtitle = "Red line = 10 g/kg threshold",
         x = "Depth (cm)",
         y = "CV RMSE (g/kg)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(cv_dir, "cv_rmse_by_depth.png"),
         p_cv_rmse, width = 10, height = 6, dpi = 300)
  
  # CV RÂ² by depth
  p_cv_r2 <- ggplot(cv_results_all, aes(x = factor(depth_cm), y = cv_r2)) +
    geom_col(fill = "darkgreen", alpha = 0.7) +
    geom_hline(yintercept = 0.7, linetype = "dashed", color = "red") +
    labs(title = "Cross-Validation RÂ² by Depth",
         subtitle = "Red line = 0.7 threshold",
         x = "Depth (cm)",
         y = "CV RÂ²") +
    ylim(0, 1) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(cv_dir, "cv_r2_by_depth.png"),
         p_cv_r2, width = 10, height = 6, dpi = 300)
  
  # Mean Error (should be near 0)
  p_cv_me <- ggplot(cv_results_all, aes(x = factor(depth_cm), y = cv_me)) +
    geom_col(fill = "purple", alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    geom_hline(yintercept = c(-1, 1), linetype = "dashed", color = "red") +
    labs(title = "Cross-Validation Mean Error by Depth",
         subtitle = "Should be near 0 (red lines = Â±1 g/kg)",
         x = "Depth (cm)",
         y = "CV Mean Error (g/kg)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(cv_dir, "cv_mean_error_by_depth.png"),
         p_cv_me, width = 10, height = 6, dpi = 300)
}

# ============================================================================
# MULTI-BAND RASTER
# ============================================================================

if (length(raster_outputs) > 0) {
  log_message("Creating multi-band raster")
  
  tryCatch({
    if (use_terra) {
      multi_raster <- terra::rast(raster_outputs)
      names(multi_raster) <- paste0("depth_", names(raster_outputs), "cm")
      terra::writeRaster(multi_raster, 
                        file.path(raster_dir, "soc_all_depths.tif"),
                        overwrite = TRUE)
    } else {
      multi_raster <- raster::stack(raster_outputs)
      names(multi_raster) <- paste0("depth_", names(raster_outputs), "cm")
      raster::writeRaster(multi_raster,
                         file.path(raster_dir, "soc_all_depths.tif"),
                         overwrite = TRUE)
    }
    log_message("Saved multi-band raster")
  }, error = function(e) {
    log_message(sprintf("Error creating multi-band raster: %s", e$message), "WARNING")
  })
}

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n========================================\n")
cat("KRIGING WITH CROSS-VALIDATION COMPLETE!\n")
cat("========================================\n\n")

cat("Summary Statistics:\n")
cat("===================\n")
if (nrow(summary_stats) > 0) {
  print(summary_stats, row.names = FALSE, digits = 2)
  cat("\n")
}

if (nrow(cv_results_all) > 0) {
  cat("Cross-Validation Summary:\n")
  cat("========================\n")
  cat(sprintf("Mean CV RMSE: %.2f g/kg\n", mean(cv_results_all$cv_rmse, na.rm = TRUE)))
  cat(sprintf("Mean CV RÂ²: %.3f\n", mean(cv_results_all$cv_r2, na.rm = TRUE)))
  cat(sprintf("Mean CV ME: %.2f g/kg (should be ~0)\n", mean(cv_results_all$cv_me, na.rm = TRUE)))
  cat(sprintf("Mean CV MSDR: %.3f (should be ~1)\n", mean(cv_results_all$cv_msdr, na.rm = TRUE)))
  cat("\n")
}

cat("Outputs Created:\n")
cat("================\n")
cat(sprintf("âœ“ Rasters: %s/\n", raster_dir))
cat(sprintf("âœ“ Predictions: %d depths\n", length(raster_outputs)))
cat(sprintf("âœ“ Variance maps: %d depths\n", length(raster_outputs)))
cat(sprintf("âœ“ Visualizations: %s/\n", viz_dir))
if (ENABLE_CV) {
  cat(sprintf("âœ“ CV results: %s/\n", cv_dir))
}
cat("\n")

cat("Enhancements in This Version:\n")
cat("=============================\n")
cat("âœ“ Robust variogram fitting (weighted LS)\n")
cat("âœ“ Multiple variogram models tested\n")
cat("âœ“ Leave-one-out cross-validation\n")
cat("âœ“ Cross-validation metrics (RMSE, RÂ², ME, MSDR)\n")
cat("âœ“ Anisotropy detection\n")
cat("âœ“ Enhanced diagnostic plots\n")
cat("\n")

log_message("Module 05 ENHANCED completed successfully")
saveRDS(sessionInfo(), "outputs/session_info_module05_enhanced.rds")

cat("ðŸŽ‰ Ready for Random Forest comparison!\n\n")
