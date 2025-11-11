# ============================================================================
# Module 05: Uncertainty Quantification via Bootstrap
# VERSION: 1.0
# ============================================================================
# Purpose: Quantify uncertainty in key estimates using bootstrap resampling
# Provides confidence intervals for means, depth profiles, spatial predictions
# Enables transparent reporting of statistical confidence
# ============================================================================

cat("========================================\n")
cat("MODULE 05: UNCERTAINTY QUANTIFICATION\n")
cat("========================================\n\n")

# ============================================================================
# 1. SETUP AND CONFIGURATION
# ============================================================================

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,    # Data manipulation
  boot,         # Bootstrap methods
  nlme,         # Mixed models for bootstrap
  mgcv,         # GAMs for bootstrap
  parallel,     # Parallel processing
  foreach,      # Parallel loops
  doParallel,   # Parallel backend
  ggplot2,      # Visualization
  patchwork,    # Combining plots
  knitr,        # Tables
  here          # File paths
)

# Create directories
dir.create("outputs/uncertainty", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/uncertainty/plots", showWarnings = FALSE, recursive = TRUE)

# Setup logging
log_file <- file.path("outputs/uncertainty", paste0("uncertainty_log_", Sys.Date(), ".txt"))
log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

log_message("Starting uncertainty quantification module")

# ============================================================================
# 2. BOOTSTRAP CONFIGURATION
# ============================================================================

# Set bootstrap parameters
BOOTSTRAP_CONFIG <- list(
  n_boot = 1000,              # Number of bootstrap iterations (increase for publication)
  conf_level = 0.95,          # Confidence level (95%)
  parallel = TRUE,            # Use parallel processing
  n_cores = detectCores() - 1, # Leave one core free
  seed = 123                  # Reproducibility
)

cat("Bootstrap Configuration:\n")
cat("  Iterations:", BOOTSTRAP_CONFIG$n_boot, "\n")
cat("  Confidence Level:", BOOTSTRAP_CONFIG$conf_level * 100, "%\n")
cat("  Parallel Processing:", BOOTSTRAP_CONFIG$parallel, "\n")
if (BOOTSTRAP_CONFIG$parallel) {
  cat("  Cores:", BOOTSTRAP_CONFIG$n_cores, "\n")
}
cat("\n")

set.seed(BOOTSTRAP_CONFIG$seed)

# ============================================================================
# 3. LOAD DATA
# ============================================================================

log_message("Loading data from previous modules")

# Check if data exists
if (!file.exists("outputs/processed_cores.rds")) {
  stop("Error: Processed data not found. Run Module 01 first.")
}

# Load processed data
hr_cores <- readRDS("outputs/hr_cores_processed.rds")
composite_cores <- readRDS("outputs/composite_cores_processed.rds")
combined_cores <- readRDS("outputs/processed_cores.rds")

# Check for models from Module 03
has_depth_models <- file.exists("outputs/models/lme_soc_best.rds")
if (has_depth_models) {
  best_lme <- readRDS("outputs/models/lme_soc_best.rds")
  log_message("Loaded depth models for bootstrap")
}

cat("Data loaded successfully:\n")
cat("  HR cores:", nrow(hr_cores), "observations\n")
cat("  Composite cores:", nrow(composite_cores), "observations\n")
cat("  Combined:", nrow(combined_cores), "observations\n\n")

# ============================================================================
# 4. HELPER FUNCTIONS FOR BOOTSTRAP
# ============================================================================

#' Calculate basic statistics for a sample
#' @param data Data frame with SOC values
#' @param indices Bootstrap sample indices
#' @return Vector of statistics
basic_stats_boot <- function(data, indices) {
  d <- data[indices, ]
  c(
    mean = mean(d$soc, na.rm = TRUE),
    median = median(d$soc, na.rm = TRUE),
    sd = sd(d$soc, na.rm = TRUE),
    cv = sd(d$soc, na.rm = TRUE) / mean(d$soc, na.rm = TRUE) * 100
  )
}

#' Calculate depth-specific means for bootstrap
#' @param data Data frame with depth and SOC
#' @param indices Bootstrap sample indices
#' @return Vector of means by depth
depth_means_boot <- function(data, indices) {
  d <- data[indices, ]
  
  # Define standard depth breaks
  depth_breaks <- c(0, 10, 20, 30, 50, 100)
  
  # Calculate means for each depth interval
  means <- sapply(1:(length(depth_breaks) - 1), function(i) {
    depth_data <- d %>%
      filter(depth_top >= depth_breaks[i] & depth_bottom <= depth_breaks[i + 1])
    
    if (nrow(depth_data) > 0) {
      return(mean(depth_data$soc, na.rm = TRUE))
    } else {
      return(NA)
    }
  })
  
  return(means)
}

#' Bootstrap for comparing two groups
#' @param data1 First group data
#' @param data2 Second group data
#' @param indices Bootstrap sample indices
#' @return Difference in means
comparison_boot <- function(data1, data2, indices1, indices2) {
  mean(data1[indices1, ]$soc, na.rm = TRUE) - 
    mean(data2[indices2, ]$soc, na.rm = TRUE)
}

#' Format confidence interval for reporting
#' @param boot_obj Boot object
#' @param type Type of CI (default "perc")
#' @return Formatted data frame
format_ci <- function(boot_obj, type = "perc", conf = BOOTSTRAP_CONFIG$conf_level) {
  ci <- boot.ci(boot_obj, conf = conf, type = type)
  
  if (type == "perc") {
    ci_vals <- ci$percent[4:5]
  } else if (type == "bca") {
    ci_vals <- ci$bca[4:5]
  } else {
    ci_vals <- ci$normal[2:3]
  }
  
  data.frame(
    estimate = boot_obj$t0,
    lower_ci = ci_vals[1],
    upper_ci = ci_vals[2],
    se = sd(boot_obj$t),
    bias = mean(boot_obj$t) - boot_obj$t0
  )
}

# ============================================================================
# 5. BOOTSTRAP: BASIC STATISTICS
# ============================================================================

cat("========================================\n")
cat("SECTION 1: Basic Statistics\n")
cat("========================================\n\n")

log_message("Bootstrapping basic statistics")

# Initialize results storage
basic_results <- list()

# Setup parallel processing if enabled
if (BOOTSTRAP_CONFIG$parallel) {
  cl <- makeCluster(BOOTSTRAP_CONFIG$n_cores)
  registerDoParallel(cl)
  log_message(paste("Parallel cluster started with", BOOTSTRAP_CONFIG$n_cores, "cores"))
}

# Bootstrap for HR cores
cat("Bootstrapping HR cores statistics...\n")
hr_boot <- boot(
  data = hr_cores,
  statistic = basic_stats_boot,
  R = BOOTSTRAP_CONFIG$n_boot,
  parallel = ifelse(BOOTSTRAP_CONFIG$parallel, "multicore", "no"),
  ncpus = BOOTSTRAP_CONFIG$n_cores
)

# Bootstrap for composite cores
cat("Bootstrapping composite cores statistics...\n")
comp_boot <- boot(
  data = composite_cores,
  statistic = basic_stats_boot,
  R = BOOTSTRAP_CONFIG$n_boot,
  parallel = ifelse(BOOTSTRAP_CONFIG$parallel, "multicore", "no"),
  ncpus = BOOTSTRAP_CONFIG$n_cores
)

# Bootstrap for combined data
cat("Bootstrapping combined data statistics...\n")
combined_boot <- boot(
  data = combined_cores,
  statistic = basic_stats_boot,
  R = BOOTSTRAP_CONFIG$n_boot,
  parallel = ifelse(BOOTSTRAP_CONFIG$parallel, "multicore", "no"),
  ncpus = BOOTSTRAP_CONFIG$n_cores
)

# Extract confidence intervals
stat_names <- c("mean", "median", "sd", "cv")

hr_basic_ci <- lapply(1:4, function(i) {
  ci <- boot.ci(hr_boot, conf = BOOTSTRAP_CONFIG$conf_level, type = "perc", index = i)
  data.frame(
    statistic = stat_names[i],
    dataset = "HR Cores",
    estimate = hr_boot$t0[i],
    lower_ci = ci$percent[4],
    upper_ci = ci$percent[5],
    se = sd(hr_boot$t[, i])
  )
}) %>% bind_rows()

comp_basic_ci <- lapply(1:4, function(i) {
  ci <- boot.ci(comp_boot, conf = BOOTSTRAP_CONFIG$conf_level, type = "perc", index = i)
  data.frame(
    statistic = stat_names[i],
    dataset = "Composite Cores",
    estimate = comp_boot$t0[i],
    lower_ci = ci$percent[4],
    upper_ci = ci$percent[5],
    se = sd(comp_boot$t[, i])
  )
}) %>% bind_rows()

combined_basic_ci <- lapply(1:4, function(i) {
  ci <- boot.ci(combined_boot, conf = BOOTSTRAP_CONFIG$conf_level, type = "perc", index = i)
  data.frame(
    statistic = stat_names[i],
    dataset = "Combined",
    estimate = combined_boot$t0[i],
    lower_ci = ci$percent[4],
    upper_ci = ci$percent[5],
    se = sd(combined_boot$t[, i])
  )
}) %>% bind_rows()

# Combine results
basic_results$summary <- bind_rows(hr_basic_ci, comp_basic_ci, combined_basic_ci)

# Print results
cat("\nBasic Statistics with 95% Confidence Intervals:\n")
cat("================================================\n\n")
print(kable(basic_results$summary, digits = 2, format = "simple"))

# Save results
saveRDS(basic_results, "outputs/uncertainty/basic_statistics_bootstrap.rds")
log_message("Basic statistics bootstrap completed")

# ============================================================================
# 6. BOOTSTRAP: DEPTH PROFILES
# ============================================================================

cat("\n========================================\n")
cat("SECTION 2: Depth Profiles\n")
cat("========================================\n\n")

log_message("Bootstrapping depth profiles")

# Define depth intervals
depth_intervals <- data.frame(
  interval = c("0-10", "10-20", "20-30", "30-50", "50-100"),
  depth_top = c(0, 10, 20, 30, 50),
  depth_bottom = c(10, 20, 30, 50, 100)
)

# Function to bootstrap depth-specific means
bootstrap_depth_profile <- function(data, depth_intervals, n_boot = BOOTSTRAP_CONFIG$n_boot) {
  
  results <- list()
  
  for (i in 1:nrow(depth_intervals)) {
    cat("  Depth interval:", depth_intervals$interval[i], "cm\n")
    
    # Subset data for this depth
    depth_data <- data %>%
      filter(depth_top >= depth_intervals$depth_top[i] & 
             depth_bottom <= depth_intervals$depth_bottom[i])
    
    if (nrow(depth_data) < 10) {
      warning(paste("Insufficient data for depth", depth_intervals$interval[i]))
      next
    }
    
    # Bootstrap mean SOC
    boot_result <- boot(
      data = depth_data,
      statistic = function(d, indices) mean(d[indices, ]$soc, na.rm = TRUE),
      R = n_boot,
      parallel = ifelse(BOOTSTRAP_CONFIG$parallel, "multicore", "no"),
      ncpus = BOOTSTRAP_CONFIG$n_cores
    )
    
    # Get confidence interval
    ci <- boot.ci(boot_result, conf = BOOTSTRAP_CONFIG$conf_level, type = "perc")
    
    results[[i]] <- data.frame(
      interval = depth_intervals$interval[i],
      depth_mid = (depth_intervals$depth_top[i] + depth_intervals$depth_bottom[i]) / 2,
      n_samples = nrow(depth_data),
      mean_soc = boot_result$t0,
      lower_ci = ci$percent[4],
      upper_ci = ci$percent[5],
      se = sd(boot_result$t)
    )
  }
  
  bind_rows(results)
}

# Bootstrap depth profiles for each dataset
cat("HR Cores depth profile:\n")
hr_depth_profile <- bootstrap_depth_profile(hr_cores, depth_intervals)
hr_depth_profile$dataset <- "HR Cores"

cat("\nComposite Cores depth profile:\n")
comp_depth_profile <- bootstrap_depth_profile(composite_cores, depth_intervals)
comp_depth_profile$dataset <- "Composite Cores"

# Combine results
depth_profile_results <- bind_rows(hr_depth_profile, comp_depth_profile)

# Print results
cat("\nDepth Profile Bootstrap Results:\n")
cat("=================================\n\n")
print(kable(depth_profile_results, digits = 2, format = "simple"))

# Save results
saveRDS(depth_profile_results, "outputs/uncertainty/depth_profile_bootstrap.rds")

# ============================================================================
# 7. BOOTSTRAP: COMPARISONS BETWEEN SAMPLING METHODS
# ============================================================================

cat("\n========================================\n")
cat("SECTION 3: Method Comparisons\n")
cat("========================================\n\n")

log_message("Bootstrapping method comparisons")

# Function for two-sample bootstrap
bootstrap_comparison <- function(data1, data2, n_boot = BOOTSTRAP_CONFIG$n_boot) {
  
  # Simple difference in means
  boot_result <- boot(
    data = list(data1 = data1, data2 = data2),
    statistic = function(data, indices) {
      mean(data$data1[indices, ]$soc, na.rm = TRUE) - 
        mean(data$data2[indices, ]$soc, na.rm = TRUE)
    },
    R = n_boot,
    parallel = ifelse(BOOTSTRAP_CONFIG$parallel, "multicore", "no"),
    ncpus = BOOTSTRAP_CONFIG$n_cores
  )
  
  ci <- boot.ci(boot_result, conf = BOOTSTRAP_CONFIG$conf_level, type = "perc")
  
  list(
    difference = boot_result$t0,
    lower_ci = ci$percent[4],
    upper_ci = ci$percent[5],
    se = sd(boot_result$t),
    p_value = mean(boot_result$t > 0)  # Approximate p-value
  )
}

# Overall comparison
cat("Comparing HR vs Composite cores (overall)...\n")
overall_comparison <- bootstrap_comparison(hr_cores, composite_cores)

comparison_results <- data.frame(
  comparison = "HR - Composite (overall)",
  mean_difference = overall_comparison$difference,
  lower_ci = overall_comparison$lower_ci,
  upper_ci = overall_comparison$upper_ci,
  se = overall_comparison$se,
  significant = ifelse(overall_comparison$lower_ci * overall_comparison$upper_ci > 0, "Yes", "No")
)

# Depth-specific comparisons
depth_comparisons <- list()

for (i in 1:nrow(depth_intervals)) {
  cat("Comparing depth interval:", depth_intervals$interval[i], "cm\n")
  
  # Subset data
  hr_depth <- hr_cores %>%
    filter(depth_top >= depth_intervals$depth_top[i] & 
           depth_bottom <= depth_intervals$depth_bottom[i])
  
  comp_depth <- composite_cores %>%
    filter(depth_top >= depth_intervals$depth_top[i] & 
           depth_bottom <= depth_intervals$depth_bottom[i])
  
  if (nrow(hr_depth) < 10 | nrow(comp_depth) < 10) {
    warning(paste("Insufficient data for comparison at depth", depth_intervals$interval[i]))
    next
  }
  
  # Bootstrap comparison
  comp <- bootstrap_comparison(hr_depth, comp_depth)
  
  depth_comparisons[[i]] <- data.frame(
    comparison = paste("HR - Composite (", depth_intervals$interval[i], " cm)", sep = ""),
    mean_difference = comp$difference,
    lower_ci = comp$lower_ci,
    upper_ci = comp$upper_ci,
    se = comp$se,
    significant = ifelse(comp$lower_ci * comp$upper_ci > 0, "Yes", "No")
  )
}

comparison_results <- bind_rows(comparison_results, bind_rows(depth_comparisons))

# Print results
cat("\nMethod Comparison Bootstrap Results:\n")
cat("=====================================\n\n")
print(kable(comparison_results, digits = 2, format = "simple"))
cat("\nNote: 'Significant' indicates 95% CI does not include zero\n\n")

# Save results
saveRDS(comparison_results, "outputs/uncertainty/method_comparison_bootstrap.rds")

# ============================================================================
# 8. BOOTSTRAP: SPATIAL PREDICTIONS (if spatial models exist)
# ============================================================================

if (file.exists("outputs/kriging_predictions.rds")) {
  
  cat("\n========================================\n")
  cat("SECTION 4: Spatial Prediction Uncertainty\n")
  cat("========================================\n\n")
  
  log_message("Bootstrapping spatial predictions")
  
  # Load spatial predictions
  krig_pred <- readRDS("outputs/kriging_predictions.rds")
  
  # Bootstrap spatial means
  cat("Bootstrapping spatial mean predictions...\n")
  
  spatial_boot <- boot(
    data = data.frame(prediction = krig_pred$var1.pred),
    statistic = function(d, indices) mean(d[indices, ]$prediction, na.rm = TRUE),
    R = BOOTSTRAP_CONFIG$n_boot,
    parallel = ifelse(BOOTSTRAP_CONFIG$parallel, "multicore", "no"),
    ncpus = BOOTSTRAP_CONFIG$n_cores
  )
  
  spatial_ci <- boot.ci(spatial_boot, conf = BOOTSTRAP_CONFIG$conf_level, type = "perc")
  
  spatial_results <- data.frame(
    metric = "Spatial Mean SOC",
    estimate = spatial_boot$t0,
    lower_ci = spatial_ci$percent[4],
    upper_ci = spatial_ci$percent[5],
    se = sd(spatial_boot$t)
  )
  
  cat("\nSpatial Prediction Uncertainty:\n")
  cat("================================\n\n")
  print(kable(spatial_results, digits = 2, format = "simple"))
  
  saveRDS(spatial_results, "outputs/uncertainty/spatial_prediction_bootstrap.rds")
  
} else {
  cat("\nSpatial predictions not found. Skipping spatial uncertainty analysis.\n")
  cat("Run Module 04 first if spatial analysis is needed.\n\n")
}

# ============================================================================
# 9. BOOTSTRAP: BULK DENSITY ESTIMATES
# ============================================================================

cat("\n========================================\n")
cat("SECTION 5: Bulk Density Uncertainty\n")
cat("========================================\n\n")

log_message("Bootstrapping bulk density estimates")

# Check if bulk density data exists
if ("bd" %in% colnames(combined_cores)) {
  
  # Overall bulk density
  cat("Bootstrapping bulk density statistics...\n")
  
  bd_boot <- boot(
    data = combined_cores %>% filter(!is.na(bd)),
    statistic = function(d, indices) {
      c(
        mean = mean(d[indices, ]$bd, na.rm = TRUE),
        median = median(d[indices, ]$bd, na.rm = TRUE),
        sd = sd(d[indices, ]$bd, na.rm = TRUE)
      )
    },
    R = BOOTSTRAP_CONFIG$n_boot,
    parallel = ifelse(BOOTSTRAP_CONFIG$parallel, "multicore", "no"),
    ncpus = BOOTSTRAP_CONFIG$n_cores
  )
  
  bd_results <- lapply(1:3, function(i) {
    ci <- boot.ci(bd_boot, conf = BOOTSTRAP_CONFIG$conf_level, type = "perc", index = i)
    data.frame(
      statistic = c("mean", "median", "sd")[i],
      estimate = bd_boot$t0[i],
      lower_ci = ci$percent[4],
      upper_ci = ci$percent[5],
      se = sd(bd_boot$t[, i])
    )
  }) %>% bind_rows()
  
  cat("\nBulk Density Bootstrap Results:\n")
  cat("================================\n\n")
  print(kable(bd_results, digits = 3, format = "simple"))
  
  saveRDS(bd_results, "outputs/uncertainty/bulk_density_bootstrap.rds")
  
} else {
  cat("Bulk density data not available. Skipping BD uncertainty analysis.\n\n")
}

# ============================================================================
# 10. VISUALIZATIONS
# ============================================================================

cat("\n========================================\n")
cat("SECTION 6: Creating Visualizations\n")
cat("========================================\n\n")

log_message("Creating uncertainty visualization plots")

# Theme for plots
uncertainty_theme <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    axis.title = element_text(size = 11),
    legend.position = "bottom"
  )

# Plot 1: Bootstrap distributions for mean SOC
cat("Creating bootstrap distribution plots...\n")

boot_dist_plot <- ggplot() +
  geom_density(aes(x = hr_boot$t[, 1], fill = "HR Cores"), alpha = 0.5) +
  geom_density(aes(x = comp_boot$t[, 1], fill = "Composite Cores"), alpha = 0.5) +
  geom_vline(xintercept = hr_boot$t0[1], linetype = "dashed", color = "#F8766D") +
  geom_vline(xintercept = comp_boot$t0[1], linetype = "dashed", color = "#00BFC4") +
  scale_fill_manual(values = c("HR Cores" = "#F8766D", "Composite Cores" = "#00BFC4")) +
  labs(
    title = "Bootstrap Distribution of Mean SOC",
    subtitle = paste0(BOOTSTRAP_CONFIG$n_boot, " bootstrap iterations"),
    x = "Mean SOC (g/kg)",
    y = "Density",
    fill = "Dataset"
  ) +
  uncertainty_theme

ggsave("outputs/uncertainty/plots/bootstrap_distributions.png", 
       boot_dist_plot, width = 10, height = 6, dpi = 300)

# Plot 2: Depth profiles with confidence bands
cat("Creating depth profile uncertainty plot...\n")

depth_profile_plot <- ggplot(depth_profile_results, aes(x = depth_mid, y = mean_soc, color = dataset)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = dataset), 
              alpha = 0.2, color = NA) +
  scale_y_continuous(trans = "log10") +
  scale_x_reverse() +
  coord_flip() +
  labs(
    title = "SOC Depth Profiles with 95% Confidence Intervals",
    subtitle = "Bootstrap confidence bands show uncertainty in depth trends",
    x = "Depth (cm)",
    y = "SOC (g/kg, log scale)",
    color = "Dataset",
    fill = "Dataset"
  ) +
  uncertainty_theme

ggsave("outputs/uncertainty/plots/depth_profile_uncertainty.png", 
       depth_profile_plot, width = 10, height = 8, dpi = 300)

# Plot 3: Comparison forest plot
cat("Creating comparison forest plot...\n")

forest_plot <- ggplot(comparison_results, aes(x = mean_difference, y = comparison)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2, size = 1) +
  geom_point(aes(color = significant), size = 4) +
  scale_color_manual(values = c("Yes" = "#D55E00", "No" = "#0072B2")) +
  labs(
    title = "HR vs Composite Core Comparisons",
    subtitle = "Mean differences with 95% confidence intervals",
    x = "Difference in Mean SOC (HR - Composite, g/kg)",
    y = "",
    color = "Significant\n(CI excludes 0)"
  ) +
  uncertainty_theme +
  theme(axis.text.y = element_text(size = 10))

ggsave("outputs/uncertainty/plots/method_comparison_forest.png", 
       forest_plot, width = 10, height = 6, dpi = 300)

# Plot 4: Coefficient of Variation uncertainty
cat("Creating CV uncertainty plot...\n")

cv_data <- basic_results$summary %>%
  filter(statistic == "cv")

cv_plot <- ggplot(cv_data, aes(x = dataset, y = estimate)) +
  geom_col(aes(fill = dataset), alpha = 0.7, width = 0.6) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, size = 1) +
  geom_text(aes(label = sprintf("%.1f%%\n(%.1f-%.1f)", estimate, lower_ci, upper_ci)),
            vjust = -0.5, size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Coefficient of Variation with Confidence Intervals",
    subtitle = "Higher CV indicates greater variability in SOC measurements",
    x = "",
    y = "CV (%)",
    fill = "Dataset"
  ) +
  uncertainty_theme +
  theme(legend.position = "none") +
  ylim(0, max(cv_data$upper_ci) * 1.15)

ggsave("outputs/uncertainty/plots/cv_uncertainty.png", 
       cv_plot, width = 8, height = 6, dpi = 300)

log_message("All uncertainty plots created")

# ============================================================================
# 11. COMPREHENSIVE SUMMARY REPORT
# ============================================================================

cat("\n========================================\n")
cat("SECTION 7: Creating Summary Report\n")
cat("========================================\n\n")

# Create comprehensive summary
uncertainty_summary <- list(
  config = BOOTSTRAP_CONFIG,
  timestamp = Sys.time(),
  basic_statistics = basic_results$summary,
  depth_profiles = depth_profile_results,
  method_comparisons = comparison_results
)

# Add conditional elements
if (exists("bd_results")) {
  uncertainty_summary$bulk_density = bd_results
}

if (exists("spatial_results")) {
  uncertainty_summary$spatial_predictions = spatial_results
}

# Save comprehensive summary
saveRDS(uncertainty_summary, "outputs/uncertainty/comprehensive_uncertainty_summary.rds")

# Create human-readable text report
report_file <- "outputs/uncertainty/uncertainty_report.txt"

sink(report_file)
cat("========================================\n")
cat("UNCERTAINTY QUANTIFICATION REPORT\n")
cat("========================================\n\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("CONFIGURATION\n")
cat("-------------\n")
cat("Bootstrap Iterations:", BOOTSTRAP_CONFIG$n_boot, "\n")
cat("Confidence Level:", BOOTSTRAP_CONFIG$conf_level * 100, "%\n")
cat("Random Seed:", BOOTSTRAP_CONFIG$seed, "\n\n")

cat("DATA SUMMARY\n")
cat("------------\n")
cat("HR Cores:", nrow(hr_cores), "observations\n")
cat("Composite Cores:", nrow(composite_cores), "observations\n")
cat("Total Combined:", nrow(combined_cores), "observations\n\n")

cat("KEY FINDINGS\n")
cat("============\n\n")

cat("1. BASIC STATISTICS (with 95% CI)\n")
cat("-----------------------------------\n")
for (ds in unique(basic_results$summary$dataset)) {
  cat("\n", ds, ":\n", sep = "")
  subset_data <- basic_results$summary %>% filter(dataset == ds)
  for (i in 1:nrow(subset_data)) {
    row <- subset_data[i, ]
    cat(sprintf("  %s: %.2f (%.2f - %.2f)\n", 
                row$statistic, row$estimate, row$lower_ci, row$upper_ci))
  }
}

cat("\n\n2. METHOD COMPARISON\n")
cat("--------------------\n")
for (i in 1:nrow(comparison_results)) {
  row <- comparison_results[i, ]
  cat(sprintf("\n%s:\n", row$comparison))
  cat(sprintf("  Difference: %.2f g/kg (%.2f to %.2f)\n", 
              row$mean_difference, row$lower_ci, row$upper_ci))
  cat(sprintf("  Statistically significant: %s\n", row$significant))
}

cat("\n\n3. INTERPRETATION GUIDANCE\n")
cat("---------------------------\n")
cat("• Confidence intervals show the range of plausible values\n")
cat("• Wider CIs indicate greater uncertainty\n")
cat("• If CI includes zero, difference is not statistically significant\n")
cat("• Bootstrap provides robust estimates without assuming normality\n")
cat("• Results are based on", BOOTSTRAP_CONFIG$n_boot, "resampling iterations\n\n")

cat("RECOMMENDATIONS FOR REPORTING\n")
cat("==============================\n\n")
cat("1. Always report point estimates WITH confidence intervals\n")
cat("   Example: 'Mean SOC was 45.2 g/kg (95% CI: 42.1-48.3)'\n\n")
cat("2. Acknowledge uncertainty explicitly:\n")
cat("   'Bootstrap confidence intervals account for sampling variability'\n\n")
cat("3. Compare uncertainty across methods:\n")
cat("   'HR cores showed", 
    round(cv_data$estimate[cv_data$dataset == "HR Cores"], 1),
    "% CV while composites showed",
    round(cv_data$estimate[cv_data$dataset == "Composite Cores"], 1), "% CV'\n\n")
cat("4. Use plots to visualize uncertainty for stakeholders\n\n")
cat("5. If CIs are wide, consider:\n")
cat("   - Increasing sample size\n")
cat("   - Stratifying sampling design\n")
cat("   - Collecting auxiliary covariates\n\n")

sink()

cat("Summary report saved to:", report_file, "\n\n")
log_message("Summary report created")

# ============================================================================
# 12. CLEANUP AND FINALIZATION
# ============================================================================

# Stop parallel cluster
if (BOOTSTRAP_CONFIG$parallel && exists("cl")) {
  stopCluster(cl)
  log_message("Parallel cluster stopped")
}

# Clean up large objects
rm(hr_boot, comp_boot, combined_boot)
if (exists("bd_boot")) rm(bd_boot)
if (exists("spatial_boot")) rm(spatial_boot)
gc()

# Final summary
cat("\n========================================\n")
cat("MODULE 05 COMPLETE\n")
cat("========================================\n\n")

cat("Uncertainty Quantification Summary:\n")
cat("-----------------------------------\n")
cat("✓ Bootstrap iterations:", BOOTSTRAP_CONFIG$n_boot, "\n")
cat("✓ Confidence level:", BOOTSTRAP_CONFIG$conf_level * 100, "%\n")
cat("✓ Basic statistics uncertainty quantified\n")
cat("✓ Depth profile uncertainty quantified\n")
cat("✓ Method comparisons completed\n")
if (exists("bd_results")) cat("✓ Bulk density uncertainty quantified\n")
if (exists("spatial_results")) cat("✓ Spatial prediction uncertainty quantified\n")
cat("✓ 4 visualization plots created\n")
cat("✓ Comprehensive report generated\n\n")

cat("Output Files:\n")
cat("-------------\n")
cat("• outputs/uncertainty/comprehensive_uncertainty_summary.rds\n")
cat("• outputs/uncertainty/uncertainty_report.txt\n")
cat("• outputs/uncertainty/basic_statistics_bootstrap.rds\n")
cat("• outputs/uncertainty/depth_profile_bootstrap.rds\n")
cat("• outputs/uncertainty/method_comparison_bootstrap.rds\n")
cat("• outputs/uncertainty/plots/*.png (4 plots)\n")
cat("• Log:", log_file, "\n\n")

cat("Key Insights for Stakeholders:\n")
cat("-------------------------------\n")

# Calculate overall precision
overall_precision <- basic_results$summary %>%
  filter(statistic == "mean" & dataset == "Combined") %>%
  mutate(
    ci_width = upper_ci - lower_ci,
    relative_precision = (ci_width / estimate) * 100
  )

cat("• Overall SOC estimate:", 
    sprintf("%.1f g/kg (95%% CI: %.1f-%.1f)\n", 
            overall_precision$estimate,
            overall_precision$lower_ci,
            overall_precision$upper_ci))
cat("• Relative uncertainty:", 
    sprintf("±%.1f%% of estimate\n", overall_precision$relative_precision / 2))

# Check if methods differ significantly
overall_comp <- comparison_results[1, ]
cat("• HR vs Composite difference:", 
    sprintf("%.1f g/kg (%s significant)\n",
            overall_comp$mean_difference,
            tolower(overall_comp$significant)))

cat("\nNext Steps:\n")
cat("-----------\n")
cat("1. Review uncertainty_report.txt for detailed findings\n")
cat("2. Use plots in presentations to show confidence in results\n")
cat("3. Consider if uncertainty is acceptable for your application\n")
cat("4. If needed, increase sampling to reduce uncertainty\n")
cat("5. Integrate uncertainty into Module 03 depth models (optional)\n\n")

log_message("Module 05 completed successfully")

cat("========================================\n\n")
