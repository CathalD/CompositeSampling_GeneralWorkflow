# ============================================================================
# Module 02: Exploratory Data Analysis - IMPROVED VERSION
# VERSION: 2.0 - Production-ready with error handling and enhanced visualizations
# ============================================================================
# Purpose: Visualize and explore patterns in soil core data
# Inputs: Processed data from Module 01
# Outputs: Plots and exploratory statistics
# Author: [Your name]
# Date: Modified on 2024
# ============================================================================

# ============================================================================
# SETUP AND CONFIGURATION
# ============================================================================

# Initialize logging
log_file <- file.path("logs", paste0("exploratory_analysis_", Sys.Date(), ".log"))
if (!dir.exists("logs")) dir.create("logs")

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

log_message("Starting Module 02: Exploratory Data Analysis")

# ============================================================================
# LOAD PACKAGES WITH ERROR HANDLING
# ============================================================================

load_required_package <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required. Install with: install.packages('%s')", pkg, pkg))
  }
  log_message(sprintf("Loaded package: %s", pkg))
}

# Load required packages
required_packages <- c("dplyr", "ggplot2", "tidyr", "gridExtra", "corrplot", "sf")

for (pkg in required_packages) {
  tryCatch({
    load_required_package(pkg)
  }, error = function(e) {
    log_message(sprintf("Failed to load %s: %s", pkg, e$message), "ERROR")
    if (pkg %in% c("dplyr", "ggplot2")) {
      stop("Cannot proceed without essential packages")
    } else {
      log_message(sprintf("Continuing without %s - some features may be unavailable", pkg), "WARNING")
    }
  })
}

# After loading all packages, add these lines:
select <- dplyr::select
filter <- dplyr::filter

# Source helper functions
tryCatch({
  if (file.exists("helper_functions_improved.R")) {
    source("helper_functions_improved.R")
  } else if (file.exists("helper_functions.R")) {
    source("helper_functions.R")
    log_message("Using original helper_functions.R", "WARNING")
  } else {
    stop("Helper functions file not found")
  }
}, error = function(e) {
  log_message(sprintf("Error loading helper functions: %s", e$message), "ERROR")
  stop("Cannot proceed without helper functions")
})

# ============================================================================
# 1. LOAD PROCESSED DATA WITH VALIDATION
# ============================================================================

log_message("Loading processed data")

# Function to safely load RDS files
load_rds_safe <- function(filename, required = TRUE) {
  filepath <- file.path("data_processed", filename)
  if (!file.exists(filepath)) {
    if (required) {
      stop(sprintf("Required file not found: %s", filepath))
    } else {
      log_message(sprintf("Optional file not found: %s", filepath), "WARNING")
      return(NULL)
    }
  }
  
  tryCatch({
    data <- readRDS(filepath)
    log_message(sprintf("Loaded %s successfully", filename))
    return(data)
  }, error = function(e) {
    log_message(sprintf("Error loading %s: %s", filename, e$message), "ERROR")
    if (required) {
      stop(sprintf("Failed to load required file: %s", filename))
    }
    return(NULL)
  })
}

# Load datasets
hr_cores <- load_rds_safe("hr_cores_clean.rds")
composite_cores <- load_rds_safe("composite_cores_clean.rds")
combined_cores <- load_rds_safe("combined_cores.rds")

# Validate loaded data
if (nrow(hr_cores) == 0 || nrow(composite_cores) == 0 || nrow(combined_cores) == 0) {
  stop("Loaded data is empty. Please run Module 01 first.")
}

cat("Data loaded successfully!\n")
cat("High-res cores:", nrow(hr_cores), "samples\n")
cat("Composite cores:", nrow(composite_cores), "samples\n")
cat("Combined dataset:", nrow(combined_cores), "samples\n\n")

# ============================================================================
# 2. SET VISUALIZATION PARAMETERS
# ============================================================================

# Create custom theme for consistency
theme_soil <- function() {
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, size = 0.5)
  )
}

# Color palette for core types
core_colors <- c("high_res" = "#2E86AB", "composite" = "#A23B72", "unknown" = "#888888")

# ============================================================================
# 3. DEPTH PROFILE VISUALIZATIONS
# ============================================================================

log_message("Creating depth profile visualizations")

# Create output directory if it doesn't exist
if (!dir.exists("outputs/plots")) {
  dir.create("outputs/plots", recursive = TRUE)
}

# 3.1 Individual core profiles with error handling
create_individual_profiles <- function(data, data_name, variable = "soc", max_plots = 6) {
  
  log_message(sprintf("Creating individual %s profiles for %s", variable, data_name))
  
  # Get unique core IDs (up to max_plots)
  core_ids <- unique(data$core_id)[1:min(max_plots, length(unique(data$core_id)))]
  
  # Create plots with error handling
  plots <- list()
  for (i in seq_along(core_ids)) {
    tryCatch({
      p <- plot_depth_profile(data, core_ids[i], variable) +
        theme_soil() +
        labs(subtitle = data_name)
      plots[[i]] <- p
    }, error = function(e) {
      log_message(sprintf("Failed to create profile for core %s: %s", 
                         core_ids[i], e$message), "WARNING")
      # Create placeholder plot
      plots[[i]] <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                label = paste("No data for", core_ids[i])) +
        theme_void()
    })
  }
  
  return(plots)
}

# Create SOC profiles for high-resolution cores
hr_soc_plots <- create_individual_profiles(hr_cores, "High-Resolution", "soc")

# Save plots with error handling
tryCatch({
  pdf("outputs/plots/hr_soc_profiles.pdf", width = 12, height = 8)
  do.call(grid.arrange, c(hr_soc_plots, ncol = 3))
  dev.off()
  log_message("Saved high-resolution SOC profiles")
}, error = function(e) {
  log_message(sprintf("Failed to save HR SOC profiles: %s", e$message), "WARNING")
})

# Create SOC profiles for composite cores
comp_soc_plots <- create_individual_profiles(composite_cores, "Composite", "soc")

tryCatch({
  pdf("outputs/plots/composite_soc_profiles.pdf", width = 12, height = 8)
  do.call(grid.arrange, c(comp_soc_plots, ncol = 3))
  dev.off()
  log_message("Saved composite SOC profiles")
}, error = function(e) {
  log_message(sprintf("Failed to save composite SOC profiles: %s", e$message), "WARNING")
})

# 3.2 Mean depth profiles by core type
log_message("Creating mean depth profile comparisons")

# Calculate mean profiles with confidence intervals
mean_profiles <- combined_cores %>%
  group_by(core_type, depth_mid) %>%
  summarise(
    n = n(),
    soc_mean = mean(soc, na.rm = TRUE),
    soc_se = sd(soc, na.rm = TRUE) / sqrt(n()),
    soc_ci_lower = soc_mean - qt(0.975, n-1) * soc_se,
    soc_ci_upper = soc_mean + qt(0.975, n-1) * soc_se,
    bd_mean = mean(bulk_density, na.rm = TRUE),
    bd_se = sd(bulk_density, na.rm = TRUE) / sqrt(n()),
    bd_ci_lower = bd_mean - qt(0.975, n-1) * bd_se,
    bd_ci_upper = bd_mean + qt(0.975, n-1) * bd_se,
    .groups = "drop"
  ) %>%
  filter(!is.na(soc_mean))  # Remove any NA groups

# SOC depth profile comparison with confidence intervals
p_soc_depth <- ggplot(mean_profiles, 
                      aes(x = soc_mean, y = -depth_mid, 
                          color = core_type, fill = core_type)) +
  geom_ribbon(aes(xmin = soc_ci_lower, xmax = soc_ci_upper),
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = core_colors,
                     labels = c("High-Resolution", "Composite", "Unknown")) +
  scale_fill_manual(values = core_colors,
                    labels = c("High-Resolution", "Composite", "Unknown")) +
  labs(
    title = "Mean SOC Depth Profiles",
    subtitle = "Shaded area shows 95% confidence interval",
    x = "SOC (g/kg)",
    y = "Depth (cm)",
    color = "Core Type",
    fill = "Core Type"
  ) +
  theme_soil()

# Save SOC profile comparison
ggsave("outputs/plots/soc_depth_profile_comparison.png", p_soc_depth,
       width = 8, height = 6, dpi = 300)
log_message("Saved SOC depth profile comparison")

# Bulk density depth profile comparison
p_bd_depth <- ggplot(mean_profiles,
                     aes(x = bd_mean, y = -depth_mid,
                         color = core_type, fill = core_type)) +
  geom_ribbon(aes(xmin = bd_ci_lower, xmax = bd_ci_upper),
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = core_colors,
                     labels = c("High-Resolution", "Composite", "Unknown")) +
  scale_fill_manual(values = core_colors,
                    labels = c("High-Resolution", "Composite", "Unknown")) +
  labs(
    title = "Mean Bulk Density Depth Profiles",
    subtitle = "Shaded area shows 95% confidence interval",
    x = expression(paste("Bulk Density (g cm"^-3, ")")),
    y = "Depth (cm)",
    color = "Core Type",
    fill = "Core Type"
  ) +
  theme_soil()

# Save BD profile comparison
ggsave("outputs/plots/bd_depth_profile_comparison.png", p_bd_depth,
       width = 8, height = 6, dpi = 300)
log_message("Saved bulk density depth profile comparison")

# ============================================================================
# 4. CUMULATIVE SOC STOCK PROFILES
# ============================================================================

log_message("Creating cumulative stock profiles")

# Calculate cumulative profiles with error handling
tryCatch({
  cumulative_profiles <- combined_cores %>%
    group_by(core_type, core_id) %>%
    arrange(depth_top) %>%
    mutate(cumulative_stock = cumsum(soc_stock)) %>%
    group_by(core_type, depth_bottom) %>%
    summarise(
      n = n(),
      cum_stock_mean = mean(cumulative_stock, na.rm = TRUE),
      cum_stock_se = sd(cumulative_stock, na.rm = TRUE) / sqrt(n()),
      cum_stock_ci_lower = cum_stock_mean - qt(0.975, n-1) * cum_stock_se,
      cum_stock_ci_upper = cum_stock_mean + qt(0.975, n-1) * cum_stock_se,
      .groups = "drop"
    ) %>%
    filter(!is.na(cum_stock_mean))
  
  # Plot cumulative stocks
  p_cumulative <- ggplot(cumulative_profiles, 
                        aes(x = cum_stock_mean, y = -depth_bottom,
                            color = core_type, fill = core_type)) +
    geom_ribbon(aes(xmin = cum_stock_ci_lower, xmax = cum_stock_ci_upper),
                alpha = 0.2, color = NA) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(values = core_colors) +
    scale_fill_manual(values = core_colors) +
    labs(
      title = "Cumulative SOC Stock Profiles",
      subtitle = "Mean cumulative stock with 95% CI",
      x = "Cumulative SOC Stock (Mg/ha)",
      y = "Depth (cm)",
      color = "Core Type",
      fill = "Core Type"
    ) +
    theme_soil()
  
  ggsave("outputs/plots/cumulative_soc_stock_profiles.png", p_cumulative,
         width = 8, height = 6, dpi = 300)
  log_message("Saved cumulative stock profiles")
  
}, error = function(e) {
  log_message(sprintf("Error creating cumulative profiles: %s", e$message), "WARNING")
})

# ============================================================================
# 5. SPATIAL DISTRIBUTION ANALYSIS
# ============================================================================

log_message("Analyzing spatial distribution")

# Aggregate data by location
spatial_summary <- combined_cores %>%
  group_by(core_id, latitude, longitude, core_type) %>%
  summarise(
    total_stock = sum(soc_stock, na.rm = TRUE),
    mean_soc = mean(soc, na.rm = TRUE),
    mean_bd = mean(bulk_density, na.rm = TRUE),
    max_depth = max(depth_bottom, na.rm = TRUE),
    .groups = "drop"
  )

# Create spatial distribution map
p_spatial <- ggplot(spatial_summary, 
                   aes(x = longitude, y = latitude, 
                       color = total_stock, shape = core_type)) +
  geom_point(size = 4, alpha = 0.7) +
  scale_color_gradient2(
    low = "blue", mid = "yellow", high = "red",
    midpoint = median(spatial_summary$total_stock, na.rm = TRUE),
    name = "Total Stock\n(Mg/ha)"
  ) +
  scale_shape_manual(values = c(16, 17, 18), name = "Core Type") +
  labs(
    title = "Spatial Distribution of SOC Stocks",
    subtitle = "Total stock (0-100 cm)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_soil() +
  coord_equal()

ggsave("outputs/plots/spatial_distribution_map.png", p_spatial,
       width = 10, height = 8, dpi = 300)
log_message("Saved spatial distribution map")

# ============================================================================
# 6. STATISTICAL DISTRIBUTIONS
# ============================================================================

log_message("Creating statistical distribution plots")

# SOC distribution by depth
p_soc_dist <- ggplot(combined_cores, 
                     aes(x = soc, fill = factor(depth_top))) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~core_type, scales = "free_y") +
  scale_fill_viridis_d(name = "Depth Top (cm)") +
  labs(
    title = "Distribution of SOC by Depth and Core Type",
    x = "SOC (g/kg)",
    y = "Count"
  ) +
  theme_soil()

ggsave("outputs/plots/soc_distribution_by_depth.png", p_soc_dist,
       width = 10, height = 6, dpi = 300)

# Box plots by depth increment
p_box <- combined_cores %>%
  mutate(depth_class = paste0(depth_top, "-", depth_bottom, " cm")) %>%
  ggplot(aes(x = depth_class, y = soc, fill = core_type)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_manual(values = core_colors) +
  labs(
    title = "SOC Distribution by Depth Increment",
    x = "Depth Range",
    y = "SOC (g/kg)",
    fill = "Core Type"
  ) +
  theme_soil() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/plots/soc_boxplots_by_depth.png", p_box,
       width = 10, height = 6, dpi = 300)
log_message("Saved distribution plots")

# ============================================================================
# 7. CORRELATION ANALYSIS
# ============================================================================

log_message("Performing correlation analysis")

# Prepare data for correlation
cor_data <- combined_cores %>%
  select(depth_mid, soc, bulk_density, soc_stock) %>%
  na.omit()

if (nrow(cor_data) > 2) {
  # Calculate correlation matrix
  cor_matrix <- cor(cor_data, use = "pairwise.complete.obs", method = "pearson")
  
  # Create correlation plot
  tryCatch({
    png("outputs/plots/correlation_matrix.png", width = 800, height = 800, res = 100)
    corrplot(cor_matrix, 
             method = "circle",
             type = "upper",
             order = "hclust",
             tl.col = "black",
             tl.srt = 45,
             addCoef.col = "black",
             title = "Correlation Matrix of Soil Variables",
             mar = c(0, 0, 2, 0))
    dev.off()
    log_message("Saved correlation matrix")
  }, error = function(e) {
    log_message(sprintf("Error creating correlation plot: %s", e$message), "WARNING")
  })
  
  # Print correlation summary
  cat("\nCorrelation Summary:\n")
  print(round(cor_matrix, 3))
}

# ============================================================================
# 8. VARIABILITY ANALYSIS
# ============================================================================

log_message("Analyzing variability patterns")

# Calculate coefficient of variation by depth
cv_by_depth <- combined_cores %>%
  group_by(core_type, depth_top, depth_bottom) %>%
  summarise(
    soc_cv = sd(soc, na.rm = TRUE) / mean(soc, na.rm = TRUE) * 100,
    bd_cv = sd(bulk_density, na.rm = TRUE) / mean(bulk_density, na.rm = TRUE) * 100,
    stock_cv = sd(soc_stock, na.rm = TRUE) / mean(soc_stock, na.rm = TRUE) * 100,
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(depth_mid = (depth_top + depth_bottom) / 2)

# Plot coefficient of variation
p_cv <- ggplot(cv_by_depth, aes(x = soc_cv, y = -depth_mid, color = core_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = core_colors) +
  labs(
    title = "Variability in SOC by Depth",
    subtitle = "Coefficient of Variation",
    x = "CV (%)",
    y = "Depth (cm)",
    color = "Core Type"
  ) +
  theme_soil()

ggsave("outputs/plots/soc_variability_by_depth.png", p_cv,
       width = 8, height = 6, dpi = 300)
log_message("Saved variability analysis")

# ============================================================================
# 9. COMPARATIVE ANALYSIS
# ============================================================================

log_message("Creating comparative visualizations")

# Compare high-res vs composite at matching depths
if (exists("hr_cores_aggregated")) {
  hr_aggregated <- load_rds_safe("hr_cores_aggregated.rds", required = FALSE)
  
  if (!is.null(hr_aggregated)) {
    # Prepare comparison data
    comparison_data <- hr_aggregated %>%
      mutate(source = "HR_aggregated") %>%
      select(depth_top, depth_bottom, soc = soc_mean, 
             bulk_density = bd_mean, source) %>%
      bind_rows(
        composite_cores %>%
          mutate(source = "Composite") %>%
          group_by(depth_top, depth_bottom) %>%
          summarise(
            soc = mean(soc, na.rm = TRUE),
            bulk_density = mean(bulk_density, na.rm = TRUE),
            source = first(source),
            .groups = "drop"
          )
      )
    
    # Create comparison plot
    p_comparison <- ggplot(comparison_data, 
                          aes(x = soc, y = -(depth_top + depth_bottom)/2,
                              color = source, shape = source)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_line(aes(group = source), alpha = 0.5) +
      scale_color_manual(values = c("HR_aggregated" = "#2E86AB", 
                                   "Composite" = "#A23B72")) +
      labs(
        title = "Comparison: Aggregated High-Res vs Composite",
        x = "SOC (g/kg)",
        y = "Depth (cm)",
        color = "Data Source",
        shape = "Data Source"
      ) +
      theme_soil()
    
    ggsave("outputs/plots/hr_vs_composite_comparison.png", p_comparison,
           width = 8, height = 6, dpi = 300)
    log_message("Saved HR vs composite comparison")
  }
}

# ============================================================================
# 10. SUMMARY STATISTICS AND REPORTS
# ============================================================================

log_message("Generating summary statistics")

# Create comprehensive summary
exploratory_summary <- list(
  timestamp = Sys.time(),
  
  overall_stats = combined_cores %>%
    summarise(
      n_cores = n_distinct(core_id),
      n_samples = n(),
      soc_mean = mean(soc, na.rm = TRUE),
      soc_sd = sd(soc, na.rm = TRUE),
      soc_min = min(soc, na.rm = TRUE),
      soc_max = max(soc, na.rm = TRUE),
      bd_mean = mean(bulk_density, na.rm = TRUE),
      bd_sd = sd(bulk_density, na.rm = TRUE),
      stock_mean = mean(soc_stock, na.rm = TRUE),
      stock_sd = sd(soc_stock, na.rm = TRUE)
    ),
  
  by_core_type = combined_cores %>%
    group_by(core_type) %>%
    summarise(
      n_cores = n_distinct(core_id),
      n_samples = n(),
      soc_mean = mean(soc, na.rm = TRUE),
      soc_sd = sd(soc, na.rm = TRUE),
      bd_mean = mean(bulk_density, na.rm = TRUE),
      bd_sd = sd(bulk_density, na.rm = TRUE),
      .groups = "drop"
    ),
  
  by_depth = combined_cores %>%
    group_by(depth_top, depth_bottom) %>%
    summarise(
      n = n(),
      soc_mean = mean(soc, na.rm = TRUE),
      soc_cv = sd(soc, na.rm = TRUE) / mean(soc, na.rm = TRUE) * 100,
      .groups = "drop"
    ),
  
  spatial_extent = list(
    lat_range = range(combined_cores$latitude, na.rm = TRUE),
    lon_range = range(combined_cores$longitude, na.rm = TRUE),
    n_locations = n_distinct(paste(combined_cores$latitude, 
                                  combined_cores$longitude))
  ),
  
  correlation_matrix = if (exists("cor_matrix")) cor_matrix else NULL
)

# Save exploratory summary
saveRDS(exploratory_summary, file.path("outputs", "exploratory_summary.rds"))
write.csv(exploratory_summary$by_core_type, 
          file.path("outputs", "summary_by_core_type.csv"),
          row.names = FALSE)
write.csv(exploratory_summary$by_depth, 
          file.path("outputs", "summary_by_depth_exploratory.csv"),
          row.names = FALSE)

# ============================================================================
# 11. CLEANUP AND FINAL REPORTING
# ============================================================================

# Clear large objects
rm(hr_soc_plots, comp_soc_plots)
gc()

log_message("Memory cleanup completed")

# Print summary
cat("\n========================================\n")
cat("MODULE 02 COMPLETE\n")
cat("========================================\n\n")

cat("Exploratory Analysis Summary:\n")
cat("------------------------------\n")
cat("✓ Depth profiles created\n")
cat("✓ Spatial distribution analyzed\n")
cat("✓ Statistical distributions plotted\n")
cat("✓ Correlation analysis completed\n")
cat("✓ Variability patterns assessed\n")
cat("✓ All plots saved to outputs/plots/\n")
cat("✓ Summary saved to outputs/\n")
cat("✓ Log saved to:", log_file, "\n\n")

cat("Key Findings:\n")
cat("- Total samples analyzed:", nrow(combined_cores), "\n")
cat("- Mean SOC:", round(mean(combined_cores$soc, na.rm = TRUE), 2), "g/kg\n")
cat("- SOC range:", round(min(combined_cores$soc, na.rm = TRUE), 2), 
    "-", round(max(combined_cores$soc, na.rm = TRUE), 2), "g/kg\n")
cat("- Spatial coverage:", 
    n_distinct(paste(combined_cores$latitude, combined_cores$longitude)), 
    "unique locations\n\n")

cat("Next steps:\n")
cat("1. Review plots in outputs/plots/\n")
cat("2. Check exploratory_summary.rds for detailed statistics\n")
cat("3. Run Module 03: source('03_depth_modeling.R')\n\n")

# Save session info
sessionInfo <- sessionInfo()
saveRDS(sessionInfo, file.path("outputs", "session_info_module02.rds"))

log_message("Module 02 completed successfully")
