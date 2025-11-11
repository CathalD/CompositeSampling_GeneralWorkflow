# ============================================================================
# SYNTHETIC TEST DATA GENERATOR - Module 04b Testing
# ============================================================================
# Purpose: Generate realistic synthetic soil core data for testing Module 04b
# ============================================================================

library(dplyr)

set.seed(42)

# ============================================================================
# GENERATE SYNTHETIC HR CORES (High-Resolution, 2cm increments)
# ============================================================================

generate_hr_core <- function(core_id, lat, lon, 
                             surface_soc = 80, decay_rate = 0.02, noise_sd = 5) {
  # Exponential decay model: SOC = a * exp(-b * depth) + c + noise
  
  depths_top <- seq(0, 98, by = 2)  # 0-2, 2-4, ..., 98-100
  depths_bottom <- seq(2, 100, by = 2)
  
  depth_mid <- (depths_top + depths_bottom) / 2
  
  # True SOC with exponential decay
  soc_true <- surface_soc * exp(-decay_rate * depth_mid) + 10
  
  # Add realistic noise
  soc_measured <- soc_true + rnorm(length(soc_true), mean = 0, sd = noise_sd)
  soc_measured <- pmax(soc_measured, 5)  # Minimum 5 g/kg
  
  # Realistic bulk density (increases with depth)
  bulk_density <- 0.8 + 0.01 * depth_mid + rnorm(length(depth_mid), 0, 0.05)
  bulk_density <- pmax(0.5, pmin(bulk_density, 1.8))  # Constrain to realistic range
  
  data.frame(
    core_id = core_id,
    latitude = lat,
    longitude = lon,
    depth_top = depths_top,
    depth_bottom = depths_bottom,
    soc = soc_measured,
    bulk_density = bulk_density
  )
}

# Generate 20 HR cores
hr_cores <- do.call(rbind, lapply(1:20, function(i) {
  generate_hr_core(
    core_id = sprintf("HR_%03d", i),
    lat = 45 + rnorm(1, 0, 0.5),
    lon = -75 + rnorm(1, 0, 0.5),
    surface_soc = runif(1, 60, 100),
    decay_rate = runif(1, 0.015, 0.025),
    noise_sd = runif(1, 3, 7)
  )
}))

cat("Generated HR cores:", length(unique(hr_cores$core_id)), "cores,", nrow(hr_cores), "samples\n")

# ============================================================================
# GENERATE SYNTHETIC COMPOSITE CORES (20cm increments)
# ============================================================================

generate_composite_core <- function(core_id, lat, lon,
                                    surface_soc = 70, decay_rate = 0.02, noise_sd = 8) {
  
  depths_top <- c(0, 20, 40, 60, 80)
  depths_bottom <- c(20, 40, 60, 80, 100)
  
  depth_mid <- (depths_top + depths_bottom) / 2
  
  # True SOC with exponential decay
  soc_true <- surface_soc * exp(-decay_rate * depth_mid) + 10
  
  # Add realistic noise (slightly more than HR due to compositing)
  soc_measured <- soc_true + rnorm(length(soc_true), mean = 0, sd = noise_sd)
  soc_measured <- pmax(soc_measured, 5)
  
  # Realistic bulk density
  bulk_density <- 0.8 + 0.01 * depth_mid + rnorm(length(depth_mid), 0, 0.08)
  bulk_density <- pmax(0.5, pmin(bulk_density, 1.8))
  
  data.frame(
    core_id = core_id,
    latitude = lat,
    longitude = lon,
    depth_top = depths_top,
    depth_bottom = depths_bottom,
    soc = soc_measured,
    bulk_density = bulk_density
  )
}

# Generate 15 composite cores
composite_cores <- do.call(rbind, lapply(1:15, function(i) {
  generate_composite_core(
    core_id = sprintf("COMP_%03d", i),
    lat = 45 + rnorm(1, 0, 0.5),
    lon = -75 + rnorm(1, 0, 0.5),
    surface_soc = runif(1, 55, 95),
    decay_rate = runif(1, 0.015, 0.025),
    noise_sd = runif(1, 5, 10)
  )
}))

cat("Generated composite cores:", length(unique(composite_cores$core_id)), "cores,", nrow(composite_cores), "samples\n")

# ============================================================================
# SAVE SYNTHETIC DATA
# ============================================================================

# Create directories
if (!dir.exists("data_processed")) dir.create("data_processed", recursive = TRUE)
if (!dir.exists("test_data")) dir.create("test_data", recursive = TRUE)

# Save for Module 04b testing
saveRDS(hr_cores, "data_processed/hr_cores_clean.rds")
saveRDS(composite_cores, "data_processed/composite_cores_clean.rds")

# Also save as CSV for inspection
write.csv(hr_cores, "test_data/synthetic_hr_cores.csv", row.names = FALSE)
write.csv(composite_cores, "test_data/synthetic_composite_cores.csv", row.names = FALSE)

cat("\n========================================\n")
cat("SYNTHETIC DATA GENERATED SUCCESSFULLY\n")
cat("========================================\n\n")

cat("Files created:\n")
cat("• data_processed/hr_cores_clean.rds (for Module 04b)\n")
cat("• data_processed/composite_cores_clean.rds (for Module 04b)\n")
cat("• test_data/synthetic_hr_cores.csv (for inspection)\n")
cat("• test_data/synthetic_composite_cores.csv (for inspection)\n\n")

cat("Data Summary:\n")
cat("=============\n")
cat("HR Cores:\n")
cat("  - Number of cores:", length(unique(hr_cores$core_id)), "\n")
cat("  - Samples per core:", nrow(hr_cores) / length(unique(hr_cores$core_id)), "\n")
cat("  - Depth increment: 2 cm\n")
cat("  - Depth range: 0-100 cm\n")
cat("  - SOC range:", round(min(hr_cores$soc), 1), "-", round(max(hr_cores$soc), 1), "g/kg\n\n")

cat("Composite Cores:\n")
cat("  - Number of cores:", length(unique(composite_cores$core_id)), "\n")
cat("  - Samples per core:", nrow(composite_cores) / length(unique(composite_cores$core_id)), "\n")
cat("  - Depth increment: 20 cm\n")
cat("  - Depth range: 0-100 cm\n")
cat("  - SOC range:", round(min(composite_cores$soc), 1), "-", round(max(composite_cores$soc), 1), "g/kg\n\n")

cat("Synthetic data characteristics:\n")
cat("• Exponential decay with depth (realistic SOC pattern)\n")
cat("• Random noise added to simulate measurement error\n")
cat("• Bulk density increases with depth\n")
cat("• Spatially distributed coordinates\n")
cat("• Ready for Module 04b testing\n\n")

cat("Next Step: Run Module 04b ENHANCED\n")
cat("===================================\n")
cat("source('04b_depth_harmonization_splines_ENHANCED.R')\n\n")

# Create a quick visualization
if (require(ggplot2, quietly = TRUE)) {
  
  # Sample a few cores for visualization
  sample_hr <- hr_cores %>% filter(core_id %in% paste0("HR_", sprintf("%03d", 1:3)))
  sample_comp <- composite_cores %>% filter(core_id %in% paste0("COMP_", sprintf("%03d", 1:3)))
  
  sample_hr$depth_mid <- (sample_hr$depth_top + sample_hr$depth_bottom) / 2
  sample_comp$depth_mid <- (sample_comp$depth_top + sample_comp$depth_bottom) / 2
  
  p <- ggplot() +
    geom_point(data = sample_hr, aes(x = soc, y = -depth_mid, color = "HR (2cm)"),
               size = 2, alpha = 0.6) +
    geom_line(data = sample_hr, aes(x = soc, y = -depth_mid, group = core_id, color = "HR (2cm)"),
              alpha = 0.4) +
    geom_point(data = sample_comp, aes(x = soc, y = -depth_mid, color = "Composite (20cm)"),
               size = 3, alpha = 0.8, shape = 17) +
    geom_line(data = sample_comp, aes(x = soc, y = -depth_mid, group = core_id, color = "Composite (20cm)"),
              alpha = 0.6, linetype = "dashed") +
    scale_color_manual(values = c("HR (2cm)" = "#2E86AB", "Composite (20cm)" = "#A23B72")) +
    labs(
      title = "Synthetic Test Data - Sample Cores",
      subtitle = "Showing 3 HR cores (2cm) and 3 Composite cores (20cm)",
      x = "SOC (g/kg)",
      y = "Depth (cm)",
      color = "Core Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom"
    )
  
  if (!dir.exists("test_data/plots")) dir.create("test_data/plots", recursive = TRUE)
  
  ggsave("test_data/plots/synthetic_data_preview.png", p, 
         width = 8, height = 6, dpi = 300)
  
  cat("Preview plot saved: test_data/plots/synthetic_data_preview.png\n\n")
}

cat("✓ Test data ready!\n")
