# ============================================================================
# Setup Script: IMPROVED VERSION with Robust Package Management
# VERSION: 2.0 - Production-ready with error handling
# ============================================================================
# Purpose: Install required R packages and create necessary folders
# Run this script FIRST before running any analysis modules
# ============================================================================

cat("========================================\n")
cat("COMPOSITE SAMPLING ANALYSIS - SETUP v2.0\n")
cat("========================================\n\n")

# ============================================================================
# 1. CONFIGURATION
# ============================================================================

# Set options for better error handling
options(
  warn = 1,  # Print warnings as they occur
  repos = c(CRAN = "https://cloud.r-project.org/")  # Set default CRAN mirror
)

# Create log file for setup process
log_file <- file.path(getwd(), paste0("setup_log_", Sys.Date(), ".txt"))
log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

log_message("Starting setup process")

# ============================================================================
# 2. CHECK R VERSION
# ============================================================================

log_message("Checking R version...")
r_version <- R.version
r_version_string <- R.version$version.string
log_message(r_version_string)

if (as.numeric(r_version$major) < 4) {
  log_message("R version 4.0 or higher is recommended for this analysis", "WARNING")
  cat("Do you want to continue anyway? (yes/no): ")
  continue_anyway <- tolower(readline())
  if (continue_anyway != "yes" && continue_anyway != "y") {
    log_message("Setup cancelled by user", "INFO")
    stop("Please upgrade R to version 4.0 or higher")
  }
}

# ============================================================================
# 3. DEFINE REQUIRED PACKAGES
# ============================================================================

# Core packages (essential for basic functionality)
core_packages <- c(
  "dplyr",        # Data manipulation
  "tidyr",        # Data tidying
  "ggplot2"       # Visualization
)

# Analysis packages (required for full functionality)
analysis_packages <- c(
  "gridExtra",    # Multiple plots
  "corrplot"      # Correlation matrices
)

# Spatial packages (for spatial analysis)
spatial_packages <- c(
  "sf",           # Spatial features
  "raster",       # Raster operations
  "terra",        # Modern raster package
  "gstat",        # Geostatistics
  "spdep"         # Spatial dependence
)

# Modeling packages (for statistical models)
modeling_packages <- c(
  "nlme",         # Mixed-effects models
  "mgcv",         # GAMs
  "splines",      # Spline functions
  "randomForest", # Machine learning
  "caret"         # Model training framework
)

# Optional packages (for advanced features)
optional_packages <- c(
  "here",         # Project-relative paths
  "testthat",     # Unit testing
  "roxygen2",     # Documentation
  "styler",       # Code formatting
  "rmarkdown",    # Report generation
  "plotly"        # Interactive plots
)

# Combine all required packages
required_packages <- c(core_packages, analysis_packages, spatial_packages, modeling_packages)
all_packages <- c(required_packages, optional_packages)

# ============================================================================
# 4. ENHANCED PACKAGE INSTALLATION FUNCTION
# ============================================================================

install_package_safe <- function(package, required = TRUE) {
  # Check if already installed
  if (requireNamespace(package, quietly = TRUE)) {
    log_message(sprintf("Package '%s' already installed ✓", package))
    return(TRUE)
  }
  
  # Try to install
  log_message(sprintf("Installing package '%s'...", package))
  
  tryCatch({
    install.packages(package, 
                    dependencies = TRUE, 
                    quiet = TRUE,
                    repos = "https://cloud.r-project.org/")
    
    # Verify installation
    if (requireNamespace(package, quietly = TRUE)) {
      log_message(sprintf("Package '%s' installed successfully ✓", package))
      return(TRUE)
    } else {
      log_message(sprintf("Package '%s' installation verification failed", package), "ERROR")
      return(FALSE)
    }
    
  }, warning = function(w) {
    log_message(sprintf("Warning installing '%s': %s", package, w$message), "WARNING")
    return(FALSE)
    
  }, error = function(e) {
    log_message(sprintf("Error installing '%s': %s", package, e$message), "ERROR")
    if (required) {
      cat(sprintf("\n✗ Failed to install required package '%s'\n", package))
      cat("Possible solutions:\n")
      cat("1. Check your internet connection\n")
      cat("2. Try a different CRAN mirror\n")
      cat("3. Install manually: install.packages('", package, "')\n", sep = "")
      cat("4. Check if you need system dependencies (especially for spatial packages)\n\n")
    }
    return(FALSE)
  })
}

# ============================================================================
# 5. INSTALL PACKAGES
# ============================================================================

cat("\n========================================\n")
cat("INSTALLING PACKAGES\n")
cat("========================================\n\n")

# Install core packages first
cat("Installing core packages (essential)...\n")
cat("----------------------------------------\n")
core_success <- sapply(core_packages, install_package_safe, required = TRUE)

if (!all(core_success)) {
  log_message("Not all core packages installed successfully", "ERROR")
  cat("\n✗ Core package installation incomplete.\n")
  cat("Please resolve issues before continuing.\n\n")
  stop("Core package installation failed")
}

# Install analysis packages
cat("\nInstalling analysis packages...\n")
cat("----------------------------------------\n")
analysis_success <- sapply(analysis_packages, install_package_safe, required = TRUE)

# Install spatial packages (may require system libraries)
cat("\nInstalling spatial packages...\n")
cat("----------------------------------------\n")
cat("Note: Spatial packages may require system libraries:\n")
cat("  - Ubuntu/Debian: sudo apt-get install gdal-bin proj-bin libgdal-dev libproj-dev\n")
cat("  - MacOS: brew install gdal proj geos\n")
cat("  - Windows: Usually works without additional setup\n\n")

spatial_success <- sapply(spatial_packages, install_package_safe, required = FALSE)

# Install modeling packages
cat("\nInstalling modeling packages...\n")
cat("----------------------------------------\n")
modeling_success <- sapply(modeling_packages, install_package_safe, required = TRUE)

# Ask about optional packages
cat("\n========================================\n")
cat("OPTIONAL PACKAGES\n")
cat("========================================\n\n")
cat("Optional packages provide additional functionality:\n")
cat("  - here: Better path management\n")
cat("  - testthat: Unit testing framework\n")
cat("  - roxygen2: Documentation generation\n")
cat("  - styler: Automatic code formatting\n")
cat("  - rmarkdown: Report generation\n")
cat("  - plotly: Interactive visualizations\n\n")

cat("Install optional packages? (yes/no): ")
install_optional <- tolower(readline())

if (install_optional == "yes" || install_optional == "y") {
  cat("\nInstalling optional packages...\n")
  cat("----------------------------------------\n")
  optional_success <- sapply(optional_packages, install_package_safe, required = FALSE)
} else {
  log_message("Optional packages skipped by user")
}

# ============================================================================
# 6. VERIFY INSTALLATIONS
# ============================================================================

cat("\n========================================\n")
cat("VERIFYING PACKAGE INSTALLATIONS\n")
cat("========================================\n\n")

# Check which packages are available
installed_packages <- character()
missing_packages <- character()

for (pkg in required_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    installed_packages <- c(installed_packages, pkg)
    cat("✓", pkg, "\n")
  } else {
    missing_packages <- c(missing_packages, pkg)
    cat("✗", pkg, "NOT AVAILABLE\n")
  }
}

# Report summary
cat("\nInstallation Summary:\n")
cat("----------------------------------------\n")
cat("Installed:", length(installed_packages), "packages\n")
cat("Missing:", length(missing_packages), "packages\n")

if (length(missing_packages) > 0) {
  cat("\nMissing packages:\n")
  for (pkg in missing_packages) {
    cat("  -", pkg, "\n")
  }
  cat("\nYou may need to install these manually.\n")
  log_message(paste("Missing packages:", paste(missing_packages, collapse = ", ")), "WARNING")
}

# ============================================================================
# 7. CREATE DIRECTORY STRUCTURE
# ============================================================================

cat("\n========================================\n")
cat("CREATING DIRECTORY STRUCTURE\n")
cat("========================================\n\n")

directories <- list(
  data_processed = "Processed R data objects",
  outputs = "Results, tables, and figures",
  "outputs/plots" = "Visualization outputs",
  "outputs/models" = "Saved statistical models",
  "outputs/reports" = "Generated reports",
  covariates = "Covariate raster files from GEE",
  logs = "Analysis logs and metadata"
)

for (dir_name in names(directories)) {
  tryCatch({
    if (!dir.exists(dir_name)) {
      dir.create(dir_name, recursive = TRUE)
      cat("✓ Created:", dir_name, "\n")
      log_message(sprintf("Created directory: %s", dir_name))
    } else {
      cat("✓ Exists:", dir_name, "\n")
    }
  }, error = function(e) {
    cat("✗ Failed to create:", dir_name, "\n")
    log_message(sprintf("Failed to create directory %s: %s", dir_name, e$message), "ERROR")
  })
}

# ============================================================================
# 8. CHECK FOR DATA FILES
# ============================================================================

cat("\n========================================\n")
cat("CHECKING FOR DATA FILES\n")
cat("========================================\n\n")

required_files <- c("hr_cores.csv", "composite_cores.csv")
found_files <- character()
missing_files <- character()

for (file in required_files) {
  if (file.exists(file)) {
    found_files <- c(found_files, file)
    cat("✓", file, "found\n")
    
    # Quick validation
    tryCatch({
      df <- read.csv(file, nrows = 5)
      cat("  Columns:", paste(names(df), collapse = ", "), "\n")
    }, error = function(e) {
      cat("  ⚠ Could not read file\n")
      log_message(sprintf("Could not read %s: %s", file, e$message), "WARNING")
    })
  } else {
    missing_files <- c(missing_files, file)
    cat("✗", file, "NOT FOUND\n")
  }
}

if (length(missing_files) > 0) {
  cat("\n⚠ Missing data files. Please ensure the following files are in the working directory:\n")
  for (file in missing_files) {
    cat("  -", file, "\n")
  }
}

# ============================================================================
# 9. CREATE HELPER FILES
# ============================================================================

cat("\n========================================\n")
cat("CREATING HELPER FILES\n")
cat("========================================\n\n")

# Create .Rprofile for project settings
rprofile_content <- '# Project-specific R settings
options(
  stringsAsFactors = FALSE,
  scipen = 999,
  digits = 4,
  max.print = 100,
  warn = 1
)

# Set default CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Load commonly used packages
suppressPackageStartupMessages({
  if (require(dplyr, quietly = TRUE)) cat("✓ dplyr loaded\\n")
  if (require(ggplot2, quietly = TRUE)) cat("✓ ggplot2 loaded\\n")
})

cat("\\n.Rprofile loaded successfully\\n\\n")
'

tryCatch({
  writeLines(rprofile_content, ".Rprofile")
  cat("✓ Created .Rprofile with project settings\n")
  log_message("Created .Rprofile")
}, error = function(e) {
  log_message(sprintf("Failed to create .Rprofile: %s", e$message), "WARNING")
})

# Create analysis template
template_content <- '# ============================================================================
# Analysis Template
# ============================================================================
# Purpose: [Describe purpose]
# Author: [Your name]
# Date: [Date]
# ============================================================================

# Load packages ----
library(dplyr)
library(ggplot2)

# Source helper functions ----
source("helper_functions.R")

# Set parameters ----
# Define analysis parameters here

# Load data ----
# Load your processed data

# Analysis ----
# Your analysis code here

# Save results ----
# Save outputs

# Session info ----
sessionInfo()
'

tryCatch({
  writeLines(template_content, "analysis_template.R")
  cat("✓ Created analysis_template.R\n")
  log_message("Created analysis template")
}, error = function(e) {
  log_message(sprintf("Failed to create template: %s", e$message), "WARNING")
})

# ============================================================================
# 10. FINAL SUMMARY AND RECOMMENDATIONS
# ============================================================================

cat("\n========================================\n")
cat("SETUP COMPLETE!\n")
cat("========================================\n\n")

# Calculate setup success metrics
total_required <- length(required_packages)
total_installed <- sum(required_packages %in% installed_packages)
success_rate <- round(total_installed / total_required * 100, 1)

cat("Setup Summary:\n")
cat("----------------------------------------\n")
cat("✓ R version:", r_version_string, "\n")
cat("✓ Working directory:", getwd(), "\n")
cat("✓ Package installation:", success_rate, "% complete\n")
cat("✓ Directory structure created\n")
cat("✓ Setup log saved to:", log_file, "\n\n")

if (success_rate == 100 && length(missing_files) == 0) {
  cat("🎉 EXCELLENT! Your environment is fully configured.\n\n")
  cat("You can now run the analysis modules:\n")
  cat("  1. source('01_data_prep.R')\n")
  cat("  2. source('02_exploratory_analysis.R')\n")
  cat("  3. source('03_depth_modeling.R')\n")
  cat("  4. source('04_spatial_modeling.R')\n")
} else {
  cat("⚠ PARTIAL SETUP: Some components need attention.\n\n")
  
  if (success_rate < 100) {
    cat("Missing packages need to be installed manually.\n")
    cat("Try: install.packages(c(", paste(sprintf('"%s"', missing_packages), collapse = ", "), "))\n\n", sep = "")
  }
  
  if (length(missing_files) > 0) {
    cat("Please add the required data files to the working directory.\n\n")
  }
}

cat("\nNext steps:\n")
cat("----------------------------------------\n")
cat("1. Review the setup log:", log_file, "\n")
cat("2. Ensure data files are in the correct format\n")
cat("3. Test with: source('helper_functions.R')\n")
cat("4. Run data validation: validate_core_data(read.csv('hr_cores.csv'))\n\n")

cat("For help, check the README.md file or analysis documentation.\n\n")

# Save setup summary
setup_summary <- list(
  date = Sys.Date(),
  r_version = r_version_string,
  working_directory = getwd(),
  packages_installed = installed_packages,
  packages_missing = missing_packages,
  data_files_found = found_files,
  data_files_missing = missing_files,
  directories_created = names(directories),
  success_rate = success_rate
)

saveRDS(setup_summary, file.path("data_processed", "setup_summary.rds"))
log_message("Setup complete")

cat("Happy analyzing! 🎉\n\n")
