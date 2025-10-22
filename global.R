# =============================================================================
# global.R - Global Configuration for MetaSuite
# =============================================================================

# Load packages
if (!require("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  shiny,
  bslib,
  shinyjs,
  DT,
  readxl,
  writexl,
  colourpicker,
  ggplot2,
  dplyr,
  tidyr,
  fs,
  here
)

# Set options
options(shiny.maxRequestSize = 50 * 1024^2)

# Source shared files
source("R/shared/theme.R")
source("R/shared/utils.R")

# Source landing page
source("R/landing/mod_landing_page.R")

# Source documentation
source("R/shared/mod_documentation.R")


# Source ROB modules
source("R/rob/mod_rob_main.R")
source("R/rob/mod_data_loader.R")
source("R/rob/mod_file_upload.R")
source("R/rob/mod_plot_controls.R")
source("R/rob/mod_plot_display.R")
source("R/rob/mod_rob_summary_plot.R")      # ← Changed: added "mod_"
source("R/rob/mod_rob_traffic_plot.R")      # ← Changed: added "mod_"
source("R/rob/mod_rob_heatmap_plot.R")
source("R/rob/rob_domain_mapper.R")
source("R/rob/mod_format_helper.R")

# Source meta & network placeholders
source("R/meta/mod_meta_main.R")
source("R/network/mod_network_main.R")

# Source common modules
source("R/common/mod_loading_indicator.R")

cat("✓ MetaSuite loaded successfully!\n")
