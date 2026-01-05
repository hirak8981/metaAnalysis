# =============================================================================
# global.R - Global Configuration for MetaSuite
# =============================================================================

# Load pacman package manager
# Note: pacman must be in renv.lock for shinyapps.io deployment
if (!require("pacman", quietly = TRUE)) {
  # This will fail on shinyapps.io - ensure pacman is in renv.lock
  stop("pacman package not found. Run renv::snapshot() locally to capture it.")
}
library(pacman)

# Load all required packages
pacman::p_load(
  # Core Shiny
  shiny,
  bslib,
  shinyjs,
  
  # Data handling
  DT,
  readxl,
  writexl,
  dplyr,
  tidyr,
  purrr,
  tibble,
  broom,
  
  # Visualization
  ggplot2,
  colourpicker,
  patchwork,
  ggrepel,
  RColorBrewer,
  viridis,
  
  # Meta-analysis
  meta,
  metafor,
  metaviz,
  netmeta,
  rankinma,
  
  # Tables and reports
  gt,
  gtExtras,
  flextable,
  officer,
  rmarkdown,
  
  # Utilities
  fs,
  here,
  rlang
)

# Set options
options(shiny.maxRequestSize = 50 * 1024^2)  # 50 MB max file upload
# Source shared files
source("R/shared/theme.R")
source("R/shared/utils.R")
source("R/shared/mod_plot_display.R")
# Source documentation
source("R/shared/mod_documentation.R")
# Source common modules
source("R/shared/mod_loading_indicator.R")


# Source landing page
source("R/landing/mod_landing_page.R")



# Source ROB modules
source("R/rob/mod_rob_main.R")
source("R/rob/mod_rob_data_loader.R")
source("R/rob/mod_rob_file_upload.R")
source("R/rob/mod_rob_plot_controls.R")
source("R/rob/mod_rob_summary_plot.R")      
source("R/rob/mod_rob_traffic_plot.R")      
source("R/rob/mod_rob_heatmap_plot.R")
source("R/rob/rob_domain_mapper.R")
source("R/rob/mod_rob_format_helper.R")
source("R/rob/mod_rob_validator.R")

# Source meta modules
source("R/meta/modules/mod_meta_main.R")
source("R/meta/modules/mod_meta_data_loader.R")
source("R/meta/modules/mod_meta_file_upload.R")
source("R/meta/modules/mod_meta_format_helper.R")
source("R/meta/modules/mod_meta_model_settings.R")
source("R/meta/modules/mod_meta_results_display.R")        
source("R/meta/modules/mod_meta_effect_plots.R")           
source("R/meta/modules/mod_meta_influence_plots.R")        
source("R/meta/modules/mod_meta_plot_customization.R")  


# Meta analysis core functions
source("R/meta/core_files/meta_rma2.R")
source("R/meta/core_files/meta_get_display_scale.R")
source("R/meta/core_files/meta_viz_forest_custom.R")
source("R/meta/core_files/meta_viz_funnel_custom.R")
source("R/meta/core_files/meta_viz_sunset_custom.R")
source("R/meta/core_files/meta_influence.R")
source("R/meta/core_files/meta_loo_plot.R")
source("R/meta/core_files/meta_plot_cook_dist.R")
source("R/meta/core_files/meta_studentized_residuals.R")
source("R/meta/core_files/meta_pubias_test.R")
source("R/meta/core_files/meta_data_validator.R")
source("R/meta/core_files/meta_analysis_engine.R")



# Source NMA modules
source("R/network/modules/mod_network_main.R")           
source("R/network/modules/mod_nma_data_loader.R")
source("R/network/modules/mod_nma_file_upload.R")
source("R/network/modules/mod_nma_model_settings.R")
source("R/network/modules/mod_nma_results_display.R")      
source("R/network/modules/mod_nma_network_plots.R")        
source("R/network/modules/mod_nma_ranking_inconsistency.R")  
source("R/network/modules/mod_nma_plot_customization.R")   
source("R/network/modules/mod_nma_format_helper.R")

# Source NMA core functions 
source("R/network/core_files/nma_run_nma.R")
source("R/network/core_files/nma_results_extract.R")
source("R/network/core_files/nma_league.R")
source("R/network/core_files/nma_network.R")
source("R/network/core_files/nma_forest.R")
source("R/network/core_files/nma_netheat.R")
source("R/network/core_files/nma_direct_evidence.R")
source("R/network/core_files/nma_rank_radial.R")
source("R/network/core_files/nma_beading_plots.R")
source("R/network/core_files/nma_stacked_plots.R")
source("R/network/core_files/nma_plot_utils.R")
source("R/network/core_files/nma_dual_network.R")
source("R/network/core_files/nma_data_validator.R")


cat("✓ MetaSuite loaded successfully!\n")
