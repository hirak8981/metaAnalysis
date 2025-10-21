# =============================================================================
# File Movement Script for MetaSuite
# Run this AFTER setup_project_structure.R to move your existing files
# =============================================================================

library(fs)

cat('📦 Moving existing files to new structure...\n\n')

# Function to safely move files
safe_move <- function(from, to) {
  if (file_exists(from)) {
    file_move(from, to)
    cat('  ✓ Moved:', from, '→', to, '\n')
  } else {
    cat('  ⚠ Not found:', from, '\n')
  }
}

# Move ROB modules
cat('Moving ROB modules...\n')
safe_move('R/mod_data_loader.R', 'R/rob/mod_data_loader.R')
safe_move('R/mod_file_upload.R', 'R/rob/mod_file_upload.R')
safe_move('R/mod_plot_controls.R', 'R/rob/mod_plot_controls.R')
safe_move('R/mod_plot_display.R', 'R/rob/mod_plot_display.R')
safe_move('R/rob_summary_plot.R', 'R/rob/rob_summary_plot.R')
safe_move('R/rob_traffic_plot.R', 'R/rob/rob_traffic_plot.R')
safe_move('R/rob_domain_mapper.R', 'R/rob/rob_domain_mapper.R')

# Move common modules
cat('\nMoving common modules...\n')
safe_move('R/mod_loading_indicator.R', 'R/common/mod_loading_indicator.R')

# Move theme
cat('\nMoving theme...\n')
safe_move('R/theme.R', 'R/shared/theme.R')
safe_move('theme.R', 'R/shared/theme.R')

cat('\n✅ File movement complete!\n')
cat('\n📝 Next steps:\n')
cat('  1. Update global.R with new file paths\n')
cat('  2. Create R/rob/mod_rob_main.R\n')
cat('  3. Create R/landing/mod_landing_page.R\n')
cat('  4. Update ui.R and server.R\n')
