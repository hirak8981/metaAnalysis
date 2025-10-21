# =============================================================================
# Project Structure Setup Script
# Creates all necessary folders and placeholder files for MetaSuite
# =============================================================================

# Install fs package if not available
if (!require("fs", quietly = TRUE)) {
  install.packages("fs")
}

library(fs)

cat("🚀 Setting up MetaSuite project structure...\n\n")

# =============================================================================
# 1. CREATE DIRECTORY STRUCTURE
# =============================================================================

cat("📁 Creating directories...\n")

# Main R module folders
dir_create("R/shared")
dir_create("R/landing")
dir_create("R/rob")
dir_create("R/meta")
dir_create("R/network")
dir_create("R/common")

# WWW folders
dir_create("www/css")
dir_create("www/js")
dir_create("www/images")

# Data folders
dir_create("data/rob")
dir_create("data/meta")
dir_create("data/network")

cat("  ✓ R/shared/\n")
cat("  ✓ R/landing/\n")
cat("  ✓ R/rob/\n")
cat("  ✓ R/meta/\n")
cat("  ✓ R/network/\n")
cat("  ✓ R/common/\n")
cat("  ✓ www/css/\n")
cat("  ✓ www/js/\n")
cat("  ✓ www/images/\n")
cat("  ✓ data/rob/\n")
cat("  ✓ data/meta/\n")
cat("  ✓ data/network/\n\n")

# =============================================================================
# 2. CREATE PLACEHOLDER FILES (R modules)
# =============================================================================

cat("📝 Creating placeholder files...\n\n")

# --- R/shared/utils.R ---
file_create("R/shared/utils.R")
writeLines(c(
  "# =============================================================================",
  "# Shared Utility Functions",
  "# =============================================================================",
  "",
  "# Add shared utility functions here"
), "R/shared/utils.R")
cat("  ✓ R/shared/utils.R\n")

# --- R/meta/mod_meta_main.R ---
file_create("R/meta/mod_meta_main.R")
writeLines(c(
  "# =============================================================================",
  "# Meta-Analysis Main Module",
  "# =============================================================================",
  "",
  "metaMainUI <- function(id) {",
  "  ns <- NS(id)",
  "  ",
  "  div(",
  "    style = 'padding: 100px; text-align: center;',",
  "    h2('Meta-Analysis Module', style = 'color: #1d3557;'),",
  "    p('Coming soon...', style = 'color: #7F8C8D; font-size: 18px;'),",
  "    p('This module will include:', style = 'margin-top: 30px; color: #5A6169;'),",
  "    tags$ul(",
  "      style = 'list-style: none; padding: 0;',",
  "      tags$li(icon('check'), ' Forest plots'),",
  "      tags$li(icon('check'), ' Funnel plots'),",
  "      tags$li(icon('check'), ' Heterogeneity assessment'),",
  "      tags$li(icon('check'), ' Subgroup analysis')",
  "    )",
  "  )",
  "}",
  "",
  "metaMainServer <- function(id) {",
  "  moduleServer(id, function(input, output, session) {",
  "    # Meta-analysis logic will go here",
  "  })",
  "}"
), "R/meta/mod_meta_main.R")
cat("  ✓ R/meta/mod_meta_main.R\n")

# --- R/network/mod_network_main.R ---
file_create("R/network/mod_network_main.R")
writeLines(c(
  "# =============================================================================",
  "# Network Meta-Analysis Main Module",
  "# =============================================================================",
  "",
  "networkMainUI <- function(id) {",
  "  ns <- NS(id)",
  "  ",
  "  div(",
  "    style = 'padding: 100px; text-align: center;',",
  "    h2('Network Meta-Analysis Module', style = 'color: #1d3557;'),",
  "    p('Coming soon...', style = 'color: #7F8C8D; font-size: 18px;'),",
  "    p('This module will include:', style = 'margin-top: 30px; color: #5A6169;'),",
  "    tags$ul(",
  "      style = 'list-style: none; padding: 0;',",
  "      tags$li(icon('check'), ' Network plots'),",
  "      tags$li(icon('check'), ' League tables'),",
  "      tags$li(icon('check'), ' SUCRA rankings'),",
  "      tags$li(icon('check'), ' Consistency assessment')",
  "    )",
  "  )",
  "}",
  "",
  "networkMainServer <- function(id) {",
  "  moduleServer(id, function(input, output, session) {",
  "    # Network meta-analysis logic will go here",
  "  })",
  "}"
), "R/network/mod_network_main.R")
cat("  ✓ R/network/mod_network_main.R\n")

# --- README.md in data folders ---
file_create("data/rob/README.md")
writeLines(c(
  "# ROB Data Files",
  "",
  "Place your ROB assessment data files here:",
  "- quadas.xlsx",
  "- quips.xlsx",
  "- rob2.xlsx",
  "- robins_e.xlsx",
  "- robins_i.xlsx"
), "data/rob/README.md")
cat("  ✓ data/rob/README.md\n")

file_create("data/meta/README.md")
writeLines(c(
  "# Meta-Analysis Data Files",
  "",
  "Place your meta-analysis data files here."
), "data/meta/README.md")
cat("  ✓ data/meta/README.md\n")

file_create("data/network/README.md")
writeLines(c(
  "# Network Meta-Analysis Data Files",
  "",
  "Place your network meta-analysis data files here."
), "data/network/README.md")
cat("  ✓ data/network/README.md\n")

# --- .gitkeep files ---
file_create("www/images/.gitkeep")
cat("  ✓ www/images/.gitkeep\n")

# =============================================================================
# 3. CREATE FILE MOVEMENT SCRIPT
# =============================================================================

cat("\n📦 Creating file movement script...\n")

file_create("move_existing_files.R")
writeLines(c(
  "# =============================================================================",
  "# File Movement Script for MetaSuite",
  "# Run this AFTER setup_project_structure.R to move your existing files",
  "# =============================================================================",
  "",
  "library(fs)",
  "",
  "cat('📦 Moving existing files to new structure...\\n\\n')",
  "",
  "# Function to safely move files",
  "safe_move <- function(from, to) {",
  "  if (file_exists(from)) {",
  "    file_move(from, to)",
  "    cat('  ✓ Moved:', from, '→', to, '\\n')",
  "  } else {",
  "    cat('  ⚠ Not found:', from, '\\n')",
  "  }",
  "}",
  "",
  "# Move ROB modules",
  "cat('Moving ROB modules...\\n')",
  "safe_move('R/mod_data_loader.R', 'R/rob/mod_data_loader.R')",
  "safe_move('R/mod_file_upload.R', 'R/rob/mod_file_upload.R')",
  "safe_move('R/mod_plot_controls.R', 'R/rob/mod_plot_controls.R')",
  "safe_move('R/mod_plot_display.R', 'R/rob/mod_plot_display.R')",
  "",
  "# Move ROB plot functions",
  "safe_move('R/rob_summary_plot.R', 'R/rob/rob_summary_plot.R')",
  "safe_move('R/rob_traffic_plot.R', 'R/rob/rob_traffic_plot.R')",
  "safe_move('R/mod_rob_summary_plot.R', 'R/rob/rob_summary_plot.R')",
  "safe_move('R/mod_rob_traffic_plot.R', 'R/rob/rob_traffic_plot.R')",
  "",
  "# Move domain mapper",
  "safe_move('R/rob_domain_mapper.R', 'R/rob/rob_domain_mapper.R')",
  "",
  "# Move format helper",
  "safe_move('R/mod_format_helper.R', 'R/rob/mod_format_helper.R')",
  "",
  "# Move common modules",
  "cat('\\nMoving common modules...\\n')",
  "safe_move('R/mod_loading_indicator.R', 'R/common/mod_loading_indicator.R')",
  "",
  "# Move theme",
  "cat('\\nMoving theme...\\n')",
  "safe_move('R/theme.R', 'R/shared/theme.R')",
  "safe_move('theme.R', 'R/shared/theme.R')",
  "",
  "cat('\\n✅ File movement complete!\\n')",
  "cat('\\n📝 Next steps:\\n')",
  "cat('  1. Update global.R with new file paths\\n')",
  "cat('  2. Create R/rob/mod_rob_main.R\\n')",
  "cat('  3. Create R/landing/mod_landing_page.R\\n')",
  "cat('  4. Update ui.R and server.R\\n')"
), "move_existing_files.R")
cat("  ✓ move_existing_files.R\n")

# =============================================================================
# 4. CREATE PROJECT INFO
# =============================================================================

cat("\n📄 Creating project documentation...\n")

file_create("PROJECT_STRUCTURE.txt")
writeLines(c(
  "# MetaSuite - Project Structure",
  "",
  "## Directory Overview",
  "",
  "MetaSuite/",
  "├── app.R",
  "├── global.R",
  "├── ui.R",
  "├── server.R",
  "├── R/",
  "│   ├── shared/",
  "│   ├── landing/",
  "│   ├── rob/",
  "│   ├── meta/",
  "│   ├── network/",
  "│   └── common/",
  "├── www/",
  "│   ├── css/",
  "│   ├── js/",
  "│   └── images/",
  "└── data/",
  "    ├── rob/",
  "    ├── meta/",
  "    └── network/",
  "",
  "## Next Steps",
  "",
  "1. Run move_existing_files.R",
  "2. Create landing page module",
  "3. Update global.R with new paths",
  "4. Test the app"
), "PROJECT_STRUCTURE.txt")
cat("  ✓ PROJECT_STRUCTURE.txt\n")

# =============================================================================
# 5. SUMMARY
# =============================================================================

cat("\n")
cat("=============================================================\n")
cat("✅ MetaSuite project structure created successfully!\n")
cat("=============================================================\n\n")

cat("📁 Folders created:\n")
cat("  - R/ (6 subfolders)\n")
cat("  - www/ (css, js, images)\n")
cat("  - data/ (rob, meta, network)\n\n")

cat("📝 Files created:\n")
cat("  - R/shared/utils.R\n")
cat("  - R/meta/mod_meta_main.R\n")
cat("  - R/network/mod_network_main.R\n")
cat("  - move_existing_files.R\n")
cat("  - PROJECT_STRUCTURE.txt\n\n")

cat("🎯 Next Steps:\n")
cat("  1. Run: source('move_existing_files.R')\n")
cat("  2. Create new landing page files\n")
cat("  3. Update global.R\n\n")

cat("🎉 Setup complete!\n")
