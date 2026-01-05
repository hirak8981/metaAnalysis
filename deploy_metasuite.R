# =============================================================================
# MetaSuite Deployment Script for shinyapps.io
# =============================================================================

library(rsconnect)
library(renv)
library(fs)

deploy_metasuite <- function() {
  
  cat("=== MetaSuite Deployment to shinyapps.io ===\n\n")
  
  # Step 1: Verify .rshinyignore exists
  cat("Step 1: Check .rshinyignore...\n")
  if (!file.exists(".rshinyignore")) {
    stop("✗ .rshinyignore not found! Create it first.")
  }
  cat("✓ .rshinyignore found\n\n")
  
  # Step 2: Check for lowercase .r files
  cat("Step 2: Check file extensions (case sensitivity)...\n")
  lowercase_files <- dir_ls("R", recurse = TRUE, glob = "*.r")
  if (length(lowercase_files) > 0) {
    cat("✗ Found lowercase .r files:\n")
    print(lowercase_files)
    stop("Fix case sensitivity before deployment!")
  }
  cat("✓ All R files use uppercase .R extension\n\n")
  
  # Step 3: Test global.R syntax
  cat("Step 3: Test global.R syntax...\n")
  test_result <- try(source("global.R", local = TRUE), silent = TRUE)
  if (inherits(test_result, "try-error")) {
    cat("✗ Error in global.R:\n")
    print(test_result)
    stop("Fix global.R syntax errors!")
  }
  cat("✓ global.R loads successfully\n\n")
  
  # Step 4: Verify renv status
  cat("Step 4: Check renv status...\n")
  renv_status <- renv::status()
  if (!is.null(renv_status) && length(renv_status) > 0) {
    cat("⚠ renv issues detected:\n")
    print(renv_status)
    
    response <- readline("Update renv snapshot? (y/n): ")
    if (tolower(response) == "y") {
      renv::snapshot()
      cat("✓ renv snapshot updated\n\n")
    } else {
      stop("Fix renv issues before deployment")
    }
  } else {
    cat("✓ renv is synchronized\n\n")
  }
  
  # Step 5: Verify pacman is in renv.lock
  cat("Step 5: Verify pacman in renv.lock...\n")
  renv_lock <- jsonlite::read_json("renv.lock")
  if (!"pacman" %in% names(renv_lock$Packages)) {
    cat("✗ pacman not in renv.lock\n")
    cat("Adding pacman to dependencies...\n")
    library(pacman)  # Load to ensure it's tracked
    renv::snapshot()
  }
  cat("✓ pacman is in renv.lock\n\n")
  
  # Step 6: Check data folder size
  cat("Step 6: Check data folder size...\n")
  data_files <- dir_ls("data", recurse = TRUE, type = "file")
  total_size <- sum(file.size(data_files)) / (1024^2)  # MB
  cat(sprintf("Data folder: %.2f MB (%d files)\n", total_size, length(data_files)))
  
  if (total_size > 100) {
    warning("⚠ Data folder is large (>100 MB). Consider optimization.")
  } else {
    cat("✓ Data folder size acceptable\n")
  }
  cat("\n")
  
  # Step 7: Verify required files exist
  cat("Step 7: Verify required files...\n")
  required_files <- c("app.R", "global.R", "ui.R", "server.R", 
                      "renv.lock", "renv/activate.R", ".Rprofile")
  
  for (file in required_files) {
    if (!file.exists(file)) {
      stop(sprintf("✗ Required file missing: %s", file))
    }
  }
  cat("✓ All required files present\n\n")
  
  # Step 8: Local test
  cat("Step 8: Test app locally...\n")
  cat("Testing app.R structure...\n")
  test_app <- try(source("app.R", local = TRUE), silent = TRUE)
  if (inherits(test_app, "try-error")) {
    cat("✗ Error loading app.R:\n")
    print(test_app)
    stop("Fix app.R before deployment!")
  }
  cat("✓ app.R structure valid\n\n")
  
  # Step 9: Confirm deployment
  cat("=== Ready to Deploy ===\n")
  cat("App name: metasuite\n")
  cat("Structure: ✓\n")
  cat("Dependencies: ✓\n")
  cat("Data files: ✓\n\n")
  
  response <- readline("Deploy to shinyapps.io? (y/n): ")
  
  if (tolower(response) != "y") {
    cat("Deployment cancelled.\n")
    return(invisible(NULL))
  }
  
  # Step 10: Deploy
  cat("\n=== Deploying to shinyapps.io ===\n")
  
  rsconnect::deployApp(
    appName = "metasuite",
    appTitle = "MetaSuite: Comprehensive Meta-Analysis Platform",
    appFiles = c(
      "app.R",
      "global.R",
      "ui.R",
      "server.R",
      "R/",
      "www/",
      "data/",
      "renv.lock",
      "renv/activate.R",
      ".Rprofile"
    ),
    forceUpdate = TRUE,
    lint = TRUE,
    logLevel = "verbose",
    launch.browser = TRUE
  )
  
  cat("\n✓ Deployment complete!\n")
  cat("Your app should open in browser automatically.\n")
  cat("If not, visit: https://hirak-sen-roy.shinyapps.io/metasuite\n")
}

# Run deployment
deploy_metasuite()

