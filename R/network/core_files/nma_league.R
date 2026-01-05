#===============================================================================
# NMA LEAGUE TABLE EXTRACTION
# Clean tidy format for Shiny renderDT (no gt dependency)
#===============================================================================

#' Extract League Table Data from NMA Object
#' 
#' @description
#' Extracts pairwise comparison data from netmeta::netleague() output
#' in a clean tidy format suitable for DT::datatable display.
#' 
#' @param nma_obj A netmeta object
#' @param model Character; "random" or "common" (default: "random")
#' @param digits Integer; decimal places for rounding (default: 3)
#' @param add_significance Logical; add significance flag column (default: TRUE)
#' 
#' @return A list with two components:
#' \itemize{
#'   \item matrix: Wide format league table (treatments × treatments)
#'   \item long: Long format with columns: comparison, row_treatment, 
#'               col_treatment, estimate, lower_ci, upper_ci, significant
#' }
#' 
#' @export
#' 
#' @examples
#' library(netmeta)
#' data(Senn2013)
#' nma <- netmeta(TE, seTE, treat1, treat2, studlab, data = Senn2013)
#' league_data <- extract_league_table(nma)
#' 
#' # View matrix format
#' print(league_data$matrix)
#' 
#' # View long format
#' print(league_data$long)

extract_league_table <- function(nma_obj,
                                 model = "random",
                                 digits = 3,
                                 add_significance = TRUE) {
  
  # Validate model
  model <- match.arg(model, c("random", "common"))
  
  # Get netleague output
  league <- netmeta::netleague(
    nma_obj, 
    digits = digits,
    bracket = "[",
    separator = "; "
  )
  
  # Extract appropriate matrix
  league_matrix <- if (model == "random") {
    as.matrix(league$random)
  } else {
    as.matrix(league$common)
  }
  
  # ===================================================================
  # MATRIX FORMAT (Wide)
  # ===================================================================
  
  league_wide <- as.data.frame(league_matrix, stringsAsFactors = FALSE)
  league_wide$Treatment <- rownames(league_wide)
  
  # Reorder: Treatment column first
  treatment_cols <- setdiff(names(league_wide), "Treatment")
  league_wide <- league_wide[, c("Treatment", treatment_cols)]
  
  # ===================================================================
  # LONG FORMAT (Tidy)
  # ===================================================================
  
  # Initialize long format data frame
  long_df <- data.frame(
    row_treatment = character(),
    col_treatment = character(),
    comparison = character(),
    estimate = numeric(),
    lower_ci = numeric(),
    upper_ci = numeric(),
    ci_formatted = character(),
    stringsAsFactors = FALSE
  )
  
  # Parse each cell
  for (i in 1:nrow(league_matrix)) {
    for (j in 1:ncol(league_matrix)) {
      
      row_trt <- rownames(league_matrix)[i]
      col_trt <- colnames(league_matrix)[j]
      cell_value <- as.character(league_matrix[i, j])
      
      # Skip diagonal and empty cells
      if (is.na(cell_value) || 
          nchar(trimws(cell_value)) == 0 || 
          cell_value == "." ||
          row_trt == col_trt) {
        next
      }
      
      # Parse "estimate [lower; upper]"
      parsed <- .parse_league_cell(cell_value)
      
      if (!is.null(parsed)) {
        long_df <- rbind(long_df, data.frame(
          row_treatment = row_trt,
          col_treatment = col_trt,
          comparison = paste(row_trt, "vs", col_trt),
          estimate = parsed$estimate,
          lower_ci = parsed$lower,
          upper_ci = parsed$upper,
          ci_formatted = sprintf("[%.3f; %.3f]", parsed$lower, parsed$upper),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Add significance flag
  if (add_significance && nrow(long_df) > 0) {
    null_value <- ifelse(nma_obj$sm %in% c("OR", "RR", "HR"), 1, 0)
    long_df$significant <- !(long_df$lower_ci <= null_value & 
                               long_df$upper_ci >= null_value)
  }
  
  # Set class for print method
  class(league_wide) <- c("league_matrix", "data.frame")
  class(long_df) <- c("league_long", "data.frame")
  
  # Return both formats
  return(list(
    matrix = league_wide,
    long = long_df,
    model = model,
    effect_measure = nma_obj$sm,
    treatments = nma_obj$trts
  ))
}

#===============================================================================
# INTERNAL HELPER: Parse League Cell
#===============================================================================

.parse_league_cell <- function(cell_value) {
  
  # Check if contains brackets
  if (!grepl("\\[", cell_value)) {
    return(NULL)
  }
  
  # Split by brackets
  parts <- strsplit(cell_value, "\\[|\\]")[[1]]
  
  if (length(parts) < 2) {
    return(NULL)
  }
  
  # Extract estimate
  estimate <- suppressWarnings(as.numeric(trimws(parts[1])))
  
  # Extract CI bounds
  ci_str <- trimws(parts[2])
  ci_parts <- strsplit(ci_str, ";|,")[[1]]
  
  if (length(ci_parts) < 2) {
    return(NULL)
  }
  
  lower <- suppressWarnings(as.numeric(trimws(ci_parts[1])))
  upper <- suppressWarnings(as.numeric(trimws(ci_parts[2])))
  
  # Return parsed values
  if (!is.na(estimate) && !is.na(lower) && !is.na(upper)) {
    return(list(
      estimate = estimate,
      lower = lower,
      upper = upper
    ))
  }
  
  return(NULL)
}

#===============================================================================
# PRINT METHODS
#===============================================================================

print.league_matrix <- function(x, ...) {
  cat("\n==================================================\n")
  cat("Network Meta-Analysis: League Table (Matrix Format)\n")
  cat("==================================================\n\n")
  
  cat("Dimensions:", nrow(x), "×", ncol(x), "\n")
  cat("Format: Estimate [Lower CI; Upper CI]\n\n")
  
  print(as.data.frame(x), row.names = FALSE)
  
  cat("\n\nNote: Row treatment vs Column treatment\n")
  cat("Diagonal cells represent same treatment (not shown)\n\n")
  
  invisible(x)
}

print.league_long <- function(x, ...) {
  cat("\n==================================================\n")
  cat("Network Meta-Analysis: League Table (Long Format)\n")
  cat("==================================================\n\n")
  
  cat("Number of comparisons:", nrow(x), "\n\n")
  
  print(as.data.frame(x), row.names = FALSE, digits = 3)
  
  cat("\n\nColumns:\n")
  cat("  row_treatment   - Treatment in row position\n")
  cat("  col_treatment   - Treatment in column position\n")
  cat("  comparison      - Full comparison label\n")
  cat("  estimate        - Effect estimate\n")
  cat("  lower_ci        - Lower confidence interval\n")
  cat("  upper_ci        - Upper confidence interval\n")
  cat("  ci_formatted    - Formatted CI string\n")
  
  if ("significant" %in% names(x)) {
    cat("  significant     - Significance flag (TRUE/FALSE)\n")
  }
  
  cat("\n")
  
  invisible(x)
}

#===============================================================================
# HELPER: Get Upper Triangle Only (for cleaner display)
#===============================================================================

#' Extract Upper Triangle of League Table
#' 
#' @description
#' Gets only the upper triangle of the league table matrix to avoid 
#' redundancy (each comparison appears only once).
#' 
#' @param league_data Output from extract_league_table()
#' 
#' @return Data frame with upper triangle comparisons only
#' 
#' @export

extract_league_upper_triangle <- function(league_data) {
  
  long_df <- league_data$long
  
  if (nrow(long_df) == 0) {
    return(long_df)
  }
  
  # Create unique comparison ID (sorted alphabetically)
  long_df$comp_id <- apply(long_df[, c("row_treatment", "col_treatment")], 1, 
                           function(x) paste(sort(x), collapse = "_"))
  
  # Keep only unique comparisons
  long_df <- long_df[!duplicated(long_df$comp_id), ]
  long_df$comp_id <- NULL
  
  return(long_df)
}
