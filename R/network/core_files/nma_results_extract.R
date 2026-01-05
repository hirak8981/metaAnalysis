# ==============================================================================
# SCRIPT: nma_results_extract.R
# Tidy Extraction of NMA Results with Auto-Formatting (3 Decimal Places)
# ==============================================================================

#' Extract NMA Summary Statistics (Model-Aware)
#'
#' @description
#' Extracts key summary statistics from a netmeta object including treatment
#' estimates, heterogeneity measures, and model tests. Automatically detects
#' which models are available (random, common, or both).
#'
#' @param nma A netmeta object from netmeta package
#' @param model Character; "random", "common", or "both" (default: "auto")
#'   - "auto": automatically detect from netmeta object
#'   - "random": extract random effects only
#'   - "common": extract common effects only
#'   - "both": extract both models (returns nested list)
#'
#' @return A list with tidy data frames for different result tables.
#'   If model = "both", returns a nested list with $random and $common components.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Random effects only
#' result <- run_nma(data, model_type = "random")
#' results <- extract_nma_summary(result$nma, model = "random")
#'
#' # Both models
#' result <- run_nma(data, model_type = "both")
#' results <- extract_nma_summary(result$nma, model = "both")
#' results$random$treatment_estimates
#' results$common$treatment_estimates
#' }
extract_nma_summary <- function(nma, model = "auto") {
  
  if (!inherits(nma, "netmeta")) {
    stop("nma must be a netmeta object")
  }
  
  # Auto-detect available models
  has_random <- !is.null(nma$random) || !is.null(nma$TE.random)
  has_common <- !is.null(nma$common) || !is.null(nma$TE.common)
  
  # Determine which model(s) to extract
  if (model == "auto") {
    if (has_random && has_common) {
      model <- "random" # Default to random when both available
      message("Both models available. Defaulting to 'random'. Use model = 'both' to extract both.")
    } else if (has_random) {
      model <- "random"
    } else if (has_common) {
      model <- "common"
    } else {
      stop("No random or common effects found in netmeta object")
    }
  }
  
  # Validate model selection
  if (model == "both") {
    if (!has_random || !has_common) {
      stop("model = 'both' requires both random and common effects. Use run_nma(..., model_type = 'both')")
    }
    
    # Extract both models
    message("Extracting both random and common effects models...")
    return(list(
      random = .extract_single_model(nma, "random"),
      common = .extract_single_model(nma, "common")
    ))
    
  } else {
    # Extract single model
    model <- match.arg(model, c("random", "common"))
    
    # Check if requested model is available
    if (model == "random" && !has_random) {
      stop("Random effects model not available. Use model = 'common' or run_nma(..., model_type = 'random')")
    }
    
    if (model == "common" && !has_common) {
      stop("Common effects model not available. Use model = 'random' or run_nma(..., model_type = 'common')")
    }
    
    return(.extract_single_model(nma, model))
  }
}


#' Internal function to extract a single model
#' @keywords internal
.extract_single_model <- function(nma, model) {
  
  # Extract treatment estimates
  ref <- nma$reference.group
  treatments <- nma$trts[nma$trts != ref]
  
  # Get effect measures
  TE_mat <- nma[[paste0("TE.", model)]]
  seTE_mat <- nma[[paste0("seTE.", model)]]
  lower_mat <- nma[[paste0("lower.", model)]]
  upper_mat <- nma[[paste0("upper.", model)]]
  
  # For ratio measures (OR, RR, HR), exponentiate
  if (nma$sm %in% c("OR", "RR", "HR", "IRR", "ROM")) {
    treatment_est <- data.frame(
      treatment = treatments,
      estimate = exp(TE_mat[treatments, ref]),
      lower_ci = exp(lower_mat[treatments, ref]),
      upper_ci = exp(upper_mat[treatments, ref]),
      z_value = TE_mat[treatments, ref] / seTE_mat[treatments, ref],
      p_value = 2 * (1 - pnorm(abs(TE_mat[treatments, ref] / seTE_mat[treatments, ref]))),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    
  } else {
    # For difference measures (MD, SMD)
    treatment_est <- data.frame(
      treatment = treatments,
      estimate = TE_mat[treatments, ref],
      lower_ci = lower_mat[treatments, ref],
      upper_ci = upper_mat[treatments, ref],
      z_value = TE_mat[treatments, ref] / seTE_mat[treatments, ref],
      p_value = 2 * (1 - pnorm(abs(TE_mat[treatments, ref] / seTE_mat[treatments, ref]))),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  
  # Add metadata and class
  attr(treatment_est, "effect_measure") <- nma$sm
  attr(treatment_est, "reference") <- ref
  attr(treatment_est, "model_type") <- ifelse(model == "random", "Random effects", "Common effects")
  class(treatment_est) <- c("nma_treatment_estimates", "data.frame")
  
  # Heterogeneity statistics (tidy)
  heterogeneity <- data.frame(
    statistic = c("tau²", "tau", "I²", "I² lower CI", "I² upper CI"),
    value = c(
      nma$tau^2,
      nma$tau,
      nma$I2,
      nma$lower.I2,
      nma$upper.I2
    ),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  class(heterogeneity) <- c("nma_heterogeneity", "data.frame")
  
  # Model tests (tidy)
  model_tests <- data.frame(
    test = c("Total", "Within designs", "Between designs"),
    q_statistic = c(nma$Q, nma$Q.heterogeneity, nma$Q.inconsistency),
    df = c(nma$df.Q, nma$df.Q.heterogeneity, nma$df.Q.inconsistency),
    p_value = c(nma$pval.Q, nma$pval.Q.heterogeneity, nma$pval.Q.inconsistency),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  class(model_tests) <- c("nma_model_tests", "data.frame")
  
  # Network characteristics (tidy)
  network_info <- data.frame(
    characteristic = c(
      "Number of studies (k)",
      "Number of pairwise comparisons (m)",
      "Number of treatments (n)",
      "Number of designs (d)",
      "Reference treatment",
      "Effect measure",
      "Model type"
    ),
    value = c(
      as.character(nma$k),
      as.character(nma$m),
      as.character(nma$n),
      as.character(nma$d),
      ref,
      nma$sm,
      ifelse(model == "random", "Random effects", "Common effects")
    ),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  class(network_info) <- c("nma_network_info", "data.frame")
  
  return(list(
    network_info = network_info,
    treatment_estimates = treatment_est,
    heterogeneity = heterogeneity,
    model_tests = model_tests,
    reference_group = ref,
    effect_measure = nma$sm,
    model_type = model
  ))
}


#' Extract Netsplit Results (Tidy Format with Interpretable Names)
#'
#' @description
#' Extracts and formats results from netsplit analysis (direct vs indirect evidence).
#' Column names follow standard netsplit conventions for easy interpretation.
#'
#' @param netsplit_obj A netsplit object from netmeta::netsplit()
#' @param model Character; "random", "common", or "both" (default: "auto")
#' @param include_metadata Logical; include column descriptions as attribute (default: TRUE)
#' @param remove_na Logical; remove rows where comparison cannot be performed (default: TRUE)
#'
#' @return A tidy data frame with netsplit results.
#'   If model = "both", returns a list with $random and $common components.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' netsplit_obj <- netmeta::netsplit(nma)
#' netsplit_data <- extract_netsplit_results(netsplit_obj)
#'
#' # View with descriptions
#' print(attr(netsplit_data, "column_info"))
#' }
extract_netsplit_results <- function(netsplit_obj,
                                     model = "auto",
                                     include_metadata = TRUE,
                                     remove_na = TRUE) {
  
  if (!inherits(netsplit_obj, "netsplit")) {
    stop("netsplit_obj must be a netsplit object from netmeta::netsplit()")
  }
  
  # Auto-detect available models
  has_random <- !is.null(netsplit_obj$random)
  has_common <- !is.null(netsplit_obj$common)
  
  # Determine which model(s) to extract
  if (model == "auto") {
    if (has_random && has_common) {
      model <- "random"
      message("Both models available in netsplit. Defaulting to 'random'. Use model = 'both' to extract both.")
    } else if (has_random) {
      model <- "random"
    } else if (has_common) {
      model <- "common"
    } else {
      stop("No random or common effects found in netsplit object")
    }
  }
  
  # Validate model selection
  if (model == "both") {
    if (!has_random || !has_common) {
      stop("model = 'both' requires netsplit run on netmeta object with both models")
    }
    
    # Extract both models
    message("Extracting netsplit results for both models...")
    return(list(
      random = .extract_single_netsplit(netsplit_obj, "random", include_metadata, remove_na),
      common = .extract_single_netsplit(netsplit_obj, "common", include_metadata, remove_na)
    ))
    
  } else {
    # Extract single model
    model <- match.arg(model, c("random", "common"))
    
    # Check if requested model is available
    if (model == "random" && !has_random) {
      stop("Random effects model not available in netsplit object")
    }
    
    if (model == "common" && !has_common) {
      stop("Common effects model not available in netsplit object")
    }
    
    return(.extract_single_netsplit(netsplit_obj, model, include_metadata, remove_na))
  }
}


#' Internal function to extract a single netsplit model
#' @keywords internal
.extract_single_netsplit <- function(netsplit_obj, model, include_metadata = TRUE, remove_na = TRUE) {
  
  # Build element names based on model
  direct_name <- paste0("direct.", model)
  indirect_name <- paste0("indirect.", model)
  compare_name <- paste0("compare.", model)
  prop_name <- paste0("prop.", model)
  
  # Extract vectors - CORRECT STRUCTURE
  comparison <- netsplit_obj$comparison
  k <- netsplit_obj$k
  prop_direct <- netsplit_obj[[prop_name]]
  
  # Nested elements
  nma <- netsplit_obj[[model]]$TE
  direct <- netsplit_obj[[direct_name]]$TE
  indirect <- netsplit_obj[[indirect_name]]$TE
  
  # Compare elements (difference, z, p)
  diff <- netsplit_obj[[compare_name]]$TE
  z <- netsplit_obj[[compare_name]]$z
  p_value <- netsplit_obj[[compare_name]]$p
  
  # Create tidy dataframe with interpretable column names
  netsplit_data <- data.frame(
    comparison = comparison,
    k = k,
    prop_direct = prop_direct,
    nma = nma,
    direct = direct,
    indirect = indirect,
    diff = diff,
    z = z,
    p_value = p_value,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  # Remove rows where comparison cannot be made (optional)
  if (remove_na) {
    valid_rows <- !is.na(netsplit_data$diff)
    n_removed <- sum(!valid_rows)
    
    if (n_removed > 0) {
      message(sprintf("  Removed %d comparison(s) with no direct or indirect evidence", n_removed))
    }
    
    netsplit_data <- netsplit_data[valid_rows, , drop = FALSE]
    rownames(netsplit_data) <- NULL
  }
  
  # Add metadata as attributes
  if (include_metadata) {
    
    # Column descriptions
    col_info <- data.frame(
      column = c("comparison", "k", "prop_direct", "nma", "direct", 
                 "indirect", "diff", "z", "p_value"),
      description = c(
        "Treatment comparison",
        "Number of studies providing direct evidence",
        "Proportion of information from direct evidence",
        sprintf("Network meta-analysis treatment effect (%s)", netsplit_obj$x$sm),
        sprintf("Treatment effect from direct evidence (%s)", netsplit_obj$x$sm),
        sprintf("Treatment effect from indirect evidence (%s)", netsplit_obj$x$sm),
        "Difference between direct and indirect estimates (Direct - Indirect)",
        "Z-value for disagreement test (Direct vs Indirect)",
        "P-value for disagreement test (Direct vs Indirect)"
      ),
      stringsAsFactors = FALSE
    )
    
    attr(netsplit_data, "column_info") <- col_info
    attr(netsplit_data, "effect_measure") <- netsplit_obj$x$sm
    attr(netsplit_data, "model_type") <- model
    attr(netsplit_data, "method") <- netsplit_obj$method
    
    # Add custom class for pretty printing
    class(netsplit_data) <- c("netsplit_tidy", "data.frame")
  }
  
  return(netsplit_data)
}


#' Get column descriptions for netsplit results
#'
#' @param netsplit_data Output from extract_netsplit_results()
#'
#' @return A data frame with column names and descriptions
#'
#' @export
#'
#' @examples
#' \dontrun{
#' netsplit_data <- extract_netsplit_results(netsplit_obj)
#' get_netsplit_legend(netsplit_data)
#' }
get_netsplit_legend <- function(netsplit_data) {
  
  col_info <- attr(netsplit_data, "column_info")
  
  if (is.null(col_info)) {
    # Fallback if metadata not available
    col_info <- data.frame(
      column = c("comparison", "k", "prop_direct", "nma", "direct", 
                 "indirect", "diff", "z", "p_value"),
      description = c(
        "Treatment comparison",
        "Number of studies providing direct evidence",
        "Proportion of information from direct evidence",
        "Network meta-analysis treatment effect",
        "Treatment effect from direct evidence",
        "Treatment effect from indirect evidence",
        "Difference between direct and indirect estimates",
        "Z-value for disagreement test",
        "P-value for disagreement test"
      ),
      stringsAsFactors = FALSE
    )
  }
  
  return(col_info)
}


#' Format netsplit data for display (with formatted columns)
#'
#' @param netsplit_data Output from extract_netsplit_results()
#' @param digits Integer; decimal places for numeric columns (default: 2)
#'
#' @return A formatted data frame ready for tables/reports
#'
#' @export
#'
#' @examples
#' \dontrun{
#' netsplit_data <- extract_netsplit_results(netsplit_obj)
#'
#' # For printing/viewing
#' format_netsplit_display(netsplit_data, digits = 3)
#'
#' # For publication tables
#' format_netsplit_display(netsplit_data, digits = 2) |>
#'   flextable::flextable()
#' }
format_netsplit_display <- function(netsplit_data, digits = 2) {
  
  df <- as.data.frame(netsplit_data)
  
  # Create formatted version
  df_display <- df
  
  # Format numeric columns using safe helper
  df_display$prop_direct <- .safe_sprintf(df$prop_direct, digits)
  df_display$nma <- .safe_sprintf(df$nma, digits)
  df_display$direct <- ifelse(is.na(df$direct), "—", .safe_sprintf(df$direct, digits))
  df_display$indirect <- ifelse(is.na(df$indirect), "—", .safe_sprintf(df$indirect, digits))
  df_display$diff <- ifelse(is.na(df$diff), "—", .safe_sprintf(df$diff, digits))
  df_display$z <- ifelse(is.na(df$z), "—", .safe_sprintf(df$z, digits))
  
  # Format p-value
  p_numeric <- suppressWarnings(as.numeric(df$p_value))
  df_display$p_value <- ifelse(
    is.na(p_numeric), "—",
    ifelse(p_numeric < 0.001, "< 0.001",
           sprintf("%.4f", p_numeric))
  )
  
  # Rename for display (optional - more readable headers)
  names(df_display) <- c(
    "Comparison", "k", "Prop. Direct", "NMA", "Direct",
    "Indirect", "Diff", "z", "P-value"
  )
  
  return(df_display)
}


#' Extract All NMA Results (Model-Aware)
#'
#' @param nma A netmeta object
#' @param run_netsplit Logical; whether to compute and extract netsplit results (default: TRUE)
#' @param model Character; "auto", "random", "common", or "both" (default: "auto")
#' @param remove_na Logical; remove netsplit rows with NA (no direct/indirect evidence) (default: TRUE)
#'
#' @return A list of tidy data frames.
#'   If model = "both", returns nested lists with $random and $common components.
#'
#' @export
extract_all_nma_results <- function(nma,
                                    run_netsplit = TRUE,
                                    model = "auto",
                                    remove_na = TRUE) {
  
  # Extract NMA summary
  nma_results <- extract_nma_summary(nma, model = model)
  
  # If both models extracted, handle separately
  if (!is.null(nma_results$random) && !is.null(nma_results$common)) {
    # Both models case
    results <- list(
      random = nma_results$random,
      common = nma_results$common
    )
    
    # Add netsplit if requested
    if (run_netsplit) {
      message("Running netsplit analysis...")
      netsplit_obj <- netmeta::netsplit(nma)
      netsplit_results <- extract_netsplit_results(
        netsplit_obj, 
        model = "both",
        include_metadata = TRUE,
        remove_na = remove_na
      )
      
      results$random$netsplit <- netsplit_results$random
      results$common$netsplit <- netsplit_results$common
    }
    
  } else {
    # Single model case
    results <- nma_results
    
    # Add netsplit if requested
    if (run_netsplit) {
      message("Running netsplit analysis...")
      netsplit_obj <- netmeta::netsplit(nma)
      results$netsplit <- extract_netsplit_results(
        netsplit_obj, 
        model = nma_results$model_type,
        include_metadata = TRUE,
        remove_na = remove_na
      )
    }
  }
  
  return(results)
}


# ==============================================================================
# CUSTOM PRINT METHODS WITH 3 DECIMAL PLACE FORMATTING
# ==============================================================================

#' Safe numeric formatting helper
#' @keywords internal
.safe_sprintf <- function(values, digits) {
  num_values <- suppressWarnings(as.numeric(values))
  formatted <- ifelse(
    is.na(num_values), 
    "NA",
    sprintf(paste0("%.", digits, "f"), num_values)
  )
  return(formatted)
}


#' @export
print.nma_network_info <- function(x, ...) {
  cat("\n")
  cat("Network Meta-Analysis: Network Information\n")
  cat("============================================\n\n")
  
  x_print <- as.data.frame(x)
  class(x_print) <- "data.frame"
  print(x_print, row.names = FALSE, right = FALSE, ...)
  
  invisible(x)
}


#' @export
print.nma_treatment_estimates <- function(x, ..., digits = 3) {
  cat("\n")
  cat("Network Meta-Analysis: Treatment Estimates\n")
  cat("===========================================\n")
  cat(sprintf("Effect measure: %s\n", attr(x, "effect_measure")))
  cat(sprintf("Reference: %s\n", attr(x, "reference")))
  cat(sprintf("Model: %s\n\n", attr(x, "model_type")))
  
  x_print <- x
  class(x_print) <- "data.frame"
  x_print$estimate <- .safe_sprintf(x$estimate, digits)
  x_print$lower_ci <- .safe_sprintf(x$lower_ci, digits)
  x_print$upper_ci <- .safe_sprintf(x$upper_ci, digits)
  x_print$z_value <- .safe_sprintf(x$z_value, digits)
  
  p_numeric <- suppressWarnings(as.numeric(x$p_value))
  x_print$p_value <- ifelse(
    is.na(p_numeric), "NA",
    ifelse(p_numeric < 0.001, "< 0.001",
           ifelse(p_numeric < 0.01, sprintf("%.4f", p_numeric),
                  sprintf("%.3f", p_numeric)))
  )
  
  print(x_print, row.names = FALSE, right = FALSE, ...)
  invisible(x)
}


#' @export
print.nma_heterogeneity <- function(x, ..., digits = 3) {
  cat("\n")
  cat("Network Meta-Analysis: Heterogeneity Statistics\n")
  cat("================================================\n\n")
  
  x_print <- x
  class(x_print) <- "data.frame"
  x_print$value <- .safe_sprintf(x$value, digits)
  
  print(x_print, row.names = FALSE, right = FALSE, ...)
  invisible(x)
}


#' @export
print.nma_model_tests <- function(x, ..., digits = 2) {
  cat("\n")
  cat("Network Meta-Analysis: Model Tests\n")
  cat("===================================\n\n")
  
  x_print <- x
  class(x_print) <- "data.frame"
  x_print$q_statistic <- .safe_sprintf(x$q_statistic, digits)
  x_print$df <- as.integer(x$df)
  
  p_numeric <- suppressWarnings(as.numeric(x$p_value))
  x_print$p_value <- ifelse(
    is.na(p_numeric), "NA",
    ifelse(p_numeric < 0.001, "< 0.001",
           sprintf("%.6f", p_numeric))
  )
  
  print(x_print, row.names = FALSE, right = FALSE, ...)
  invisible(x)
}


#' @export
print.netsplit_tidy <- function(x, ..., show_legend = TRUE, digits = 3) {
  
  cat("\n")
  cat("Network Meta-Analysis: Direct vs Indirect Evidence\n")
  cat("==================================================\n")
  cat(sprintf("Effect measure: %s\n", attr(x, "effect_measure")))
  cat(sprintf("Model: %s effects\n", attr(x, "model_type")))
  cat(sprintf("Method: %s\n", attr(x, "method")))
  cat(sprintf("Number of comparisons: %d\n\n", nrow(x)))
  
  # Format using safe helper
  x_print <- x
  class(x_print) <- "data.frame"
  x_print$prop_direct <- .safe_sprintf(x$prop_direct, digits)
  x_print$nma <- .safe_sprintf(x$nma, digits)
  x_print$direct <- .safe_sprintf(x$direct, digits)
  x_print$indirect <- .safe_sprintf(x$indirect, digits)
  x_print$diff <- .safe_sprintf(x$diff, digits)
  x_print$z <- .safe_sprintf(x$z, digits)
  
  p_numeric <- suppressWarnings(as.numeric(x$p_value))
  x_print$p_value <- ifelse(
    is.na(p_numeric), "NA",
    ifelse(p_numeric < 0.001, "< 0.001",
           ifelse(p_numeric < 0.01, sprintf("%.4f", p_numeric),
                  sprintf("%.3f", p_numeric)))
  )
  
  print(x_print, row.names = FALSE, right = FALSE, ...)
  
  if (show_legend && !is.null(attr(x, "column_info"))) {
    cat("\n")
    cat("Legend:\n")
    col_info <- attr(x, "column_info")
    for (i in seq_len(nrow(col_info))) {
      cat(sprintf("  %-12s - %s\n", col_info$column[i], col_info$description[i]))
    }
  }
  
  invisible(x)
}
