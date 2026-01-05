#' Run Network Meta-Analysis
#'
#' @description
#' Performs network meta-analysis for Shiny app integration.
#' Returns NMA object and treatment list for UI controls.
#'
#' @param data Data frame containing network meta-analysis data
#' @param data_type Character. Either "continuous" or "dichotomous"
#' @param effect_measure Character. Effect size measure (MD, SMD, OR, RR, RD)
#' @param reference_group Character. Name of reference treatment (user-selected)
#' @param model_type Character. Either "random" (default), "common", or "both"
#' @param method_tau Character. Tau estimation method. Default "REML"
#'
#' @return List containing:
#'   - nma: netmeta object for plotting
#'   - treatments: character vector of all treatments (for dropdown)
#'   - n_studies: number of studies
#'   - n_comparisons: number of pairwise comparisons
#'   - treatment_classes: data frame with treatment and treatment_class columns (NEW)
#'
#' @export
run_nma <- function(data,
                    data_type,
                    effect_measure,
                    reference_group,
                    model_type = "random",
                    method_tau = "REML") {
  
  # ==========================================================================
  # VALIDATION
  # ==========================================================================
  
  # Check data
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    stop("Invalid or empty data frame")
  }
  
  # Check required columns
  required_cols <- c("study", "treatment", "n")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Validate data type
  data_type <- tolower(data_type)
  if (!data_type %in% c("continuous", "dichotomous")) {
    stop("'data_type' must be 'continuous' or 'dichotomous'")
  }
  
  # Check type-specific columns
  if (data_type == "continuous" && !all(c("mean", "sd") %in% names(data))) {
    stop("Continuous data requires: study, treatment, mean, sd, n")
  }
  
  if (data_type == "dichotomous" && !"events" %in% names(data)) {
    stop("Dichotomous data requires: study, treatment, events, n")
  }
  
  # Validate effect measure
  effect_measure <- toupper(effect_measure)
  valid_continuous <- c("MD", "SMD")
  valid_dichotomous <- c("OR", "RR", "RD")
  
  if (data_type == "continuous" && !effect_measure %in% valid_continuous) {
    stop("For continuous: use MD or SMD")
  }
  
  if (data_type == "dichotomous" && !effect_measure %in% valid_dichotomous) {
    stop("For dichotomous: use OR, RR, or RD")
  }
  
  # Validate reference group
  treatments <- unique(data$treatment)
  if (!reference_group %in% treatments) {
    stop("Reference group '", reference_group, "' not found in data")
  }
  
  # Validate model settings
  model_type <- tolower(model_type)
  if (!model_type %in% c("random", "common")) {
    stop("'model_type' must be 'random', 'common', or 'both'")
  }
  
  # ==========================================================================
  # ✅ EXTRACT TREATMENT CLASSES BEFORE PAIRWISE TRANSFORMATION
  # ==========================================================================
  
  treatment_classes <- NULL
  
  if ("treatment_class" %in% names(data)) {
    
    # cat("\n=== DEBUG: Extracting Treatment Classes ===\n")
    # cat("Column names in data:\n")
    # print(names(data))
    # cat("\nFirst few rows:\n")
    # print(head(data[, c("treatment", "treatment_class")]))
    
    treatment_classes <- data %>%
      dplyr::select(treatment, treatment_class) %>%
      dplyr::distinct() %>%
      dplyr::arrange(treatment)
    
    # Convert tibble to data.frame for compatibility
    treatment_classes <- as.data.frame(treatment_classes)
    
    # cat("\nExtracted treatment_classes:\n")
    # print(treatment_classes)
    # cat("Class:", class(treatment_classes), "\n")
    # cat("Dimensions:", nrow(treatment_classes), "x", ncol(treatment_classes), "\n")
    # cat("===========================================\n\n")
    # 
    # Validate
    if (nrow(treatment_classes) == 0) {
      warning("treatment_class column exists but no classes found")
      treatment_classes <- NULL
    }
    
  } else {
    # cat("\n⚠️ WARNING: No 'treatment_class' column found in data\n")
    # cat("Available columns:", paste(names(data), collapse = ", "), "\n\n")
  }
  
  # ==========================================================================
  # TRANSFORM TO PAIRWISE FORMAT
  # ==========================================================================
  
  if (data_type == "continuous") {
    pw <- meta::pairwise(
      treat = treatment,
      mean = mean,
      sd = sd,
      n = n,
      studlab = study,
      data = data,
      sm = effect_measure
    )
  } else {
    pw <- meta::pairwise(
      treat = treatment,
      event = events,
      n = n,
      studlab = study,
      data = data,
      sm = effect_measure
    )
  }
  
  # ==========================================================================
  # RUN NETWORK META-ANALYSIS
  # ==========================================================================
  
  common_effects <- model_type %in% c("common", "both")
  random_effects <- model_type %in% c("random", "both")
  
  nma <- netmeta::netmeta(
    TE = TE,
    seTE = seTE,
    treat1 = treat1,
    treat2 = treat2,
    studlab = studlab,
    data = pw,
    sm = effect_measure,
    common = common_effects,
    random = random_effects,
    reference.group = reference_group,
    method.tau = method_tau,
    details.chkmultiarm = TRUE,
    sep.trts = " vs "
  )
  
  # ==========================================================================
  # EXTRACT LEAGUE TABLE
  # ==========================================================================
  
  league_model <- if (model_type == "common") "common" else "random"
  league_table <- extract_league_table(
    nma,
    model = league_model,
    digits = 3,
    add_significance = TRUE
  )
  
  # ==========================================================================
  # ✅ RETURN RESULTS WITH TREATMENT CLASSES
  # ==========================================================================
  
  result <- list(
    nma = nma,
    treatments = sort(treatments),
    n_studies = length(unique(data$study)),
    n_comparisons = nrow(pw),
    league = league_table,
    treatment_classes = treatment_classes  # ✅ Include treatment class mapping
  )
  
  # cat("\n=== DEBUG: Final Result Structure ===\n")
  # cat("Result list names:\n")
  # print(names(result))
  # cat("treatment_classes in result:\n")
  # print(result$treatment_classes)
  # cat("=====================================\n\n")
  # 
  return(result)
}



# =============================================================================
# HELPER: GET TREATMENTS LIST (for populating dropdown BEFORE analysis)
# =============================================================================

#' Get Treatment List from Data
#'
#' @description
#' Extracts unique treatments from uploaded data for UI dropdown.
#' Use this to populate reference group selector before running analysis.
#'
#' @param data Data frame with 'treatment' column
#' @return Character vector of unique treatments (sorted)
#'
#' @export
get_treatments <- function(data) {
  if (is.null(data) || !is.data.frame(data)) {
    return(NULL)
  }
  
  if (!"treatment" %in% names(data)) {
    return(NULL)
  }
  
  treatments <- unique(data$treatment)
  return(sort(treatments))
}

# ✅ NEW HELPER FUNCTION
#' Get Treatment Classes from Data
#'
#' @description
#' Extracts treatment class mapping from uploaded data.
#' Returns NULL if no treatment_class column exists.
#'
#' @param data Data frame with 'treatment' and optional 'treatment_class' columns
#' @return Data frame with treatment and treatment_class columns, or NULL
#'
#' @export
get_treatment_classes <- function(data) {
  if (is.null(data) || !is.data.frame(data)) {
    return(NULL)
  }
  
  if (!all(c("treatment", "treatment_class") %in% names(data))) {
    return(NULL)
  }
  
  classes <- data %>%
    dplyr::select(treatment, treatment_class) %>%
    dplyr::distinct() %>%
    dplyr::arrange(treatment)
  
  return(classes)
}
