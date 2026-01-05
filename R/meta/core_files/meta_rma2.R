# ==============================================================================
# META-ANALYSIS WRAPPER FUNCTIONS WITH TIDY EVALUATION
# ==============================================================================

#' Automatic Transform Selection for Effect Measures
#'
#' @description
#' Automatically selects the appropriate transformation function and name
#' based on the effect size measure.
#'
#' @param measure Character; effect size measure code
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item transform: Function to back-transform (e.g., exp) or NULL
#'     \item name: Character name of the transformed measure
#'   }
#'
#' @keywords internal

auto_transform_for_measure <- function(measure) {
  
  measure <- toupper(measure)
  
  if (measure %in% c("OR", "RR", "ROM", "PETO")) {
    return(list(
      transform = exp,
      name = switch(
        measure,
        OR = "Odds Ratio",
        RR = "Risk Ratio",
        ROM = "Ratio of Means",
        PETO = "Odds Ratio"
      )
    ))
  }
  
  if (measure == "ZCOR") {
    return(list(
      transform = tanh,
      name = "Correlation"
    ))
  }
  
  list(
    transform = NULL,
    name = "Effect Size"
  )
}


#' Check Required Arguments for Meta-Analysis
#'
#' @description
#' Validates that all required arguments are provided based on the
#' effect size measure type.
#'
#' @param measure Character; effect size measure
#' @param required Character vector; names of required arguments
#' @param quosures Named list of quosures to check
#'
#' @return NULL (invisibly). Stops with error if required arguments are missing.
#'
#' @keywords internal

check_required <- function(measure, required, quosures) {
  
  missing <- vapply(
    required,
    function(nm) quo_is_null(quosures[[nm]]),
    logical(1)
  )
  
  if (any(missing)) {
    stop(
      "Measure '", measure,
      "' requires the following arguments: ",
      paste(required[missing], collapse = ", "),
      call. = FALSE
    )
  }
}


#' Perform Meta-Analysis with Tidy Evaluation
#'
#' @description
#' Comprehensive wrapper for metafor::escalc() and metafor::rma() that supports
#' tidy evaluation, automatic effect size calculation, and multiple measure types.
#' Handles binary outcomes, continuous outcomes, correlations, test statistics,
#' and generic effect sizes.
#'
#' @param data A data frame containing the study data
#' @param measure Character; effect size measure (see Details for supported measures)
#'
#' @section Binary Outcome Parameters:
#' @param event_exp Number of events in experimental group (bare column name)
#' @param n_exp Sample size in experimental group (bare column name)
#' @param event_ctrl Number of events in control group (bare column name)
#' @param n_ctrl Sample size in control group (bare column name)
#'
#' @section Continuous Outcome Parameters:
#' @param mean_exp Mean in experimental group (bare column name)
#' @param sd_exp Standard deviation in experimental group (bare column name)
#' @param mean_ctrl Mean in control group (bare column name)
#' @param sd_ctrl Standard deviation in control group (bare column name)
#'
#' @section Correlation Parameters:
#' @param ri Correlation coefficient (bare column name)
#' @param ni Sample size for correlation (bare column name)
#'
#' @section Test Statistic Parameters:
#' @param ti t-statistic (bare column name)
#' @param fi F-statistic (bare column name)
#'
#' @section Generic Effect Size Parameters:
#' @param yi Effect size (bare column name)
#' @param vi Sampling variance (bare column name)
#' @param mi Mean for single-group (bare column name)
#' @param sdi Standard deviation for single-group (bare column name)
#'
#' @section Meta-Analysis Options:
#' @param moderator Moderator variable for meta-regression (bare column name)
#' @param method Character; estimation method for tau² (default: "REML")
#' @param conf_level Numeric; confidence level for intervals (default: 0.95)
#'
#' @section Zero-Cell Correction:
#' @param add Numeric; value to add to cells (default: 0.5)
#' @param to Character; which cells to add to - "only0", "all", "if0all" (default: "only0")
#' @param drop00 Logical; drop studies with zero events in both groups (default: FALSE)
#'
#' @section Transformation:
#' @param transform Function to transform estimates (e.g., exp for log odds).
#'   If NULL, auto-detects based on measure.
#' @param transform_name Character; name for transformed estimate column
#'
#' @details
#' **Supported Effect Size Measures:**
#' 
#' *Binary outcomes:* RR (Risk Ratio), OR (Odds Ratio), RD (Risk Difference),
#' PETO (Peto's Odds Ratio), AS (Arcsine Square Root)
#' 
#' *Continuous outcomes:* MD (Mean Difference), SMD (Standardized Mean Difference),
#' SMDH (Hedges' g), SMD1, SMD2, ROM (Ratio of Means)
#' 
#' *Correlations:* COR (Correlation), ZCOR (Fisher's Z)
#' 
#' *Test statistics:* TSTAT (t-statistic), FSTAT (F-statistic)
#' 
#' *Generic:* GEN (Generic with yi/vi), MN (Single mean)
#'
#' @return A list with components:
#'   \itemize{
#'     \item summary: Overall meta-analysis summary tibble
#'     \item heterogeneity: Heterogeneity statistics tibble
#'     \item model_results: Model coefficients and confidence intervals tibble
#'     \item moderator_test: Test for moderators tibble (NULL if no moderators)
#'     \item data: Effect size data frame
#'     \item model: Full rma model object
#'     \item measure: Effect size measure used
#'   }
#'
#' @export
#'
#' @examples
#' library(metafor)
#' library(dplyr)
#' 
#' # Binary outcome: Odds Ratio
#' data(dat.bcg)
#' result <- perform_meta_analysis2(
#'   dat.bcg,
#'   measure = "OR",
#'   event_exp = tpos,
#'   n_exp = tpos + tneg,
#'   event_ctrl = cpos,
#'   n_ctrl = cpos + cneg
#' )
#' 
#' # Continuous outcome: SMD with moderator
#' result <- perform_meta_analysis2(
#'   data,
#'   measure = "SMD",
#'   mean_exp = mean_treatment,
#'   sd_exp = sd_treatment,
#'   n_exp = n_treatment,
#'   mean_ctrl = mean_control,
#'   sd_ctrl = sd_control,
#'   n_ctrl = n_control,
#'   moderator = study_quality
#' )

perform_meta_analysis2 <- function(
    data,
    measure,
    
    # ---- Binary ----
    event_exp = NULL, n_exp = NULL,
    event_ctrl = NULL, n_ctrl = NULL,
    
    # ---- Continuous (two-group) ----
    mean_exp = NULL, sd_exp = NULL,
    mean_ctrl = NULL, sd_ctrl = NULL,
    
    # ---- Correlation ----
    ri = NULL, ni = NULL,
    
    # ---- Test statistics ----
    ti = NULL,
    fi = NULL,
    
    # ---- Generic / single-group ----
    yi = NULL, vi = NULL,
    mi = NULL, sdi = NULL,
    
    # ---- Meta options ----
    moderator = NULL,
    method = "REML",
    
    # ---- Zero-cell correction ----
    add = 0.5,
    to = "only0",
    drop00 = FALSE,
    
    # ---- Transform ----
    transform = NULL,
    transform_name = "Transformed",
    conf_level = 0.95
) {
  
  measure <- toupper(measure)
  
  # ===================================================
  # CAPTURE QUOSURES
  # ===================================================
  
  event_exp_q <- enquo(event_exp)
  n_exp_q <- enquo(n_exp)
  event_ctrl_q <- enquo(event_ctrl)
  n_ctrl_q <- enquo(n_ctrl)
  
  mean_exp_q <- enquo(mean_exp)
  sd_exp_q <- enquo(sd_exp)
  mean_ctrl_q <- enquo(mean_ctrl)
  sd_ctrl_q <- enquo(sd_ctrl)
  
  ri_q <- enquo(ri)
  ni_q <- enquo(ni)
  ti_q <- enquo(ti)
  fi_q <- enquo(fi)
  
  yi_q <- enquo(yi)
  vi_q <- enquo(vi)
  mi_q <- enquo(mi)
  sdi_q <- enquo(sdi)
  
  moderator_q <- enquo(moderator)
  
  quosure_map <- list(
    event_exp = event_exp_q, n_exp = n_exp_q,
    event_ctrl = event_ctrl_q, n_ctrl = n_ctrl_q,
    mean_exp = mean_exp_q, sd_exp = sd_exp_q,
    mean_ctrl = mean_ctrl_q, sd_ctrl = sd_ctrl_q,
    ri = ri_q, ni = ni_q,
    ti = ti_q, fi = fi_q,
    yi = yi_q, vi = vi_q,
    mi = mi_q, sdi = sdi_q
  )
  
  # ===================================================
  # MEASURE DEFINITIONS
  # ===================================================
  
  binary_measures <- c("RR","OR","RD","PETO","AS")
  cont_measures <- c("MD","SMD","SMDH","SMD1","SMD1H","ROM")
  cor_measures <- c("COR","ZCOR")
  test_measures <- c("TSTAT","FSTAT")
  generic_measures <- c("GEN")
  single_measures <- c("MN")
  
  # ===================================================
  # REQUIRED ARGUMENTS
  # ===================================================
  
  if (measure %in% binary_measures) {
    required_args <- c("event_exp","n_exp","event_ctrl","n_ctrl")
    
  } else if (measure %in% cont_measures) {
    required_args <- c(
      "mean_exp","sd_exp","n_exp",
      "mean_ctrl","sd_ctrl","n_ctrl"
    )
    
  } else if (measure %in% cor_measures) {
    required_args <- c("ri","ni")
    
  } else if (measure == "TSTAT") {
    required_args <- c("ti","ni")
    
  } else if (measure == "FSTAT") {
    required_args <- c("fi","ni")
    
  } else if (measure %in% generic_measures) {
    required_args <- c("yi","vi")
    
  } else if (measure %in% single_measures) {
    required_args <- c("mi","sdi","ni")
    
  } else {
    stop("Unsupported measure: ", measure, call. = FALSE)
  }
  
  
  check_required(measure, required_args, quosure_map)
  
  # ===================================================
  # EFFECT SIZE CALCULATION
  # ===================================================
  
  if (measure %in% binary_measures) {
    
    yi_vi <- escalc(
      measure = measure,
      ai = data[[as_label(event_exp_q)]],
      n1i = data[[as_label(n_exp_q)]],
      ci = data[[as_label(event_ctrl_q)]],
      n2i = data[[as_label(n_ctrl_q)]],
      add = add,
      to = to,
      drop00 = drop00,
      data = data
    )
    
  } else if (measure %in% cont_measures) {
    
    yi_vi <- escalc(
      measure = measure,
      m1i = data[[as_label(mean_exp_q)]],
      sd1i = data[[as_label(sd_exp_q)]],
      n1i = data[[as_label(n_exp_q)]],
      m2i = data[[as_label(mean_ctrl_q)]],
      sd2i = data[[as_label(sd_ctrl_q)]],
      n2i = data[[as_label(n_ctrl_q)]],
      data = data
    )
    
  } else if (measure %in% cor_measures) {
    
    yi_vi <- escalc(
      measure = measure,
      ri = data[[as_label(ri_q)]],
      ni = data[[as_label(ni_q)]],
      data = data
    )
    
  } else if (measure == "TSTAT") {
    
    yi_vi <- escalc(
      measure = "TSTAT",
      ti = data[[as_label(ti_q)]],
      ni = data[[as_label(ni_q)]],
      data = data
    )
    
  } else if (measure == "FSTAT") {
    
    yi_vi <- escalc(
      measure = "FSTAT",
      fi = data[[as_label(fi_q)]],
      ni = data[[as_label(ni_q)]],
      data = data
    )
    
  } else if (measure == "GEN") {
    
    yi_vi <- escalc(
      measure = "GEN",
      yi = data[[as_label(yi_q)]],
      vi = data[[as_label(vi_q)]],
      data = data
    )
    
  } else if (measure == "MN") {
    
    yi_vi <- escalc(
      measure = "MN",
      mi = data[[as_label(mi_q)]],
      sdi = data[[as_label(sdi_q)]],
      ni = data[[as_label(ni_q)]],
      data = data
    )
  }
  
  # ===================================================
  # META-ANALYSIS MODEL
  # ===================================================
  
  if (!quo_is_null(moderator_q)) {
    yi_vi$moderator <- data[[as_label(moderator_q)]]
    res <- rma(yi, vi, mods = ~ moderator - 1, data = yi_vi, method = method)
  } else {
    res <- rma(yi, vi, data = yi_vi, method = method)
  }
  
  # ===================================================
  # AUTOMATIC TRANSFORM
  # ===================================================
  
  if (is.null(transform)) {
    auto <- auto_transform_for_measure(measure)
    transform <- auto$transform
    transform_name <- auto$name
  }
  
  results <- extract_meta_results(
    res,
    transform = transform,
    transform_name = transform_name,
    conf_level = conf_level
  )
  
  results$data <- yi_vi
  results$model <- res
  results$measure <- measure
  
  return(results)
}


# ==============================================================================
# RESULT EXTRACTION FUNCTIONS
# ==============================================================================

#' Extract Heterogeneity Statistics from RMA Model
#'
#' @description
#' Extracts heterogeneity statistics including tau², I², H², and Q test
#' from a fitted rma model object.
#'
#' @param x An rma model object from metafor::rma()
#'
#' @return A tibble with two columns:
#'   \itemize{
#'     \item Statistic: Name of the heterogeneity statistic
#'     \item Value: Numeric value (rounded to 4 decimal places)
#'   }
#'
#' @export
#'
#' @examples
#' library(metafor)
#' data(dat.bcg)
#' res <- rma(yi, vi, data = dat.bcg)
#' extract_heterogeneity(res)

extract_heterogeneity <- function(x) {
  
  stopifnot(inherits(x, "rma"))
  
  stats <- list(
    "tau²" = x$tau2,
    "tau" = if (!is.null(x$tau2)) sqrt(x$tau2) else NULL,
    "I² (%)" = x$I2,
    "H²" = x$H2,
    "Q" = x$QE,
    "Q df" = x$QE.df,
    "Q p-value" = x$QEp,
    "k" = x$k
  )
  
  # Remove NULL entries safely
  stats <- stats[!vapply(stats, is.null, logical(1))]
  
  tibble(
    Statistic = names(stats),
    Value = round(unlist(stats), 4)
  )
}


#' Extract Model Coefficients from RMA Model
#'
#' @description
#' Extracts model coefficients, standard errors, z-values, p-values,
#' and confidence intervals from a fitted rma model. Optionally applies
#' back-transformation to estimates.
#'
#' @param x An rma model object from metafor::rma()
#' @param transform Function to back-transform estimates (e.g., exp for log odds).
#'   If NULL, no transformation is applied.
#' @param transform_name Character; name for the transformed estimate column
#'   (default: "Transformed")
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item Term: Coefficient name
#'     \item Estimate: Raw estimate (log scale for OR/RR)
#'     \item SE: Standard error
#'     \item z: z-value
#'     \item p: p-value
#'     \item CI.lb: Lower confidence interval bound
#'     \item CI.ub: Upper confidence interval bound
#'     \item [transform_name]: Back-transformed estimate (if transform provided)
#'     \item CI.lb.trans: Back-transformed lower CI (if transform provided)
#'     \item CI.ub.trans: Back-transformed upper CI (if transform provided)
#'     \item Significance: Significance stars (*** p<0.001, ** p<0.01, * p<0.05, . p<0.1)
#'   }
#'
#' @export
#'
#' @examples
#' library(metafor)
#' data(dat.bcg)
#' res <- rma(yi, vi, data = dat.bcg)
#' 
#' # Without transformation
#' extract_model_results(res)
#' 
#' # With transformation to odds ratios
#' extract_model_results(res, transform = exp, transform_name = "Odds Ratio")

extract_model_results <- function(x, transform = NULL, transform_name = "Transformed") {
  
  stopifnot(inherits(x, "rma"))
  
  df <- tibble(
    Term = rownames(x$b),
    Estimate = as.numeric(x$b),
    SE = as.numeric(x$se),
    z = as.numeric(x$zval),
    p = as.numeric(x$pval),
    CI.lb = as.numeric(x$ci.lb),
    CI.ub = as.numeric(x$ci.ub)
  )
  
  if (!is.null(transform)) {
    df <- df %>%
      mutate(
        !!transform_name := transform(Estimate),
        CI.lb.trans = transform(CI.lb),
        CI.ub.trans = transform(CI.ub)
      )
  }
  
  df %>%
    mutate(
      Significance = case_when(
        p < 0.001 ~ "***",
        p < 0.01 ~ "**",
        p < 0.05 ~ "*",
        p < 0.1 ~ ".",
        TRUE ~ ""
      )
    )
}


#' Extract Complete Meta-Analysis Results
#'
#' @description
#' Extracts and formats complete results from a fitted rma model including
#' summary statistics, heterogeneity measures, model coefficients, and
#' moderator tests (if applicable).
#'
#' @param x An rma model object from metafor::rma()
#' @param transform Function to back-transform estimates (e.g., exp for log odds).
#'   If NULL, no transformation is applied.
#' @param transform_name Character; name for the transformed estimate column
#'   (default: "Transformed")
#' @param conf_level Numeric; confidence level for intervals (default: 0.95)
#'
#' @return A list with four components:
#'   \itemize{
#'     \item summary: Tibble with method, tau², I², k, confidence level, and model type
#'     \item heterogeneity: Tibble with heterogeneity statistics
#'     \item model_results: Tibble with model coefficients and CIs
#'     \item moderator_test: Tibble with QM test (NULL if no moderators)
#'   }
#'
#' @export
#'
#' @examples
#' library(metafor)
#' data(dat.bcg)
#' res <- rma(yi, vi, data = dat.bcg)
#' results <- extract_meta_results(res, transform = exp, transform_name = "Odds Ratio")
#' 
#' # Access components
#' results$summary
#' results$heterogeneity
#' results$model_results
#' results$moderator_test

extract_meta_results <- function(
    x,
    transform = NULL,
    transform_name = "Transformed",
    conf_level = 0.95
) {
  
  stopifnot(inherits(x, "rma"))
  
  list(
    summary = tibble(
      Method = x$method,
      Tau2 = round(x$tau2, 4),
      I2 = paste0(round(x$I2, 1), "%"),
      k = x$k,
      Confidence = paste0(conf_level * 100, "%"),
      Model = ifelse(ncol(x$X) > 1, "With moderator", "Intercept-only")
    ),
    heterogeneity = extract_heterogeneity(x),
    model_results = extract_model_results(
      x,
      transform = transform,
      transform_name = transform_name
    ),
    moderator_test =
      if (!is.null(x$QM)) tibble(
        QM = round(x$QM, 3),
        df = x$QMdf,
        p = ifelse(x$QMp < 0.001, "<0.001", round(x$QMp, 3))
      ) else NULL
  )
}
