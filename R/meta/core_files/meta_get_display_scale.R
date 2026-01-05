# ==============================================================================
# Display Scale Helper for Meta-Analysis Effect Measures
# ==============================================================================

#' Get Display Scale Information for Meta-Analysis Effect Measures
#'
#' Determines appropriate axis labels, transformation functions, and null values
#' for displaying meta-analysis results based on the effect measure type. Handles
#' both log-scale measures (OR, RR, ROM) and their transformations to original scale.
#'
#' @param x A meta-analysis object (typically from metafor::rma() or meta package)
#'   containing a \code{measure} element specifying the effect size type.
#' @param x_trans_function Optional transformation function to apply to effect sizes
#'   for display purposes. Common examples: \code{exp} for ratio measures,
#'   \code{tanh} for Fisher's z correlation. Default is NULL (no transformation).
#'
#' @return A list containing display scale information:
#'   \item{trans}{Transformation function to apply (NULL or function)}
#'   \item{xlab}{Character string for x-axis label describing the effect measure}
#'   \item{ci_label}{Abbreviated label for confidence intervals (e.g., "OR", "SMD")}
#'   \item{null}{Numeric value representing no effect on the analysis scale}
#'
#' @details
#' The function handles different classes of effect measures:
#'
#' \strong{Ratio Measures (log scale):}
#' \itemize{
#'   \item OR - Odds Ratio (log scale → exp transformation)
#'   \item RR - Risk Ratio (log scale → exp transformation)
#'   \item ROM - Ratio of Means (log scale → exp transformation)
#'   \item PETO - Peto Odds Ratio (log scale → exp transformation)
#' }
#'
#' \strong{Correlation Measures:}
#' \itemize{
#'   \item ZCOR - Fisher's z-transformed correlation (z scale → tanh transformation)
#'   \item COR - Correlation coefficient (no transformation needed)
#' }
#'
#' \strong{Continuous Outcomes:}
#' \itemize{
#'   \item SMD, SMDH, SMD1, SMD2 - Standardized Mean Difference variants
#'   \item MD - Mean Difference
#'   \item RD - Risk Difference
#' }
#'
#' When a transformation function is provided and matches the expected transformation
#' for the measure, labels are adjusted to show the transformed scale. Otherwise,
#' labels indicate the original analysis scale.
#'
#' The null value is 0 for all supported measures on their respective analysis scales
#' (e.g., log(1) = 0 for ratios, 0 for differences).
#'
#' @examples
#' \dontrun{
#' # For odds ratio meta-analysis on log scale
#' meta_result <- rma(measure = "OR", ...)
#' scale_info <- get_display_scale(meta_result)
#' # Returns: xlab = "Effect Size: log(Odds Ratio)", null = 0
#'
#' # For odds ratio with exp transformation (display as OR)
#' scale_info <- get_display_scale(meta_result, x_trans_function = exp)
#' # Returns: xlab = "Effect Size: Odds Ratio", trans = exp, null = 0
#'
#' # For standardized mean difference
#' meta_result <- rma(measure = "SMD", ...)
#' scale_info <- get_display_scale(meta_result)
#' # Returns: xlab = "Effect Size: Standardized Mean Difference", null = 0
#'
#' # For Fisher's z correlation with back-transformation
#' meta_result <- rma(measure = "ZCOR", ...)
#' scale_info <- get_display_scale(meta_result, x_trans_function = tanh)
#' # Returns: xlab = "Effect Size: Correlation", trans = tanh, null = 0
#' }
#'
#' @seealso \code{\link[metafor]{escalc}} for effect size calculation
#'
#' @export
get_display_scale <- function(x, x_trans_function = NULL) {
  
  # ---------------------------
  # Safe defaults
  # ---------------------------
  out <- list(
    trans = NULL,
    xlab = "Effect Size",
    ci_label = "Effect",
    null = 0
  )
  
  # If measure metadata is missing, return defaults
  if (is.null(x$measure)) {
    return(out)
  }
  
  measure <- toupper(x$measure)
  
  # Is the display being transformed?
  has_transform <- is.function(x_trans_function)
  
  # ---------------------------
  # Ratio measures (log scale)
  # ---------------------------
  if (measure == "OR") {
    out$null <- 0  # log(OR)
    if (has_transform && identical(x_trans_function, exp)) {
      out$trans <- exp
      out$xlab <- "Effect Size: Odds Ratio"
      out$ci_label <- "OR"
    } else {
      out$xlab <- "Effect Size: log(Odds Ratio)"
      out$ci_label <- "log(OR)"
    }
    
  } else if (measure == "RR") {
    out$null <- 0  # log(RR)
    if (has_transform && identical(x_trans_function, exp)) {
      out$trans <- exp
      out$xlab <- "Effect Size: Risk Ratio"
      out$ci_label <- "RR"
    } else {
      out$xlab <- "Effect Size: log(Risk Ratio)"
      out$ci_label <- "log(RR)"
    }
    
  } else if (measure == "ROM") {
    out$null <- 0  # log(ROM)
    if (has_transform && identical(x_trans_function, exp)) {
      out$trans <- exp
      out$xlab <- "Effect Size: Ratio of Means"
      out$ci_label <- "ROM"
    } else {
      out$xlab <- "Effect Size: log(Ratio of Means)"
      out$ci_label <- "log(ROM)"
    }
    
  } else if (measure == "PETO") {
    out$null <- 0  # log(OR, Peto)
    if (has_transform && identical(x_trans_function, exp)) {
      out$trans <- exp
      out$xlab <- "Effect Size: Odds Ratio (Peto)"
      out$ci_label <- "OR (Peto)"
    } else {
      out$xlab <- "Effect Size: log(Odds Ratio, Peto)"
      out$ci_label <- "log(OR, Peto)"
    }
    
    # ---------------------------
    # Correlation measures
    # ---------------------------
  } else if (measure == "ZCOR") {
    out$null <- 0  # Fisher z
    if (has_transform && identical(x_trans_function, tanh)) {
      out$trans <- tanh
      out$xlab <- "Effect Size: Correlation"
      out$ci_label <- "r"
    } else {
      out$xlab <- "Effect Size: Fisher's z"
      out$ci_label <- "z"
    }
    
  } else if (measure == "COR") {
    out$null <- 0
    out$xlab <- "Effect Size: Correlation"
    out$ci_label <- "r"
    
    # ---------------------------
    # Continuous outcomes
    # ---------------------------
  } else if (measure == "SMD") {
    out$null <- 0
    out$xlab <- "Effect Size: Standardized Mean Difference"
    out$ci_label <- "SMD"
    
  } else if (measure == "SMDH") {
    out$null <- 0
    out$xlab <- "Effect Size: Hedge's g"
    out$ci_label <- "SMDH"
    
  } else if (measure == "SMD1") {
    out$null <- 0
    out$xlab <- "SMD1 - SMD using SD of Second Group"
    out$ci_label <- "SMD1"
    
  } else if (measure == "SMD1H") {
    out$null <- 0
    out$xlab <- "SMD1H - SMD1 with Heteroscedastic Variances"
    out$ci_label <- "SMD1H"
    
  } else if (measure == "MD") {
    out$null <- 0
    out$xlab <- "Effect Size: Mean Difference"
    out$ci_label <- "MD"
    
  } else if (measure == "RD") {
    out$null <- 0
    out$xlab <- "Effect Size: Risk Difference"
    out$ci_label <- "RD"
  }
  
  out
}
