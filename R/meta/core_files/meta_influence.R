# ==============================================================================
# Influence Analysis for Meta-Analysis
# ==============================================================================

#' Perform Influence Analysis on Meta-Analysis Model
#'
#' Conducts comprehensive influence diagnostics for a meta-analysis model to identify
#' studies that have disproportionate impact on overall results. Calculates Cook's
#' distance, studentized residuals, leverage, and DFFITS for each study.
#'
#' @param results A list object from \code{perform_meta_analysis()} containing
#'   \code{model} (rma object) and \code{data} (original data frame). Alternatively,
#'   can directly accept an rma object from metafor package.
#' @param study_labels Unquoted column name from the original data containing study
#'   identifiers. Uses tidy evaluation. Default is \code{study}. If studies are
#'   not labeled, numeric labels "Study 1", "Study 2", etc. are generated.
#'
#' @return A list containing influence diagnostic results:
#'   \item{influence_object}{Raw influence object from metafor::influence()}
#'   \item{influence_stats}{Tibble with all influence statistics for each study:}
#'     \itemize{
#'       \item Study - Study identifier
#'       \item cook.d - Cook's distance
#'       \item rstudent - Studentized residual
#'       \item hat - Leverage (diagonal of hat matrix)
#'       \item dffits - DFFITS statistic
#'       \item row_id - Row number
#'       \item Influential_Cook - Logical, exceeds Cook's threshold
#'       \item Influential_Studentized - Logical, exceeds studentized threshold
#'       \item Influential_Overall - Logical, influential by any criterion
#'     }
#'   \item{influential_studies}{Filtered tibble containing only influential studies}
#'   \item{n_influential}{Integer, number of influential studies detected}
#'   \item{cook_threshold}{Numeric, Cook's distance threshold (median + 6×IQR)}
#'   \item{studentized_threshold}{Numeric, studentized residual threshold (Bonferroni-corrected)}
#'   \item{model}{Original rma model object}
#'
#' @details
#' \strong{Influence Diagnostics Calculated:}
#' \itemize{
#'   \item \strong{Cook's Distance}: Measures overall influence of each study on
#'     fitted values. Threshold: median + 6 × IQR (robust to outliers).
#'   \item \strong{Studentized Residuals}: Standardized residuals accounting for
#'     leverage. Threshold: Bonferroni-corrected critical value from normal distribution.
#'   \item \strong{Leverage (hat)}: Measures potential influence based on study weight
#'     and predictor values. Values closer to 1 indicate higher leverage.
#'   \item \strong{DFFITS}: Combined measure of residual size and leverage, indicating
#'     influence on fitted value when study is removed.
#' }
#'
#' \strong{Threshold Calculations:}
#' \itemize{
#'   \item Cook's distance: Uses robust criterion median + 6×IQR instead of
#'     conventional 4/n to reduce false positives in small meta-analyses.
#'   \item Studentized residuals: Uses qnorm(0.05 / (2×k)) where k = number of studies,
#'     applying Bonferroni correction for multiple comparisons.
#' }
#'
#' Studies are flagged as influential if they exceed either Cook's distance threshold
#' OR studentized residual threshold.
#'
#' @examples
#' \dontrun{
#' # Basic usage with meta-analysis results
#' library(metafor)
#' library(dplyr)
#'
#' # Assuming you have meta-analysis results
#' meta_results <- perform_meta_analysis2(data, yi, vi, study)
#'
#' # Perform influence analysis
#' influence_results <- perform_influence_analysis2(meta_results, study_labels = study)
#'
#' # View summary
#' print(influence_results$n_influential)
#' print(influence_results$influential_studies)
#'
#' # Access thresholds
#' cat("Cook's threshold:", influence_results$cook_threshold, "\n")
#' cat("Studentized threshold:", influence_results$studentized_threshold, "\n")
#'
#' # View all studies with influence stats
#' View(influence_results$influence_stats)
#'
#' # Identify studies that are influential by Cook's distance only
#' influence_results$influence_stats %>%
#'   filter(Influential_Cook, !Influential_Studentized)
#'
#' # Direct usage with rma object
#' rma_model <- rma(yi, vi, data = meta_data)
#' results_list <- list(model = rma_model, data = meta_data)
#' influence_results <- perform_influence_analysis(results_list, study_labels = author)
#' }
#'
#' @references
#' Viechtbauer, W., & Cheung, M. W. L. (2010). Outlier and influence diagnostics
#' for meta-analysis. \emph{Research Synthesis Methods, 1}(2), 112-125.
#'
#' @seealso
#' \code{\link[metafor]{influence.rma.uni}} for the underlying influence calculation,
#' \code{\link{plot_cooks_distance}}, \code{\link{plot_studentized_residuals}}
#'
#' @importFrom metafor influence
#' @importFrom dplyr mutate bind_cols filter select as_tibble
#' @importFrom purrr pluck
#' @importFrom rlang enquo quo_is_null eval_tidy
#' @importFrom stats qnorm median IQR
#'
#' @export
perform_influence_analysis <- function(results, study_labels = study) {
  
  # Allow either:
  # 1) results = list(model = rma_object, data = data_frame)
  # 2) results = rma object directly
  # 3) results = list from perform_meta_analysis2 (has $model and $data)
  
  if (inherits(results, "rma")) {
    # Case 2: direct rma object
    x    <- results
    data <- NULL
    
  } else if (is.list(results) && !is.null(results$model)) {
    # Case 1 or 3: list with $model and (optionally) $data
    x    <- results$model
    data <- results$data
    
  } else {
    stop("Input must be results from perform_meta_analysis2() or an rma object")
  }
  
  if (!inherits(x, "rma")) {
    stop("Input must contain a valid 'rma' object in $model or be an rma object directly")
  }
  
  # Calculate influence diagnostics
  data_influence <- influence(x)
  
  # Extract study names using tidy eval
  study_labels_quo <- rlang::enquo(study_labels)
  
  if (!is.null(data) && !rlang::quo_is_null(study_labels_quo)) {
    study_names <- rlang::eval_tidy(study_labels_quo, data = data)
  } else {
    study_names <- paste("Study", 1:x$k)
  }
  
  # Create result tibble
  result_tibble <- data_influence %>%
    purrr::pluck("inf") %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(Study = study_names) %>%
    dplyr::bind_cols(is_infl = data_influence %>% purrr::pluck("is.infl")) %>%
    dplyr::mutate(row_id = dplyr::row_number())
  
  # Calculate thresholds
  k <- nrow(result_tibble)
  
  # Cook's distance threshold: median + 6 * IQR
  median_cooks <- stats::median(result_tibble$cook.d)
  iqr_cooks <- stats::IQR(result_tibble$cook.d)
  cook_threshold <- median_cooks + 6 * iqr_cooks
  
  # Studentized residuals threshold: Normal distribution with Bonferroni correction
  studentized_threshold <- stats::qnorm(0.05 / (2 * k), lower.tail = FALSE)
  
  # Identify influential studies
  result_tibble <- result_tibble %>%
    dplyr::mutate(
      Influential_Cook = cook.d > cook_threshold,
      Influential_Studentized = abs(rstudent) > studentized_threshold,
      Influential_Overall = Influential_Cook | Influential_Studentized
    )
  
  # Summary of influential studies
  influential_studies <- result_tibble %>%
    dplyr::filter(Influential_Overall) %>%
    dplyr::select(Study, cook.d, rstudent, hat, dffits,
                  Influential_Cook, Influential_Studentized)
  
  return(list(
    influence_object = data_influence,
    influence_stats = result_tibble,
    influential_studies = influential_studies,
    n_influential = nrow(influential_studies),
    cook_threshold = cook_threshold,
    studentized_threshold = studentized_threshold,
    model = x
  ))
}
