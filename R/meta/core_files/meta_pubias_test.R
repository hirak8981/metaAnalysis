#' Tidy Report for Publication Bias Tests
#' Compact Summary for Publication Bias Tests
#'
#' @param model A fitted rma object from metafor
#' @param regtest_result Optional pre-computed regtest result (if NULL, will compute)
#' @param ranktest_result Optional pre-computed ranktest result (if NULL, will compute)
#' @param ... Additional arguments passed to regtest
#'
#' @return A data frame with compact test summary
#' @export
#'
#' @examples
#' library(metafor)
#' res <- rma(yi, vi, data = dat.bcg)
#' tidy_pubias_summary(res)

tidy_pubias_summary <- function(model = NULL,
                                regtest_result = NULL,
                                ranktest_result = NULL,
                                ...) {
  
  # Compute tests if not provided
  if (is.null(regtest_result) && !is.null(model)) {
    regtest_result <- metafor::regtest(model, ...)
  }
  if (is.null(ranktest_result) && !is.null(model)) {
    ranktest_result <- metafor::ranktest(model)
  }
  
  # Validate inputs
  if (is.null(regtest_result) || is.null(ranktest_result)) {
    stop("Either provide 'model' or both test results")
  }
  
  # Determine significance
  reg_sig <- regtest_result$pval <= 0.05
  rank_sig <- ranktest_result$pval <= 0.05
  
  # Determine overall conclusion with line breaks
  if (!reg_sig && !rank_sig) {
    conclusion_text <- "No Asymmetry Detected:<br>Both tests have p-values greater than 0.05."
  } else if (reg_sig && rank_sig) {
    conclusion_text <- "Potential Asymmetry Detected:<br>Both tests show significant results with p-values at or below 0.05,<br>indicating possible publication bias."
  } else {
    conclusion_text <- "Asymmetry Detected by One Test Only:<br>One test is significant while the other is not,<br>suggesting the need for careful interpretation of asymmetry detection."
  }
  
  # Create compact summary data frame with properly formatted column names
  summary_df <- data.frame(
    Test = c("Egger's Regression Test", "Begg's Rank Correlation Test"),
    Statistic = c(
      sprintf("z = %.3f", regtest_result$zval),
      sprintf("τ = %.3f", ranktest_result$tau)
    ),
    `P-value` = c(
      format.pval(regtest_result$pval, digits = 3, eps = 0.001),
      format.pval(ranktest_result$pval, digits = 3, eps = 0.001)
    ),
    `Estimate [95% CI]` = c(
      sprintf("%.3f [%.3f, %.3f]", 
              regtest_result$est, 
              regtest_result$ci.lb, 
              regtest_result$ci.ub),
      "—"
    ),
    Result = c(
      ifelse(reg_sig, "Asymmetry detected", "No asymmetry"),
      ifelse(rank_sig, "Correlation detected", "No correlation")
    ),
    Significant = c(reg_sig, rank_sig),
    Conclusion = conclusion_text,
    stringsAsFactors = FALSE,
    check.names = FALSE  # This preserves spaces and special characters in column names
  )
  
  return(summary_df)
}
