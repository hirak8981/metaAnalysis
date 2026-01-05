# ==============================================================================
# Leave-One-Out Plot for Meta-Analysis
# ==============================================================================

#' Plot Leave-One-Out Sensitivity Analysis Results
#'
#' Creates a visualization of leave-one-out (LOO) sensitivity analysis, showing
#' how the meta-analytic estimate changes when each study is removed. Displays
#' point estimates with optional confidence intervals and a reference line for
#' the full model estimate.
#'
#' @param influence_results A list object from \code{\link{perform_influence_analysis}}
#'   containing LOO results and the original model.
#' @param base_size Numeric, base font size for plot. Default is 11.
#' @param title Character string for plot title. Default is "Leave-One-Out Analysis".
#' @param point_color Character string, color for LOO estimate points.
#'   Default is "#3498DB" (blue).
#' @param point_size Numeric, size of points. Default is 3.
#' @param show_ci Logical, whether to display confidence intervals as error bars.
#'   Default is TRUE.
#' @param ci_color Character string, color for confidence interval error bars.
#'   Default is "#34495E" (dark gray).
#' @param ci_alpha Numeric between 0 and 1, transparency of error bars.
#'   Default is 0.6.
#' @param ci_width Numeric, width of error bar caps. Default is 0.2.
#' @param refline_color Character string, color for reference line showing full
#'   model estimate. Default is "#E74C3C" (red).
#' @param subtitle_color Character string, color for subtitle text.
#'   Default is "#E74C3C" (red) to match reference line.
#' @param refline_label Character string, description for reference line in subtitle.
#'   Default is "Full Model Estimate".
#' @param custom_theme Optional ggplot2 theme object to override default styling.
#'
#' @return A ggplot2 object
#'
#' @details
#' \strong{Leave-One-Out Visualization:}
#' The plot shows the meta-analytic estimate recalculated with each study omitted
#' in turn. The x-axis represents the study omitted (by number), and the y-axis
#' shows the resulting effect estimate.
#'
#' \strong{Visual Elements:}
#' \itemize{
#'   \item \strong{Points}: Each point represents the LOO estimate
#'   \item \strong{Error bars}: Show confidence intervals (if \code{show_ci = TRUE})
#'   \item \strong{Reference line}: Dashed horizontal line at full model estimate
#'   \item \strong{Subtitle}: Displays full model estimate value
#' }
#'
#' \strong{Interpretation:}
#' \itemize{
#'   \item Points close to reference line: Study removal has minimal impact
#'   \item Points far from reference line: Study substantially influences results
#'   \item All LOO estimates on same side of null: Robust finding
#'   \item LOO estimates crossing null while full estimate doesn't: Sensitivity concern
#' }
#'
#' This plot complements influence diagnostics (Cook's distance, studentized residuals)
#' by directly showing the impact of each study on the pooled estimate.
#'
#' @examples
#' \dontrun{
#' # Perform LOO analysis first
#' loo_results <- perform_loo_analysis(meta_results, study_labels = study)
#'
#' # Basic LOO plot
#' p1 <- plot_leave_one_out(loo_results)
#' print(p1)
#'
#' # Custom colors and styling
#' p2 <- plot_leave_one_out(
#'   loo_results,
#'   point_color = "#2ECC71",
#'   ci_color = "#95A5A6",
#'   refline_color = "#E67E22",
#'   subtitle_color = "#E67E22"
#' )
#'
#' # Without confidence intervals
#' p3 <- plot_leave_one_out(
#'   loo_results,
#'   show_ci = FALSE,
#'   point_size = 4,
#'   title = "Sensitivity Analysis: Study Removal"
#' )
#'
#' # Larger plot with custom CI styling
#' p4 <- plot_leave_one_out(
#'   loo_results,
#'   base_size = 14,
#'   ci_width = 0.3,
#'   ci_alpha = 0.8,
#'   refline_label = "Overall Effect"
#' )
#'
#' # With custom theme
#' my_theme <- theme_bw() +
#'   theme(panel.border = element_rect(color = "black", linewidth = 1))
#' p5 <- plot_leave_one_out(loo_results, custom_theme = my_theme)
#'
#' # Save plot
#' ggsave("loo_analysis.png", p1, width = 10, height = 6, dpi = 300)
#'
#' # Identify most influential study
#' loo_results$loo_results %>%
#'   mutate(deviation = abs(Estimate - loo_results$overall_estimate)) %>%
#'   arrange(desc(deviation)) %>%
#'   slice(1)
#' }
#'
#' @seealso
#' \code{\link{perform_loo_analysis}},
#' \code{\link{perform_influence_analysis}},
#' \code{\link{plot_cooks_distance}},
#' \code{\link{plot_studentized_residuals}}
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_point geom_errorbar
#'   scale_x_continuous labs theme_minimal theme element_text element_blank
#' @importFrom metafor leave1out
#' @importFrom stats coef
#'
#' @export
plot_leave_one_out <- function(influence_results,
                               base_size = 12,
                               title = "Leave-One-Out Analysis",
                               point_color = "#3498DB",
                               point_size = 3,
                               show_ci = TRUE,
                               ci_color = "#34495E",
                               ci_alpha = 0.6,
                               ci_width = 0.2,
                               refline_color = "#E74C3C",
                               subtitle_color = "#E74C3C",
                               refline_label = "Full Model Estimate",
                               custom_theme = NULL) {
  
  if (!is.null(influence_results$loo_precomputed)) {
    message("✓ Using pre-computed leave-one-out results")
    loo_results <- influence_results$loo_precomputed
  } else {
    message("⚠ Computing leave-one-out now (fallback)...")
    loo_results <- metafor::leave1out(influence_results$model)
  }

  # Get study labels
  study_labels <- influence_results$influence_stats$Study
  
  # Create data frame for plotting
  loo_df <- data.frame(
    Study = study_labels,
    estimate = loo_results$estimate,
    ci.lb = loo_results$ci.lb,
    ci.ub = loo_results$ci.ub,
    study_num = seq_along(study_labels)
  )
  
  # Get full model estimate for reference line
  full_estimate <- stats::coef(influence_results$model)
  
  # Create base plot
  p <- ggplot2::ggplot(loo_df, ggplot2::aes(x = study_num, y = estimate)) +
    
    # Reference line for full model estimate
    ggplot2::geom_hline(
      yintercept = full_estimate,
      linetype = "dashed",
      color = refline_color,
      linewidth = 1
    ) +
    
    # LOO estimates
    ggplot2::geom_point(color = point_color, size = point_size) +
    
    # Axis and labels
    ggplot2::scale_x_continuous(breaks = loo_df$study_num, 
                                labels = loo_df$study_num) +
    ggplot2::labs(
      title = title,
      subtitle = paste0("Reference line: ", refline_label, " = ", 
                        round(full_estimate, 3)),
      x = "Study Omitted",
      y = "Effect Estimate (LOO)"
    )
  
  # Add confidence intervals if requested
  if (show_ci) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci.lb, ymax = ci.ub),
      width = ci_width,
      color = ci_color,
      alpha = ci_alpha
    )
  }
  
  # Apply custom theme or default
  if (!is.null(custom_theme)) {
    p <- p + custom_theme +
      ggplot2::theme(plot.title = element_text(face = "bold", hjust = 0.5),
                     plot.subtitle = ggplot2::element_text(hjust = 0.5, 
                                                           color = subtitle_color,
                                                           face = "italic", 
                                                           size = base_size - 1))
  } else {
    p <- p +
      ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, color = subtitle_color,
                                              size = base_size - 1, face = "italic"),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )
  }
  
  return(p)
}
