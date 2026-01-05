# ==============================================================================
# Studentized Residuals Plot for Meta-Analysis
# ==============================================================================

#' Plot Studentized Residuals from Influence Analysis
#'
#' Creates a bubble plot of studentized residuals from meta-analysis influence
#' diagnostics, highlighting studies that exceed Bonferroni-corrected thresholds.
#' Bubble size indicates magnitude of residual; color indicates whether threshold
#' is exceeded.
#'
#' @param influence_results A list object from \code{\link{perform_influence_analysis}}
#'   containing influence statistics and thresholds.
#' @param color_below Character string, color for points below threshold.
#'   Default is "#B4D2EE" (light blue).
#' @param color_above Character string, color for points above threshold.
#'   Default is "#C83737" (red).
#' @param base_size Numeric, base font size for plot. Default is 11.
#' @param title Character string for plot title. Default is "Studentized Residuals".
#' @param show_threshold_label Logical, whether to display threshold value on plot.
#'   Default is TRUE.
#' @param threshold_line_color Character string, color for threshold lines.
#'   Default is "#2C3E50" (dark blue-gray).
#' @param subtitle_color Character string, color for subtitle text.
#'   Default is "#5D6D7E" (gray).
#' @param custom_theme Optional ggplot2 theme object to override default styling.
#'   If provided, \code{base_size}, \code{threshold_line_color}, and other styling
#'   parameters are applied on top of this theme.
#'
#' @return A ggplot2 object
#'
#' @details
#' \strong{Studentized Residuals:}
#' Studentized residuals (also called externally studentized or deleted residuals)
#' are standardized residuals where each study's residual is standardized using
#' an estimate of the residual standard error that excludes that study. They follow
#' a t-distribution and are more sensitive to outliers than ordinary residuals.
#'
#' \strong{Threshold Calculation:}
#' The plot displays Bonferroni-corrected thresholds at ±qnorm(0.05/(2×k)) where
#' k is the number of studies. This conservative threshold accounts for multiple
#' testing and indicates studies with residuals significantly larger than expected.
#'
#' \strong{Visual Features:}
#' \itemize{
#'   \item Studies exceeding thresholds shown in red (customizable)
#'   \item Bubble size proportional to absolute residual magnitude
#'   \item Reference line at zero (gray)
#'   \item Threshold lines (dashed) at ±critical value
#'   \item X-axis shows study numbers for identification
#' }
#'
#' Studies with large studentized residuals may indicate:
#' \itemize{
#'   \item Outliers or influential observations
#'   \item Heterogeneity not captured by the model
#'   \item Data errors or unique study characteristics
#' }
#'
#' @examples
#' \dontrun{
#' # Perform influence analysis first
#' influence_results <- perform_influence_analysis(meta_results, study_labels = study)
#'
#' # Basic plot
#' p1 <- plot_studentized_residuals(influence_results)
#' print(p1)
#'
#' # Custom colors
#' p2 <- plot_studentized_residuals(
#'   influence_results,
#'   color_below = "#A8DADC",
#'   color_above = "#E63946",
#'   threshold_line_color = "#1D3557"
#' )
#'
#' # Larger plot with no threshold label
#' p3 <- plot_studentized_residuals(
#'   influence_results,
#'   base_size = 14,
#'   show_threshold_label = FALSE,
#'   title = "Outlier Detection: Studentized Residuals"
#' )
#'
#' # With custom theme
#' my_theme <- theme_bw() + theme(panel.grid = element_blank())
#' p4 <- plot_studentized_residuals(influence_results, custom_theme = my_theme)
#'
#' # Save plot
#' ggsave("studentized_residuals.png", p1, width = 10, height = 6, dpi = 300)
#' }
#'
#' @seealso
#' \code{\link{perform_influence_analysis}},
#' \code{\link{plot_cooks_distance}},
#' \code{\link{plot_leave_one_out}}
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_hline scale_color_manual
#'   scale_size_continuous scale_x_continuous labs theme_minimal theme
#'   element_text element_blank annotate
#' @importFrom dplyr mutate row_number filter
#'
#' @export
plot_studentized_residuals <- function(influence_results,
                                       color_below = "#B4D2EE",
                                       color_above = "#C83737",
                                       base_size = 11,
                                       title = "Studentized Residuals",
                                       show_threshold_label = TRUE,
                                       threshold_line_color = "#2C3E50",
                                       subtitle_color = "#5D6D7E",
                                       custom_theme = NULL) {
  
  # Extract data
  result_tibble <- influence_results$influence_stats
  crit <- influence_results$studentized_threshold
  
  # Create numeric study labels and color groups
  result_tibble <- result_tibble %>%
    dplyr::mutate(
      study_num = dplyr::row_number(),
      color_group = ifelse(abs(rstudent) > crit, "Above", "Below"),
      abs_rstudent = abs(rstudent)
    )
  
  # Create base plot
  p <- ggplot2::ggplot(result_tibble, ggplot2::aes(x = study_num, y = rstudent, 
                                                   color = color_group, 
                                                   size = abs_rstudent)) +
    ggplot2::geom_point(alpha = 0.7) +
    
    # Threshold lines
    ggplot2::geom_hline(yintercept = c(-crit, crit), linetype = "dashed",
                        color = threshold_line_color, linewidth = 0.8) +
    
    # Zero reference line
    ggplot2::geom_hline(yintercept = 0, color = "gray50", linewidth = 0.5) +
    
    # Scales
    ggplot2::scale_color_manual(values = c("Below" = color_below, "Above" = color_above), 
                                guide = "none") +
    ggplot2::scale_size_continuous(range = c(3, 10), guide = "none") +
    ggplot2::scale_x_continuous(breaks = result_tibble$study_num, 
                                labels = result_tibble$study_num) +
    
    # Labels
    ggplot2::labs(
      title = title,
      subtitle = "Dashed lines represent thresholds using Bonferroni-corrected critical values",
      x = "Study",
      y = "Studentized Residual"
    )
  
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
                                              face = "italic", size = base_size - 1),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )
  }
  
  # Add threshold label if requested
  if (show_threshold_label) {
    y_max <- max(abs(result_tibble$rstudent))
    p <- p +
      ggplot2::annotate(
        "text",
        x = max(result_tibble$study_num),
        y = crit + y_max * 0.05,
        label = paste("Threshold: ±", round(crit, 2)),
        hjust = 1,
        vjust = -0.5,
        size = 3.5,
        color = threshold_line_color,
        fontface = "italic"
      )
  }
  
  return(p)
}
