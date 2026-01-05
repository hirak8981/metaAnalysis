# ==============================================================================
# Cook's Distance Plot for Meta-Analysis
# ==============================================================================

#' Plot Cook's Distance from Influence Analysis
#'
#' Creates a bubble plot of Cook's distance values from meta-analysis influence
#' diagnostics, highlighting studies that exceed the threshold for influential
#' observations. Bubble size indicates magnitude of Cook's distance; color indicates
#' whether threshold is exceeded.
#'
#' @param influence_results A list object from \code{\link{perform_influence_analysis}}
#'   containing influence statistics and Cook's distance threshold.
#' @param color_below Character string, color for points below threshold.
#'   Default is "#B4D2EE" (light blue).
#' @param color_above Character string, color for points above threshold.
#'   Default is "#C83737" (red).
#' @param base_size Numeric, base font size for plot. Default is 11.
#' @param title Character string for plot title. Default is "Cook's Distance".
#' @param show_threshold_label Logical, whether to display threshold value on plot.
#'   Default is TRUE.
#' @param threshold_line_color Character string, color for threshold line.
#'   Default is "#2C3E50" (dark blue-gray).
#' @param subtitle_color Character string, color for subtitle text.
#'   Default is "#5D6D7E" (gray).
#' @param custom_theme Optional ggplot2 theme object to override default styling.
#'   If provided, subtitle styling is applied on top of this theme.
#'
#' @return A ggplot2 object
#'
#' @details
#' \strong{Cook's Distance:}
#' Cook's distance (Cook's D) measures the overall influence of each study on the
#' fitted values in meta-analysis. It combines information about the size of the
#' residual and the leverage of the study. Large Cook's D values indicate studies
#' that have substantial impact on the meta-analytic estimate when removed.
#'
#' \strong{Threshold Calculation:}
#' Uses a robust threshold of median + 6×IQR (interquartile range) rather than
#' the conventional 4/n threshold. This approach is more appropriate for meta-analysis
#' where sample sizes are often small and reduces false positive identifications.
#'
#' \strong{Visual Features:}
#' \itemize{
#'   \item Studies exceeding threshold shown in red (customizable)
#'   \item Bubble size proportional to Cook's distance
#'   \item Dashed horizontal line at threshold
#'   \item X-axis shows study numbers for identification
#' }
#'
#' Studies with high Cook's distance should be examined for:
#' \itemize{
#'   \item Unusual study characteristics or design
#'   \item Data errors or extreme effect sizes
#'   \item Potential impact on conclusions if removed
#' }
#'
#' @examples
#' \dontrun{
#' # Perform influence analysis first
#' influence_results <- perform_influence_analysis(meta_results, study_labels = study)
#'
#' # Basic plot
#' p1 <- plot_cooks_distance(influence_results)
#' print(p1)
#'
#' # Custom colors matching publication style
#' p2 <- plot_cooks_distance(
#'   influence_results,
#'   color_below = "#7FB3D5",
#'   color_above = "#C0392B",
#'   threshold_line_color = "#34495E"
#' )
#'
#' # Larger plot without threshold label
#' p3 <- plot_cooks_distance(
#'   influence_results,
#'   base_size = 14,
#'   show_threshold_label = FALSE,
#'   title = "Influence Diagnostics: Cook's Distance"
#' )
#'
#' # With custom theme
#' my_theme <- theme_classic() +
#'   theme(axis.line = element_line(color = "black", linewidth = 0.5))
#' p4 <- plot_cooks_distance(influence_results, custom_theme = my_theme)
#'
#' # Save plot
#' ggsave("cooks_distance.png", p1, width = 10, height = 6, dpi = 300)
#'
#' # Identify influential studies
#' influence_results$influential_studies %>%
#'   filter(Influential_Cook) %>%
#'   select(Study, cook.d)
#' }
#'
#' @references
#' Cook, R. D. (1977). Detection of influential observation in linear regression.
#' \emph{Technometrics, 19}(1), 15-18.
#'
#' @seealso
#' \code{\link{perform_influence_analysis}},
#' \code{\link{plot_studentized_residuals}},
#' \code{\link{plot_leave_one_out}}
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_hline scale_color_manual
#'   scale_size_continuous scale_x_continuous labs theme_minimal theme
#'   element_text element_blank annotate
#' @importFrom dplyr mutate row_number
#'
#' @export
plot_cooks_distance <- function(influence_results,
                                color_below = "#B4D2EE",
                                color_above = "#C83737",
                                base_size = 12,
                                title = "Cook's Distance",
                                show_threshold_label = TRUE,
                                threshold_line_color = "#2C3E50",
                                subtitle_color = "#5D6D7E",
                                custom_theme = NULL) {
  
  # Extract data
  result_tibble <- influence_results$influence_stats
  threshold <- influence_results$cook_threshold
  
  # Create numeric study labels and color groups
  result_tibble <- result_tibble %>%
    dplyr::mutate(
      study_num = dplyr::row_number(),
      color_group = ifelse(cook.d > threshold, "Above", "Below")
    )
  
  # Create base plot
  p <- ggplot2::ggplot(result_tibble, ggplot2::aes(x = study_num, y = cook.d, 
                                                   color = color_group, 
                                                   size = cook.d)) +
    ggplot2::geom_point(alpha = 0.7) +
    
    # Threshold line
    ggplot2::geom_hline(yintercept = threshold, linetype = "dashed",
                        color = threshold_line_color, linewidth = 0.8) +
    
    # Scales
    ggplot2::scale_color_manual(values = c("Below" = color_below, "Above" = color_above), 
                                guide = "none") +
    ggplot2::scale_size_continuous(range = c(3, 10), guide = "none") +
    ggplot2::scale_x_continuous(breaks = result_tibble$study_num, 
                                labels = result_tibble$study_num) +
    
    # Labels
    ggplot2::labs(
      title = title,
      subtitle = "Dashed line represents threshold using median + 6×IQR criterion",
      x = "Study",
      y = "Cook's Distance"
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
    p <- p + ggplot2::annotate(
      "text",
      x = max(result_tibble$study_num),
      y = threshold + max(result_tibble$cook.d) * 0.05,
      label = paste("Threshold:", round(threshold, 3)),
      hjust = 1,
      vjust = -0.5,
      size = 3.5,
      color = threshold_line_color,
      fontface = "italic"
    )
  }
  
  return(p)
}
