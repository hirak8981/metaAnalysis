#' Create Dual Network Plots with Shared Legend
#' 
#' @description
#' Creates two network plots stacked vertically:
#' - Top: Colored by treatment class
#' - Bottom: Colored by individual treatments
#' Both plots share a single combined legend on the right.
#' 
#' @param nma_result Result object from run_nma() containing nma object and treatment_class_map
#' @param treatment_colors Optional named vector of colors for individual treatments. 
#'   If provided, overrides color_palette. Can be from create_treatment_colors() or custom.
#' @param color_palette Palette for individual treatment colors. 
#'   Used only if treatment_colors is NULL. Default is "colorblind".
#' @param class_palette Named vector of colors for treatment classes. If NULL, auto-generated.
#' @param edge_color Color for network edges. Default is "grey60".
#' @param node_border_color Color for node borders. Default is "#333333".
#' @param node_size_range Numeric vector of length 2 for min/max node sizes. Default is c(6, 14).
#' @param edge_width_range Numeric vector of length 2 for min/max edge widths. Default is c(0.5, 3).
#' @param label_size Size of treatment labels. Default is 4.
#' @param title_class Title for class-based plot. Default is "Network by Treatment Class".
#' @param title_treatment Title for treatment-based plot. Default is "Network by Treatment".
#' @param overall_title Overall title for combined plot. Default is NULL.
#' 
#' @return A patchwork object with two network plots and shared legend
#' 
#' @examples
#' \dontrun{
#' # Example 1: Using auto-generated palette
#' result <- run_nma(dat_continuous, "continuous", "MD", "Placebo")
#' dual_plot <- plot_dual_networks(result, color_palette = "viridis")
#' 
#' # Example 2: Using create_treatment_colors()
#' tx_colors <- create_treatment_colors(result$nma, palette = "plasma")
#' dual_plot <- plot_dual_networks(result, treatment_colors = tx_colors)
#' 
#' # Example 3: Using custom colors
#' custom_colors <- c(
#'   "Drug A" = "#e41a1c",
#'   "Drug B" = "#377eb8",
#'   "Drug C" = "#4daf4a",
#'   "Placebo" = "#999999"
#' )
#' dual_plot <- plot_dual_networks(result, treatment_colors = custom_colors)
#' }
#' 
#' @export
plot_dual_networks <- function(nma_result,
                               treatment_colors = NULL,
                               color_palette = "colorblind",
                               class_palette = NULL,
                               edge_color = "grey60",
                               node_border_color = "#333333",
                               node_size_range = c(6, 14),
                               edge_width_range = c(0.5, 3),
                               label_size = 4,
                               title_class = "Network by Treatment Class",
                               title_treatment = "Network by Treatment",
                               overall_title = NULL) {
  
  # Check for patchwork package
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required. Install with: install.packages('patchwork')")
  }
  
  # Extract nma object and treatment class mapping
  nma_obj <- nma_result$nma
  tx_class_map <- nma_result$treatment_class_map
  
  # ==================================================
  # DETERMINE CLASS PALETTE
  # ==================================================
  
  if (!is.null(tx_class_map) && is.null(class_palette)) {
    unique_classes <- unique(tx_class_map$treatment_class)
    n_classes <- length(unique_classes)
    
    if (n_classes == 2) {
      # Default for binary classification
      class_palette <- c("#E0E0E0", "#4DAF4A")
      names(class_palette) <- sort(unique_classes)
    } else {
      # Multiple classes - use Set2 palette
      class_colors <- RColorBrewer::brewer.pal(max(3, n_classes), "Set2")[1:n_classes]
      class_palette <- setNames(class_colors, sort(unique_classes))
    }
  } else if (is.null(class_palette)) {
    # Fallback default
    class_palette <- c("Control" = "#E0E0E0", "Active" = "#4DAF4A")
  }
  
  # ==================================================
  # PLOT 1: By Treatment Class
  # ==================================================
  
  if (!is.null(tx_class_map)) {
    # Ensure column names are standard
    if (!"treatment" %in% names(tx_class_map)) {
      names(tx_class_map)[1] <- "treatment"
    }
    if (!"treatment_class" %in% names(tx_class_map)) {
      names(tx_class_map)[2] <- "treatment_class"
    }
    
    # Pass column names as strings (quoted)
    p1 <- ggplot_netgraph(
      nma_obj,
      treatment_classes = tx_class_map,
      trt_col = "treatment",
      class_col = "treatment_class",
      use_treatment_colors = FALSE,
      class_palette = class_palette,
      edge_color = edge_color,
      node_border_color = node_border_color,
      node_size_range = node_size_range,
      edge_width_range = edge_width_range,
      label_size = label_size,
      title = title_class
    )
  } else {
    # No class mapping - use auto-classification
    p1 <- ggplot_netgraph(
      nma_obj,
      use_treatment_colors = FALSE,
      class_palette = class_palette,
      edge_color = edge_color,
      node_border_color = node_border_color,
      node_size_range = node_size_range,
      edge_width_range = edge_width_range,
      label_size = label_size,
      title = title_class
    )
  }
  
  # ==================================================
  # PLOT 2: By Individual Treatment (UPDATED - accepts custom colors)
  # ==================================================
  
  p2 <- ggplot_netgraph(
    nma_obj,
    use_treatment_colors = TRUE,
    treatment_colors = treatment_colors,  # Can be NULL, pre-generated, or custom
    color_palette = color_palette,         # Used only if treatment_colors is NULL
    edge_color = edge_color,
    node_border_color = node_border_color,
    node_size_range = node_size_range,
    edge_width_range = edge_width_range,
    label_size = label_size,
    title = title_treatment
  )
  
  # ==================================================
  # COMBINE PLOTS WITH SHARED LEGEND
  # ==================================================
  
  # Stack vertically with shared legend on right
  combined_plot <- p1 / p2 + 
    patchwork::plot_layout(
      ncol = 1,
      heights = c(1, 1),
      guides = "collect"  # Collect all legends into one
    ) +
    patchwork::plot_annotation(
      title = overall_title,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          size = 18,
          face = "bold",
          margin = ggplot2::margin(b = 15)
        )
      )
    )
  
  return(combined_plot)
}
