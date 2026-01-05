# ==============================================================================
# NET HEAT MATRIX - CLEAN VERSION WITH FIXES
# ==============================================================================

#' Create Net Heat Matrix (Clean Version - No Text, No Diagonal)
#'
#'
#' @param nma_obj A netmeta object from the netmeta package
#' @param gradient_colors Character vector of length 3. Default is c("white", "#fee08b", "#d73027").
#' @param tile_border_color Character string. Default is "white".
#' @param tile_border_width Numeric. Default is 0.5.
#' @param na_color Character string. Default is "gray80".
#' @param show_diagonal Logical. Default is FALSE (hides diagonal).
#' @param title Character string.
#' @param subtitle Character string.
#' @param legend_title Character string. Default is "|Residual|".
#' @param legend_position Character string. Default is "right".
#' @param base_size Numeric. Default is 11.
#'
#' @return A ggplot2 object
#' @export
plot_netheat_matrix <- function(nmaobj,
                                precomputed_decomp = NULL,  # NEW
                                gradient_colors = c("white", "#fee08b", "#d73027"),
                                tile_border_color = "white",
                                tile_border_width = 0.5,
                                na_color = "gray80",
                                show_diagonal = FALSE,
                                title = "Net Heat Matrix - Design Inconsistency Residuals",
                                subtitle = NULL,
                                legend_title = "Residual",
                                legend_position = "right",
                                base_size = 11) {
  
  # Use pre-computed decomposition if available
  # ✅ Use pre-computed decomposition if available
  if (!is.null(precomputed_decomp)) {
    decomp_obj <- precomputed_decomp
  } else {
    decomp_obj <- netmeta::decomp.design(nma_obj)
  }
  
  # Extract residual matrix
  if (is.null(decomp_obj$residuals.inc.detach)) {
    stop("No residual matrix available from design decomposition")
  }
  
  resid_matrix <- decomp_obj$residuals.inc.detach
  
  # Convert to long format
  heat_long <- expand.grid(
    design_row = rownames(resid_matrix),
    design_col = colnames(resid_matrix),
    stringsAsFactors = FALSE
  )
  heat_long$residual <- as.vector(resid_matrix)
  
  # Calculate absolute residuals for coloring
  heat_long$abs_residual <- abs(heat_long$residual)
  
  # Hide diagonal if requested
  if (!show_diagonal) {
    heat_long$abs_residual <- ifelse(
      heat_long$design_row == heat_long$design_col,
      NA,
      heat_long$abs_residual
    )
  }
  
  # Calculate midpoint (excluding NA/diagonal)
  midpoint_val <- median(heat_long$abs_residual, na.rm = TRUE)
  
  # Create plot
  p <- ggplot2::ggplot(heat_long, ggplot2::aes(x = design_col, y = design_row, fill = abs_residual)) +
    
    # Tiles with residual coloring
    ggplot2::geom_tile(color = tile_border_color, linewidth = tile_border_width) +
    
    # Color scale (continuous gradient)
    ggplot2::scale_fill_gradient2(
      low = gradient_colors[1],
      mid = gradient_colors[2],
      high = gradient_colors[3],
      midpoint = midpoint_val,
      na.value = na_color,
      name = legend_title,
      guide = ggplot2::guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = 1.5,
        barheight = 15,
        frame.colour = "black",
        ticks.colour = "black"
      )
    ) +
    
    # Labels
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Design",
      y = "Design"
    ) +
    
    # Equal aspect ratio (square tiles)
    ggplot2::coord_equal() +
    
    # Theme
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45, 
        hjust = 1,
        vjust = 1,
        size = base_size - 1,
        color = "black"
      ),
      axis.text.y = ggplot2::element_text(
        size = base_size - 1,
        color = "black"
      ),
      axis.title = ggplot2::element_text(
        face = "bold",
        size = base_size + 1
      ),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = base_size + 3,
        hjust = 0.5
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size,
        hjust = 0.5,
        color = "gray40",
        margin = ggplot2::margin(b = 10)
      ),
      legend.position = legend_position,
      legend.title = ggplot2::element_text(face = "bold", size = base_size),
      legend.text = ggplot2::element_text(size = base_size - 1),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
  
  return(p)
}

#' Create Bar Plot of Design Q Statistics
#'
#' @param nma_obj A netmeta object
#' @param sig_color Character. Default "#d73027".
#' @param nonsig_color Character. Default "#1a9850".
#' @param title Character string.
#' @param show_pvalues Logical. Default TRUE.
#' @param base_size Numeric. Default 11.
#'
#' @return A ggplot2 object
#' @export
plot_design_q_barplot <- function(nma_obj,
                                  precomputed_decomp = NULL,
                                  sig_color = "#d73027",
                                  nonsig_color = "#1a9850",
                                  title = "Design Q Statistics: Inconsistency Assessment",
                                  show_pvalues = TRUE,
                                  base_size = 11) {
  
  if (!is.null(precomputed_decomp)) {
    decomp_obj <- precomputed_decomp
  } else {
    decomp_obj <- netmeta::decomp.design(nma_obj)
  }
  
  # Extract Q statistics
  q_detach_data <- decomp_obj$Q.inc.detach
  
  # Extract Q statistics
  q_detach_data <- decomp_obj$Q.inc.detach
  
  # Create data frame
  netheat_data <- data.frame(
    design = rownames(q_detach_data),
    Q_detach = q_detach_data$Q,
    df = q_detach_data$df,
    pval = q_detach_data$pval,
    stringsAsFactors = FALSE
  )
  
  # Format p-values properly
  netheat_data$pval_label <- ifelse(
    netheat_data$pval < 0.001, "p < 0.001",
    sprintf("p = %.3f", netheat_data$pval)
  )
  
  # Create plot
  p <- ggplot2::ggplot(netheat_data, 
                       ggplot2::aes(x = reorder(design, -Q_detach), y = Q_detach)) +
    
    ggplot2::geom_col(
      ggplot2::aes(fill = pval < 0.05), 
      width = 0.7,
      color = "black",
      linewidth = 0.3
    ) +
    
    # P-values (if requested)
    # {
    #   if (show_pvalues) {
    #     ggplot2::geom_text(
    #       ggplot2::aes(label = sprintf("Q=%.2f\n%s", Q_detach, pval_label)),
    #       vjust = -0.3, 
    #       size = 3
    #     )
    #   }
    # } +
    
    ggplot2::scale_fill_manual(
      values = c("TRUE" = sig_color, "FALSE" = nonsig_color),
      labels = c("TRUE" = "Significant (p<0.05)", "FALSE" = "Non-significant"),
      name = "Inconsistency"
    ) +
    
    # FIX: Expand y-axis to accommodate all values
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.15)),
      limits = c(0, NA)  # Start at 0, no upper limit
    ) +
    
    ggplot2::labs(
      title = title,
      subtitle = "Between-designs Q statistics after detaching each design",
      x = "Design",
      y = "Q Statistic"
    ) +
    
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      plot.title = ggplot2::element_text(face = "bold", size = 13, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 10, color = "gray40", hjust = 0.5),
      legend.position = "top",
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
  
  return(p)
}

#' Get Summary Table
#'
#' @param nma_obj A netmeta object
#' @param sort_by Character: "Q", "pval", or "design". Default "Q".
#'
#' @return A data frame
#' @export
get_netheat_summary <- function(nma_obj, sort_by = "Q") {
  
  decomp_obj <- netmeta::decomp.design(nma_obj)
  q_detach_data <- decomp_obj$Q.inc.detach
  
  summary_df <- data.frame(
    Design = rownames(q_detach_data),
    Q_statistic = round(q_detach_data$Q, 2),
    df = q_detach_data$df,
    p_value = q_detach_data$pval,
    Significant = ifelse(q_detach_data$pval < 0.05, "Yes", "No"),
    stringsAsFactors = FALSE
  )
  
  # Format p-values
  summary_df$p_formatted <- ifelse(
    summary_df$p_value < 0.001, "< 0.001",
    sprintf("%.3f", summary_df$p_value)
  )
  
  # Sort
  if (sort_by == "Q") {
    summary_df <- summary_df[order(-summary_df$Q_statistic), ]
  } else if (sort_by == "pval") {
    summary_df <- summary_df[order(summary_df$p_value), ]
  } else if (sort_by == "design") {
    summary_df <- summary_df[order(summary_df$Design), ]
  }
  
  rownames(summary_df) <- NULL
  return(summary_df)
}

# ==============================================================================
# USAGE EXAMPLES
# ==============================================================================

# library(netmeta)
# library(ggplot2)
# 
# # Example 1: Clean heatmap (no text, no diagonal, legend on right)
# p1 <- plot_netheat_matrix(nma_con)
# print(p1)
# ggsave("netheat_clean.png", p1, width = 12, height = 10, dpi = 300)
# 
# # Example 2: Show diagonal (if needed)
# p2 <- plot_netheat_matrix(nma_con, show_diagonal = TRUE)
# 
# 
# # Example 3: Legend on bottom (if preferred)
# p3 <- plot_netheat_matrix(nma_con, legend_position = "bottom")
# 
# # Example 4: Bar plot (fixed y-axis)
# p4 <- plot_design_q_barplot(nma_con)
# print(p4)
# ggsave("design_q_barplot.png", p4, width = 10, height = 8, dpi = 300)
# 
# # Example 5: Bar plot without p-values (cleaner)
# p5 <- plot_design_q_barplot(nma_con, show_pvalues = FALSE)
# 
# # Example 6: Get summary table
# summary_table <- get_netheat_summary(nma_con)
# print(summary_table)
# 
# # Example 7: Custom colors
# p6 <- plot_netheat_matrix(
#   nma_con,
#   gradient_colors = c("#f7fbff", "#6baed6", "#08519c"),
#   title = "Design Inconsistency Analysis"
# )
# 
# print(p6)
# 
# # Example 8: Combined view with patchwork
# library(patchwork)
# p_heat <- plot_netheat_matrix(nma_con, legend_position = "left")
# p_bar <- plot_design_q_barplot(nma_con)
# 
# p_combined <- p_heat + p_bar + plot_layout(widths = c(2, 2))
# print(p_combined)
# ggsave("netheat_combined.png", p_combined, width = 18, height = 9, dpi = 300)
