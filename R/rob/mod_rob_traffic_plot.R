# =============================================================================
# Module: ROB Traffic Light Plot (Updated with bold multi-line caption)
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

#' ROB Traffic Light Plot Function
#' @param df Data frame with ROB assessments
#' @param color_map Named vector of colors
#' @param symbol_map Named vector of symbols
#' @param domain_names Named vector mapping codes to full names (for caption)
#' @param psize Point size
#' @param show_overall Show Overall column
#' @param font_size Base font size
#' @param legend_title Legend title
#' @param study_col Study column name
#' @param overall_col Overall column name
rob_traffic_plot <- function(df,
                             color_map,
                             symbol_map,
                             domain_names = NULL,
                             psize = 10,
                             show_overall = TRUE,
                             font_size = 6,
                             legend_title = "Judgement",
                             study_col = "Study",
                             overall_col = "Overall") {
  
  # Detect domain columns
  domain_columns <- colnames(df)
  domain_columns <- setdiff(domain_columns, study_col)
  if (!show_overall) {
    domain_columns <- setdiff(domain_columns, overall_col)
  }
  
  # Create multi-line caption if domain names provided
  caption_text <- if (!is.null(domain_names)) {
    create_domain_caption(domain_names)
  } else {
    NULL
  }
  
  # Preprocess data to long format
  df_long <- df %>%
    mutate(!!study_col := factor(.data[[study_col]], levels = rev(unique(.data[[study_col]])))) %>%
    pivot_longer(
      cols = all_of(domain_columns),
      names_to = "Domain",
      values_to = "Judgement"
    ) %>%
    filter(Domain %in% domain_columns)
  
  df_long$Domain <- factor(df_long$Domain, levels = domain_columns)
  df_long$Symbol <- symbol_map[df_long$Judgement]
  
  n_stud <- nlevels(df_long[[study_col]])
  idx_overall <- which(domain_columns == overall_col)
  
  # Create base plot
  base <- ggplot(df_long, aes(x = Domain, y = .data[[study_col]]))
  
  # Add grey background for Overall column if present
  if (length(idx_overall) == 1) {
    base <- base +
      geom_rect(
        inherit.aes = FALSE,
        xmin = idx_overall - 0.5,
        xmax = idx_overall + 0.5,
        ymin = 0.5,
        ymax = n_stud + 0.5,
        fill = "#E8EAED",
        color = NA
      )
  }
  
  # Build complete plot
  base +
    # Grid lines
    geom_tile(fill = NA, color = "#1d3557", linewidth = 0.8, width = 1, height = 1) +
    # Colored circles
    geom_point(aes(fill = Judgement), shape = 21, size = psize, stroke = 0, show.legend = TRUE) +
    # Symbols
    geom_text(aes(label = Symbol), size = psize * 0.6, fontface = "bold", color = "black") +
    # Scales
    scale_y_discrete(position = "left") +
    scale_x_discrete(position = "top") +
    scale_fill_manual(
      values = color_map,
      drop = FALSE,
      name = legend_title,
      guide = guide_legend(override.aes = list(color = NA, stroke = 0))
    ) +
    # Add caption
    labs(caption = caption_text) +
    # Theme
    theme_minimal(base_size = 15, base_family = "sans") +
    theme(
      # Remove axis titles
      axis.title = element_blank(),
      
      # Domain labels (top)
      axis.text.x = element_text(
        size = font_size * 2.2,
        face = "bold",
        vjust = 1.5,
        color = "#1d3557"
      ),
      
      # Study labels (left)
      axis.text.y = element_text(
        size = font_size * 1.9,
        face = "bold",
        hjust = 1,
        color = "#1d3557"
      ),
      
      # Remove grid
      panel.grid = element_blank(),
      panel.border = element_blank(),
      
      # Plot margins - increase bottom for multi-line caption
      plot.margin = margin(12, 14, 20, 8),
      
      # Caption styling - BOLD, multi-line, left-aligned
      plot.caption = element_text(
        hjust = 0,              # Left align
        size = font_size * 1.8, # Slightly larger
        color = "#1d3557",      # Dark blue color
        face = "bold",          # BOLD
        margin = margin(t = 15), # Space above caption
        lineheight = 1.4        # Line spacing for multi-line
      ),
      
      # Legend styling
      legend.position = "right",
      legend.key.height = unit(0.9, "cm"),
      legend.key.width = unit(0.8, "cm"),
      legend.title = element_text(
        face = "bold",
        color = "#1d3557",
        size = font_size * 2
      ),
      legend.text = element_text(
        color = "#5A6169",
        size = font_size * 1.8
      ),
      
      # Remove ticks and lines
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      
      # Background
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}