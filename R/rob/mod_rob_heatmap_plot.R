# =============================================================================
# Module: ROB Heatmap Plot (Fixed - No Overlapping Labels)
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

#' ROB Heatmap Plot Function (Fixed)
#' @param df Data frame with ROB assessments
#' @param color_map Named vector of colors for risk levels
#' @param risk_labels Character vector of risk levels in order
#' @param show_row_avg Show row (study) averages
#' @param show_col_avg Show column (domain) averages
#' @param font_size Base font size
#' @param study_col Study column name
#' @param overall_col Overall column name
#' @param show_overall Show Overall column

rob_heatmap_plot <- function(df,
                             color_map,
                             risk_labels,
                             show_row_avg = TRUE,
                             show_col_avg = TRUE,
                             font_size = 12,
                             study_col = "Study",
                             overall_col = "Overall",
                             show_overall = TRUE) {
  
  # Detect domain columns
  domain_cols <- grep("^D[0-9]+$", names(df), value = TRUE)
  
  # Sort domains numerically
  domain_nums <- as.numeric(gsub("D", "", domain_cols))
  domain_cols <- domain_cols[order(domain_nums)]
  
  # Add Overall if requested
  if (show_overall && overall_col %in% names(df)) {
    domain_cols <- c(domain_cols, overall_col)
  }
  
  # Create numeric mapping for risk levels
  risk_numeric <- setNames(seq_along(risk_labels), risk_labels)
  
  # Prepare main data
  plot_data <- df %>%
    select(all_of(c(study_col, domain_cols))) %>%
    pivot_longer(
      cols = all_of(domain_cols),
      names_to = "Domain",
      values_to = "Risk"
    ) %>%
    mutate(
      Domain = factor(Domain, levels = domain_cols),
      Study = factor(!!sym(study_col), levels = rev(unique(!!sym(study_col)))),
      Risk_Numeric = risk_numeric[Risk],
      Display_Type = "Main"
    )
  
  # Calculate row averages (per study)
  if (show_row_avg) {
    row_avg <- plot_data %>%
      group_by(Study) %>%
      summarise(Row_Average = mean(Risk_Numeric, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        Domain = "Row_Avg",
        Risk = NA_character_,
        Display_Type = "Row_Avg"
      ) %>%
      rename(Risk_Numeric = Row_Average)
    
    plot_data <- bind_rows(plot_data, row_avg) %>%
      mutate(Domain = factor(Domain, levels = c(domain_cols, "Row_Avg")))
  }
  
  # Calculate column averages (per domain)
  if (show_col_avg) {
    col_avg <- plot_data %>%
      filter(Display_Type == "Main") %>%
      group_by(Domain) %>%
      summarise(Col_Average = mean(Risk_Numeric, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        Study = "Column_Avg",
        Risk = NA_character_,
        Display_Type = "Column_Avg"
      ) %>%
      rename(Risk_Numeric = Col_Average)
    
    plot_data <- bind_rows(plot_data, col_avg) %>%
      mutate(Study = factor(Study, levels = c("Column_Avg", levels(plot_data$Study))))
  }
  
  # Use actual risk colors directly from color_map for main cells
  plot_data <- plot_data %>%
    mutate(
      Cell_Color = case_when(
        Display_Type == "Main" ~ color_map[Risk],
        Display_Type == "Row_Avg" ~ "#301934",    # MetaSuite purple
        Display_Type == "Column_Avg" ~ "#457b9d"  # MetaSuite blue
      ),
      Text_Color = "white",
      Cell_Label = case_when(
        Display_Type == "Main" ~ "",
        TRUE ~ sprintf("%.2f", Risk_Numeric)
      )
    )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = Domain, y = Study)) +
    geom_tile(aes(fill = Cell_Color), color = "white", linewidth = 2) +
    geom_text(
      aes(label = Cell_Label, color = Text_Color),
      size = font_size / 3,
      fontface = "bold"
    ) +
    scale_fill_identity() +
    scale_color_identity() +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = font_size) +
    theme(
      # Axes
      axis.text.x = element_text(
        angle = 45, 
        hjust = 1, 
        face = "bold", 
        color = "#1d3557",
        size = font_size * 1.1
      ),
      axis.text.y = element_text(
        face = "bold",
        color = "#1d3557",
        size = font_size * 0.95
      ),
      
      # Grid
      panel.grid = element_blank(),
      
      # Background
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Margins
      plot.margin = margin(20, 20, 20, 20),
      
      # Remove legend since we use actual colors
      legend.position = "none"
    ) +
    # Add subtle separators for averages
    {if(show_row_avg) geom_vline(xintercept = length(domain_cols) + 0.5, 
                                 color = "#301934", linewidth = 1, alpha = 0.3)} +
    {if(show_col_avg) geom_hline(yintercept = length(unique(df[[study_col]])) + 1.5,
                                 color = "#457b9d", linewidth = 1, alpha = 0.3)}
  
  return(p)
}
