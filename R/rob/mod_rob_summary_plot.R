# =============================================================================
# Module: ROB Summary Plot (With domain name mapping)
# =============================================================================


#' ROB Summary Plot Function
#' @param df Data frame with ROB assessments
#' @param color_map Named vector of colors for each risk level
#' @param risk_labels Character vector of risk levels in order
#' @param domain_names Named vector mapping codes to full names (optional)
#' @param study_col Name of the study column
#' @param overall_col Name of the overall column
#' @param include_overall Whether to include Overall column
#' @param base_size Base font size for plot
rob_summary_plot <- function(df,
                             color_map,
                             risk_labels,
                             domain_names = NULL,
                             study_col = "Study",
                             overall_col = "Overall",
                             include_overall = TRUE,
                             base_size = 14) {
  
  # Rename domains if mapping provided
  if (!is.null(domain_names)) {
    df <- rename_domains(df, domain_names)
    # Update overall_col name if it was renamed
    if ("Overall" %in% names(domain_names)) {
      overall_col <- domain_names["Overall"]
    }
  }
  
  # Get all columns except Study
  domain_columns <- setdiff(colnames(df), study_col)
  
  # Remove Overall if not wanted
  if (!include_overall && overall_col %in% domain_columns) {
    domain_columns <- setdiff(domain_columns, overall_col)
  }
  
  # Make sure we have at least one domain
  if (length(domain_columns) == 0) {
    stop("No domain columns found to plot")
  }
  
  # Filter data to only selected columns
  df_filtered <- df %>%
    select(all_of(c(study_col, domain_columns)))
  
  # Reshape to long format
  df_long <- df_filtered %>%
    pivot_longer(
      all_of(domain_columns),
      names_to = "Domain",
      values_to = "Judgement"
    ) %>%
    mutate(
      Domain = factor(Domain, levels = domain_columns, ordered = TRUE),
      Judgement = factor(Judgement, levels = risk_labels, ordered = TRUE)
    )
  
  # Expand grid
  complete_grid <- expand.grid(
    Domain = domain_columns,
    Judgement = risk_labels,
    stringsAsFactors = FALSE
  )
  
  # Calculate proportions
  plot_data <- df_long %>%
    count(Domain, Judgement) %>%
    right_join(complete_grid, by = c("Domain", "Judgement")) %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    group_by(Domain) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
  
  plot_data$Domain <- factor(plot_data$Domain, levels = domain_columns, ordered = TRUE)
  plot_data$Judgement <- factor(plot_data$Judgement, levels = risk_labels, ordered = TRUE)
  
  # Create plot
  ggplot(plot_data, aes(y = Domain, x = prop, fill = Judgement)) +
    geom_bar(stat = "identity", color = "#1d3557", linewidth = 0.5, width = 0.75) +
    scale_fill_manual(values = color_map, drop = FALSE) +
    scale_y_discrete(limits = rev(domain_columns), drop = FALSE) +
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = c(0, 0)
    ) +
    labs(
      x = "Proportion of Studies (%)",
      y = NULL,
      fill = "Risk of Bias"
    ) +
    theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      axis.title.x = element_text(face = "bold", color = "#1d3557", size = base_size, margin = margin(t = 10)),
      axis.text.y = element_text(face = "bold", color = "#1d3557", size = base_size - 1),
      axis.text.x = element_text(face = "bold", color = "#5A6169", size = base_size - 2),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#E0E0E0", linewidth = 0.3),
      legend.position = "top",
      legend.title = element_text(face = "bold", color = "#1d3557", size = base_size),
      legend.text = element_text(color = "#5A6169", size = base_size - 2),
      legend.key.size = unit(0.8, "cm"),
      legend.spacing.x = unit(0.3, "cm"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(15, 15, 15, 15)
    )
}