# ==============================================================================
# ENHANCED RADIAL RANK HEAT PLOT WITH IMPROVED LEGEND
# ==============================================================================

#' Create Radial Rank Heat Plot with Enhanced Interpretive Legend
#'
#' Generates a circular heatmap showing the probability distribution of each
#' treatment achieving different ranks in network meta-analysis, with an enhanced
#' legend that clearly explains the ranking interpretation.
#'
#' @param nma_obj A netmeta object from the netmeta package
#' @param nsim Integer, number of simulations for rankogram calculation. 
#'   Default is 10000.
#' @param random_seed Integer for reproducible simulation results. 
#'   Default is 123.
#' @param gradient_colors Character vector of length 3 specifying colors for
#'   gradient (low, mid, high). Default is c("#d73027", "#fee08b", "#1a9850")
#'   representing red-yellow-green scale.
#' @param gradient_midpoint Numeric, midpoint value for color gradient (0-100).
#'   Default is 50.
#' @param text_color Character string, color for probability text labels.
#'   Default is "black".
#' @param text_size Numeric, size of probability text labels. Default is 3.
#' @param text_threshold Numeric between 0 and 1, minimum probability to display
#'   text label. Default is 0.05 (5%). Values below this are left blank.
#' @param tile_border_color Character string, color for borders between tiles.
#'   Default is "white".
#' @param tile_border_width Numeric, width of tile borders. Default is 0.8.
#' @param grid_color Character string, color for radial grid lines.
#'   Default is "gray80".
#' @param grid_width Numeric, width of grid lines. Default is 0.5.
#' @param show_treatment_legend Logical, whether to display numbered treatment legend.
#'   Default is FALSE.
#' @param treatment_legend_title Character string for treatment legend title.
#'   Default is "Treatment".
#' @param title Character string for plot title. 
#'   Default is "Radial Rank Heat Plot".
#' @param subtitle Character string for plot subtitle. 
#'   Default is NULL (auto-generated based on ranking direction).
#' @param legend_title Character string for color legend title. 
#'   Default is "Probability (%)".
#' @param legend_position Character string for legend position: "right", "left",
#'   "top", "bottom", or "none". Default is "right".
#' @param treatment_text_size Numeric, size of treatment labels. Default is 11.
#' @param rank_text_size Numeric, size of rank labels. Default is 9.
#' @param title_size Numeric, size of title text. Default is 14.
#' @param subtitle_size Numeric, size of subtitle text. Default is 10.
#'
#' @return A ggplot2 object
#'
#' @references
#' Salanti, G., Ades, A. E., & Ioannidis, J. P. (2011). Graphical methods and
#' numerical summaries for presenting results from multiple-treatment meta-analysis:
#' an overview and tutorial. \emph{Journal of Clinical Epidemiology, 64}(2), 163-171.
#'
#' Veroniki, A. A., Straus, S. E., Fyraridis, A., & Tricco, A. C. (2016). The
#' rank-heat plot is a novel way to present the results from a network meta-analysis
#' including multiple outcomes. \emph{Journal of Clinical Epidemiology, 76}, 193-199.
#' doi: 10.1016/j.jclinepi.2016.02.016
#'
#' @export
plot_radial_ranks <- function(nma_obj,
                              precomputed_ranking = NULL,
                              nsim = 10000,
                              random_seed = 123,
                              gradient_colors = c("#d73027", "#fee08b", "#1a9850"),
                              gradient_midpoint = 50,
                              text_color = "black",
                              text_size = 3,
                              text_threshold = 0.05,
                              tile_border_color = "white",
                              tile_border_width = 0.8,
                              grid_color = "gray80",
                              grid_width = 0.5,
                              show_treatment_legend = FALSE,
                              treatment_legend_title = "Treatment",
                              title = "Radial Rank Heat Plot",
                              subtitle = NULL,
                              legend_title = "Probability (%)",
                              legend_position = "right",
                              treatment_text_size = 11,
                              rank_text_size = 9,
                              title_size = 14,
                              subtitle_size = 10) {
  
  # ✅ Use pre-computed ranking data if available
  if (!is.null(precomputed_ranking)) {
    # Check if it's a rankogram object (has $ranking.matrix.random)
    if (!is.null(precomputed_ranking$ranking.matrix.random)) {
      ranking_probs <- precomputed_ranking$ranking.matrix.random
    } else {
      stop("precomputed_ranking must be a rankogram object with $ranking.matrix.random")
    }
  } else {
    # Calculate ranking probabilities
    rank_obj <- netmeta::rankogram(nma_obj, nsim = nsim, random = TRUE)
    ranking_probs <- rank_obj$ranking.matrix.random
  }
  
  # Get treatment names
  treatment_names <- rownames(ranking_probs)
  n_treatments <- length(treatment_names)
  n_ranks <- ncol(ranking_probs)
  
  # Create treatment labels (numbered if legend requested)
  if (show_treatment_legend) {
    treatment_labels <- as.character(1:n_treatments)
    treatment_mapping <- data.frame(
      Number = 1:n_treatments,
      Treatment = treatment_names,
      stringsAsFactors = FALSE
    )
  } else {
    treatment_labels <- treatment_names
  }
  
  # Convert to long format
  rank_data <- data.frame(
    treatment_original = rep(treatment_names, n_ranks),
    treatment = rep(treatment_labels, n_ranks),
    rank = rep(1:n_ranks, each = n_treatments),
    probability = as.vector(ranking_probs),
    stringsAsFactors = FALSE
  )
  
  # Convert to percentage
  rank_data$prob_pct <- rank_data$probability * 100
  
  # Auto-generate subtitle if not provided
  if (is.null(subtitle)) {
    subtitle <- "Inner circle = Rank 1 (Best) • Outer circle = Rank n (Worst)"
  }
  
  # Create base plot
  p <- ggplot2::ggplot(rank_data, ggplot2::aes(x = treatment, y = rank, fill = prob_pct)) +
    
    # Tiles (heatmap cells)
    ggplot2::geom_tile(color = tile_border_color, linewidth = tile_border_width) +
    
    # Text labels (only show if above threshold)
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(probability > text_threshold, 
                                  sprintf("%.1f", prob_pct), "")),
      size = text_size, 
      color = text_color,
      fontface = "bold"
    ) +
    
    # Convert to polar coordinates (circular plot)
    ggplot2::coord_polar(theta = "x") +
    
    # CONTINUOUS color gradient (not discrete)
    ggplot2::scale_fill_gradient2(
      low = gradient_colors[1],
      mid = gradient_colors[2], 
      high = gradient_colors[3],
      midpoint = gradient_midpoint,
      limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0%\nUnlikely", "25%\nLow", "50%\nMedium", 
                 "75%\nHigh", "100%\nCertain"),
      name = legend_title,
      guide = ggplot2::guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = 1.8,
        barheight = 18,
        frame.colour = "black",
        frame.linewidth = 0.8,
        ticks.colour = "black",
        ticks.linewidth = 0.8
      )
    ) +
    
    # Reverse y-axis so Rank 1 is at center
    ggplot2::scale_y_reverse(
      breaks = 1:n_ranks,
      labels = paste0("Rank ", 1:n_ranks)
    ) +
    
    # Labels
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = paste0("Based on ", format(nsim, big.mark = ","), " simulations")
    ) +
    
    # Theme
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        size = treatment_text_size, 
        face = "bold",
        color = "black"
      ),
      axis.text.y = ggplot2::element_text(
        size = rank_text_size,
        color = "gray30",
        face = "bold"
      ),
      panel.grid.major.x = ggplot2::element_line(
        color = grid_color, 
        linewidth = grid_width
      ),
      panel.grid.major.y = ggplot2::element_line(
        color = grid_color, 
        linewidth = grid_width
      ),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = legend_position,
      legend.title = ggplot2::element_text(face = "bold", size = 11),
      legend.text = ggplot2::element_text(size = 9, lineheight = 0.9),
      legend.box.background = ggplot2::element_rect(
        color = "gray40", 
        linewidth = 1,
        fill = "white"
      ),
      legend.box.margin = ggplot2::margin(5, 5, 5, 5),
      plot.title = ggplot2::element_text(
        hjust = 0.5, 
        face = "bold", 
        size = title_size
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5, 
        size = subtitle_size, 
        color = "gray40",
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0.5,
        size = 9,
        color = "gray50",
        margin = ggplot2::margin(t = 10)
      ),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
  
  # Add treatment legend if requested (same as before)
  if (show_treatment_legend) {
    legend_text <- paste0(treatment_mapping$Number, ": ", 
                          treatment_mapping$Treatment)
    legend_string <- paste(legend_text, collapse = "\n")
    
    p <- p + 
      ggplot2::annotate(
        "text",
        x = Inf,
        y = Inf,
        label = paste0("═══ ", treatment_legend_title, " ═══\n", legend_string),
        hjust = -0.1,
        vjust = 1.5,
        size = 3,
        fontface = "plain",
        color = "black",
        lineheight = 0.9
      ) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(10, 150, 10, 10)
      )
    
    attr(p, "treatment_mapping") <- treatment_mapping
  }
  
  return(p)
}

#' Create Comprehensive Legend Panel with Proper Spacing
#'
#' Creates a detailed legend panel with rank interpretation guide, continuous color scale,
#' and optional treatment mapping with proper spacing between sections.
#'
#' @param nma_obj A netmeta object from the netmeta package
#' @param gradient_colors Character vector of length 3 for color gradient.
#'   Default is c("#d73027", "#fee08b", "#1a9850").
#' @param show_treatment_mapping Logical, whether to include treatment numbers.
#'   Default is TRUE.
#' @param section_spacing Numeric, vertical spacing between sections (in plot units).
#'   Default is 2.
#'
#' @return A ggplot2 object (legend panel)
#'
#' @examples
#' \dontrun{
#' library(patchwork)
#' 
#' # Create plot and separate legend with proper spacing
#' p_main <- plot_radial_ranks(nma_con, legend_position = "none")
#' p_legend <- create_rank_legend_panel(nma_con, section_spacing = 2.5)
#' 
#' # Combine
#' p_combined <- p_main + p_legend + plot_layout(widths = c(2.5, 1))
#' print(p_combined)
#' }
#'
#' @export
create_rank_legend_panel <- function(nma_obj,
                                     gradient_colors = c("#d73027", "#fee08b", "#1a9850"),
                                     show_treatment_mapping = TRUE,
                                     section_spacing = 2) {
  
  # Get treatment info
  treatment_names <- nma_obj$trts
  n_treatments <- length(treatment_names)
  
  # Calculate positions with spacing
  current_y <- 1
  
  # Section 1: Color Scale (continuous gradient)
  color_section_header_y <- current_y
  current_y <- current_y - 1
  color_bar_y <- current_y - 4.5  # Center of 9-unit tall bar
  current_y <- current_y - 10  # Bar takes 9 units + 1 buffer
  
  # Add section spacing
  current_y <- current_y - section_spacing
  
  # Section 2: Rank Interpretation
  rank_section_header_y <- current_y
  current_y <- current_y - 1
  rank_items <- c(
    "Rank 1 (Best) → CENTER",
    "Middle Ranks → Mid-circle",
    paste0("Rank ", n_treatments, " (Worst) → EDGE")
  )
  rank_y_positions <- seq(current_y, current_y - length(rank_items) + 1, by = -1)
  current_y <- min(rank_y_positions) - 1
  
  # Add section spacing
  current_y <- current_y - section_spacing
  
  # Section 3: Treatments (if requested)
  if (show_treatment_mapping) {
    treatment_section_header_y <- current_y
    current_y <- current_y - 1
    treatment_items <- paste0(1:n_treatments, ": ", treatment_names)
    treatment_y_positions <- seq(current_y, current_y - n_treatments + 1, by = -1)
  }
  
  # Create gradient data for continuous color bar
  gradient_df <- data.frame(
    y = seq(color_bar_y - 4.5, color_bar_y + 4.5, length.out = 100),
    value = seq(0, 100, length.out = 100)
  )
  
  # Create plot
  p <- ggplot2::ggplot() +
    
    # ═══════════════════════════════════════════════════
    # SECTION 1: PROBABILITY SCALE (Continuous)
    # ═══════════════════════════════════════════════════
    
    # Section header
    ggplot2::annotate(
      "text",
      x = 0.5, y = color_section_header_y,
      label = "PROBABILITY SCALE",
      hjust = 0.5, fontface = "bold", size = 4, color = "black"
    ) +
    
    # Continuous color gradient bar
    ggplot2::geom_tile(
      data = gradient_df,
      ggplot2::aes(x = 0.3, y = y, fill = value),
      width = 0.15, height = 0.1
    ) +
    ggplot2::scale_fill_gradient2(
      low = gradient_colors[1],
      mid = gradient_colors[2],
      high = gradient_colors[3],
      midpoint = 50,
      limits = c(0, 100),
      guide = "none"
    ) +
    
    # Color bar border
    ggplot2::annotate(
      "rect",
      xmin = 0.225, xmax = 0.375,
      ymin = color_bar_y - 4.5, ymax = color_bar_y + 4.5,
      fill = NA, color = "black", linewidth = 0.8
    ) +
    
    # Labels for color scale
    ggplot2::annotate(
      "text",
      x = 0.42, 
      y = c(color_bar_y + 4.5, color_bar_y + 2.25, color_bar_y, 
            color_bar_y - 2.25, color_bar_y - 4.5),
      label = c("100% - Certain", "75% - High probability", "50% - Medium probability",
                "25% - Low probability", "0% - Unlikely"),
      hjust = 0, size = 3.2, fontface = "plain"
    ) +
    
    # ═══════════════════════════════════════════════════
    # SECTION 2: RANK INTERPRETATION
    # ═══════════════════════════════════════════════════
    
    # Section header
    ggplot2::annotate(
      "text",
      x = 0.5, y = rank_section_header_y,
      label = "RANK INTERPRETATION",
      hjust = 0.5, fontface = "bold", size = 4, color = "black"
    ) +
    
    # Rank items
    ggplot2::annotate(
      "text",
      x = 0.5, y = rank_y_positions,
      label = rank_items,
      hjust = 0.5, size = 3.2, fontface = "plain"
    ) +
    
    # ═══════════════════════════════════════════════════
    # SECTION 3: TREATMENTS
    # ═══════════════════════════════════════════════════
    {
      if (show_treatment_mapping) {
        list(
          # Section header
          ggplot2::annotate(
            "text",
            x = 0.5, y = treatment_section_header_y,
            label = "TREATMENTS",
            hjust = 0.5, fontface = "bold", size = 4, color = "black"
          ),
          # Treatment items
          ggplot2::annotate(
            "text",
            x = 0.5, y = treatment_y_positions,
            label = treatment_items,
            hjust = 0.5, size = 3, fontface = "plain"
          )
        )
      }
    } +
    
    # Set limits and theme
    ggplot2::xlim(0, 1) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = "white", 
        color = "gray50", 
        linewidth = 1.5
      ),
      plot.margin = ggplot2::margin(20, 15, 20, 15)
    )
  
  return(p)
}




