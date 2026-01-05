# ==============================================================================
# SCRIPT : nma_stacked_plots.R
# Stacked Bar Plot Functions for Network Meta-Analysis Ranking Probabilities
# UPDATED: Publication-friendly version with cleaner design
# ==============================================================================

#' Create Stacked Layout Plot
#'
#' Internal helper function to create stacked bar plot showing ranking probabilities
#' for all treatments. UPDATED: Removed internal labels for cleaner publication look.
#'
#' @param rank_data Data frame with ranking probability data
#' @param rank_colors Character vector of colors for ranks
#' @param n_ranks Integer, number of possible ranks
#' @param legend_position Character string for legend position
#' @param title Character string for plot title
#' @param subtitle Character string for plot subtitle
#' @param nsim Integer, number of simulations used
#' @param custom_theme Optional ggplot2 theme object
#'
#' @return A ggplot2 object
#'
#' @keywords internal
create_stacked_layout <- function(rank_data, rank_colors, n_ranks,
                                  legend_position, title, subtitle, nsim,
                                  custom_theme = NULL) {
  
  rank_data$rank_factor <- factor(rank_data$rank, levels = 1:n_ranks)
  
  p <- ggplot2::ggplot(rank_data, ggplot2::aes(x = reorder(treatment, sucra),
                                               y = probability,
                                               fill = rank_factor)) +
    # Clean stacked bars without labels
    ggplot2::geom_col(width = 0.7, color = "white", linewidth = 0.5) +
    
    ggplot2::scale_fill_manual(
      values = rank_colors,
      name = "Rank",
      labels = paste0("Rank ", 1:n_ranks)
    ) +
    
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = c(0, 0),
      breaks = seq(0, 1, 0.25)
    ) +
    
    ggplot2::coord_flip() +
    
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Cumulative Probability",
      caption = paste0("Based on ", format(nsim, big.mark = ","),
                       " simulations from random-effects model")
    )
  
  # Enhanced publication-quality theme
  if (!is.null(custom_theme)) {
    p <- p + custom_theme
  } else {
    p <- p +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        # Grid and background
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line(color = "gray90", linewidth = 0.3),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        
        # Axis styling
        axis.text.y = ggplot2::element_text(size = 10, color = "black"),
        axis.text.x = ggplot2::element_text(size = 9, color = "black"),
        axis.title.x = ggplot2::element_text(size = 10, margin = ggplot2::margin(t = 10)),
        axis.line.x = ggplot2::element_line(color = "black", linewidth = 0.5),
        axis.ticks.x = ggplot2::element_line(color = "black", linewidth = 0.3),
        
        # Text elements
        plot.title = ggplot2::element_text(size = 12, face = "bold",
                                           margin = ggplot2::margin(b = 5)),
        plot.subtitle = ggplot2::element_text(size = 9, color = "gray30",
                                              margin = ggplot2::margin(b = 15)),
        plot.caption = ggplot2::element_text(size = 8, color = "gray40",
                                             hjust = 0, face = "italic",
                                             margin = ggplot2::margin(t = 10)),
        
        # Legend styling - more prominent since it's the only label source
        legend.position = legend_position,
        legend.title = ggplot2::element_text(face = "bold", size = 10),
        legend.text = ggplot2::element_text(size = 9),
        legend.key.size = ggplot2::unit(0.5, "cm"),
        legend.key = ggplot2::element_rect(color = "gray80", linewidth = 0.3),
        legend.background = ggplot2::element_rect(fill = "white", color = "gray60", linewidth = 0.4),
        legend.margin = ggplot2::margin(5, 5, 5, 5),
        
        # Plot margins
        plot.margin = ggplot2::margin(10, 10, 10, 10)
      )
  }
  
  return(p)
}


#' Create Faceted Layout Plot
#'
#' Internal helper function to create faceted bar plot showing ranking probabilities
#' for each treatment in separate panels.
#'
#' @inheritParams create_stacked_layout
#'
#' @return A ggplot2 object
#'
#' @keywords internal
create_faceted_layout <- function(rank_data, n_ranks,
                                  legend_position, title, subtitle, nsim,
                                  custom_theme = NULL) {
  
  p <- ggplot2::ggplot(rank_data, ggplot2::aes(x = rank, y = probability, fill = colorTx)) +
    ggplot2::geom_col(width = 0.7, color = "white", linewidth = 0.5) +
    ggplot2::facet_wrap(~reorder(treatment, -sucra), ncol = 3) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(breaks = 1:n_ranks) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = c(0, 0),
      limits = c(0, NA)
    ) +
    
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Rank",
      y = "Probability",
      caption = paste0("Based on ", format(nsim, big.mark = ","), " simulations")
    )
  
  # Enhanced theme for faceted layout
  if (!is.null(custom_theme)) {
    p <- p + custom_theme
  } else {
    p <- p +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        # Facet styling
        strip.text = ggplot2::element_text(face = "bold", size = 9),
        strip.background = ggplot2::element_rect(fill = "gray95", color = "gray70", linewidth = 0.3),
        panel.spacing = ggplot2::unit(1, "lines"),
        
        # Grid
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = "gray90", linewidth = 0.3),
        panel.background = ggplot2::element_rect(fill = "white", color = "gray80", linewidth = 0.3),
        
        # Axis
        axis.text = ggplot2::element_text(size = 8, color = "black"),
        axis.title = ggplot2::element_text(size = 10),
        axis.line = ggplot2::element_line(color = "black", linewidth = 0.4),
        axis.ticks = ggplot2::element_line(color = "black", linewidth = 0.3),
        
        # Text
        plot.title = ggplot2::element_text(face = "bold", size = 12, margin = ggplot2::margin(b = 5)),
        plot.subtitle = ggplot2::element_text(size = 9, color = "gray30", margin = ggplot2::margin(b = 10)),
        plot.caption = ggplot2::element_text(size = 8, color = "gray40", hjust = 0, face = "italic"),
        
        # Background
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.margin = ggplot2::margin(10, 10, 10, 10)
      )
  }
  
  return(p)
}


#' Create Stacked Bar Plot for Ranking Probabilities
#'
#' Creates a stacked or faceted bar plot showing the probability distribution of
#' each treatment achieving different ranks in network meta-analysis.
#'
#' @param nma_obj A netmeta object
#' @param data_rankinma A rankinma object from \code{\link{prepare_ranking_data}}
#' @param nsim Integer, number of simulations for rankogram calculation. Default is 10000.
#' @param treatment_colors Optional named vector of colors for treatments. If NULL,
#'   auto-generated using treatment_palette.
#' @param treatment_palette Character string for auto-generating treatment colors.
#'   Used for faceted layout. Options same as in \code{\link{plot_beading}}.
#'   Default is "default".
#' @param rank_colors Optional vector of colors for ranks. If NULL, auto-generated
#'   using rank_palette. Used for stacked layout.
#' @param rank_palette Character string for auto-generating rank gradient colors.
#'   Options: "gradient" (green to red), "colorblind" (colorblind-safe), "grayscale",
#'   "blues", "greens", "purples", "reds", "spectral", "viridis", "plasma". 
#'   Default is "gradient".
#' @param color_seed Integer for reproducible color generation. Default is 123.
#' @param layout Character string: "stacked" (single stacked bar per treatment) or
#'   "faceted" (separate panel per treatment). Default is "stacked".
#' @param legend_position Character string for legend position. Default is "right".
#' @param title Optional character string for plot title. If NULL, auto-generated.
#' @param subtitle Optional character string for plot subtitle. If NULL, auto-generated.
#' @param custom_theme Optional ggplot2 theme object to override default styling.
#'
#' @return A ggplot2 object
#'
#' @details
#' The stacked bar plot visualizes the probability distribution of rankings for each
#' treatment based on simulations from the network meta-analysis model.
#'
#' Layout options:
#' \itemize{
#'   \item "stacked": Shows all treatments in one plot with stacked bars colored by rank.
#'     Best for comparing overall ranking patterns across treatments.
#'   \item "faceted": Shows each treatment in a separate panel with bars colored by treatment.
#'     Best for examining individual treatment ranking distributions.
#' }
#'
#' UPDATED (2026-01-02): Stacked layout now has cleaner appearance without internal
#' text labels. All rank information is conveyed through the legend.
#'
#' Color handling:
#' \itemize{
#'   \item Treatment colors: Used in faceted layout, can be manual or auto-generated
#'   \item Rank colors: Used in stacked layout, can be manual or auto-generated gradient
#'   \item New palettes: "colorblind" (recommended for accessibility), "grayscale" (for print)
#' }
#'
#' @examples
#' \dontrun{
#' # Stacked layout with colorblind-safe palette (RECOMMENDED)
#' p1 <- plot_stacked_bars(
#'   nma_con,
#'   data_sucra,
#'   nsim = 10000,
#'   rank_palette = "colorblind",
#'   layout = "stacked"
#' )
#'
#' # Grayscale for print journals
#' p2 <- plot_stacked_bars(
#'   nma_con,
#'   data_sucra,
#'   rank_palette = "grayscale",
#'   layout = "stacked"
#' )
#'
#' # Faceted layout with manual treatment colors
#' my_colors <- c("Drug A" = "#FF0000", "Drug B" = "#00FF00", "Placebo" = "#0000FF")
#' p3 <- plot_stacked_bars(
#'   nma_con,
#'   data_sucra,
#'   treatment_colors = my_colors,
#'   layout = "faceted"
#' )
#' }
#'
#' @seealso \code{\link{prepare_ranking_data}}, \code{\link{plot_beading}}
#'
#' @export
plot_stacked_bars <- function(nma_obj,
                              data_rankinma,
                              precomputed_ranking = NULL,
                              nsim = 10000,
                              treatment_colors = NULL,
                              treatment_palette = "default",
                              rank_colors = NULL,
                              rank_palette = "gradient",
                              color_seed = 123,
                              layout = c("stacked", "faceted"),
                              legend_position = "right",
                              title = NULL,
                              subtitle = NULL,
                              custom_theme = NULL) {
  
  layout <- match.arg(layout)
  
  # ✅ EXTRACT RANKING PROBABILITIES CORRECTLY
  if (!is.null(precomputed_ranking)) {
    # Check if it's a rankogram object (has $ranking.matrix.random)
    if (!is.null(precomputed_ranking$ranking.matrix.random)) {
      ranking_probs <- precomputed_ranking$ranking.matrix.random
      message("✓ Using pre-computed rankogram data (instant)")
    } else if (is.matrix(precomputed_ranking)) {
      # It's already a matrix
      ranking_probs <- precomputed_ranking
      message("✓ Using pre-computed ranking matrix (instant)")
    } else {
      # Unknown structure, compute from scratch
      message("⚠ Unrecognized pre-computed format, computing now...")
      set.seed(123)
      rank_obj <- netmeta::rankogram(nma_obj, nsim = nsim, random = TRUE)
      ranking_probs <- rank_obj$ranking.matrix.random
    }
  } else {
    # Fallback: compute if not provided (SLOW)
    message("⚠ Computing ranking probabilities (", nsim, " simulations)...")
    set.seed(123)
    rank_obj <- netmeta::rankogram(nma_obj, nsim = nsim, random = TRUE)
    ranking_probs <- rank_obj$ranking.matrix.random
  }
  
  # ✅ VALIDATE EXTRACTED DATA
  if (is.null(ranking_probs) || !is.matrix(ranking_probs)) {
    stop("Failed to extract ranking probability matrix")
  }
  
  if (is.null(rownames(ranking_probs))) {
    rownames(ranking_probs) <- nma_obj$trts
  }
  
  # Convert to long format
  rank_data <- data.frame(
    treatment = rep(rownames(ranking_probs), ncol(ranking_probs)),
    rank = rep(1:ncol(ranking_probs), each = nrow(ranking_probs)),
    probability = as.vector(ranking_probs),
    stringsAsFactors = FALSE
  )
  
  # Get SUCRA/P-score for ordering
  treatment_info <- data_rankinma$data %>%
    dplyr::select(tx, metrics) %>%
    dplyr::rename(treatment = tx, sucra = metrics)
  
  # Join with treatment info
  rank_data <- rank_data %>%
    dplyr::left_join(treatment_info, by = "treatment")
  
  # ✅ CRITICAL: Convert to base R data frame after dplyr operations
  rank_data <- as.data.frame(rank_data, stringsAsFactors = FALSE)
  
  # ✅ Ensure sucra is numeric (defensive programming)
  rank_data$sucra <- as.numeric(rank_data$sucra)
  
  # Handle treatment colors
  final_treatment_colors <- create_treatment_colors(
    nma_obj,
    colors = treatment_colors,
    palette = treatment_palette,
    seed = color_seed
  )
  
  # ✅ ENSURE final_treatment_colors is a properly named vector
  if (is.null(names(final_treatment_colors))) {
    names(final_treatment_colors) <- unique(rank_data$treatment)
  }
  
  # ✅ Apply treatment colors using BASE R (NO PIPES)
  rank_data$colorTx <- final_treatment_colors[rank_data$treatment]
  
  # Handle rank colors with enhanced palettes
  n_ranks <- ncol(ranking_probs)
  if (is.null(rank_colors)) {
    rank_colors_final <- switch(
      rank_palette,
      "gradient" = colorRampPalette(c("#1b7837", "#7fbc41", "#ffffbf",
                                      "#fc8d59", "#d7191c"))(n_ranks),
      "blues" = colorRampPalette(c("#08519c", "#3182bd", "#6baed6", "#c6dbef"))(n_ranks),
      "colorblind" = colorRampPalette(c("#0571b0", "#92c5de", "#f7f7f7",
                                        "#f4a582", "#ca0020"))(n_ranks),
      "grayscale" = colorRampPalette(c("#252525", "#737373", "#bdbdbd", "#f0f0f0"))(n_ranks),
      "greens" = colorRampPalette(c("#00441b", "#238b45", "#74c476", "#c7e9c0"))(n_ranks),
      "purples" = colorRampPalette(c("#54278f", "#756bb1", "#9e9ac8", "#cbc9e2"))(n_ranks),
      "reds" = colorRampPalette(c("#a50f15", "#de2d26", "#fb6a4a", "#fcae91"))(n_ranks),
      "spectral" = rev(RColorBrewer::brewer.pal(min(11, n_ranks), "Spectral")),
      "viridis" = viridis::viridis(n_ranks, option = "D"),
      "plasma" = viridis::plasma(n_ranks),
      colorRampPalette(c("#1b7837", "#ffffbf", "#d7191c"))(n_ranks)
    )
    message(paste0("Auto-generating rank colors using '", rank_palette, "' gradient"))
  } else {
    if (length(rank_colors) < n_ranks) {
      rank_colors_final <- colorRampPalette(rank_colors)(n_ranks)
      message(paste0("Interpolating ", length(rank_colors), " colors to ", n_ranks, " ranks"))
    } else if (length(rank_colors) > n_ranks) {
      rank_colors_final <- rank_colors[1:n_ranks]
      message(paste0("Using first ", n_ranks, " colors from provided colors"))
    } else {
      rank_colors_final <- rank_colors
      message("Using user-provided rank colors")
    }
  }
  
  # Default titles
  if (is.null(title)) {
    title <- paste0("Ranking Probabilities: ", data_rankinma$metrics.name)
  }
  
  if (is.null(subtitle)) {
    subtitle <- paste0(data_rankinma$ls.outcome,
                       " | Treatments ordered by ",
                       data_rankinma$metrics.name)
  }
  
  # Handle treatment colors consistently for both layouts
  if (!is.null(treatment_colors) && length(treatment_colors) > 0) {
    # Colors coming from outside (e.g. class‑based colors) win
    final_treatment_colors <- treatment_colors
    if (is.null(names(final_treatment_colors))) {
      names(final_treatment_colors) <- unique(rank_data$treatment)
    }
  } else {
    # Otherwise derive from selected treatment_palette
    final_treatment_colors <- create_treatment_colors(
      nma_obj,
      colors  = NULL,
      palette = treatment_palette,
      seed    = color_seed
    )
    if (is.null(names(final_treatment_colors))) {
      names(final_treatment_colors) <- unique(rank_data$treatment)
    }
  }
  
  rank_data$colorTx <- final_treatment_colors[rank_data$treatment]
  
  # Create plot based on layout
  if (layout == "stacked") {
    p <- create_stacked_layout(
      rank_data = rank_data,
      rank_colors = rank_colors_final,
      n_ranks = n_ranks,
      legend_position = legend_position,
      title = title,
      subtitle = subtitle,
      nsim = nsim,
      custom_theme = custom_theme
    )
  } else {
    p <- create_faceted_layout(
      rank_data = rank_data,
      # rank_colors = rank_colors_final,  # ✅ Pass rank colors
      n_ranks = n_ranks,
      legend_position = legend_position,
      title = title,
      subtitle = subtitle,
      nsim = nsim,
      custom_theme = custom_theme
    )
  }
  
  return(p)
}
