# ==============================================================================
# SCRIPT 2: nma_beading_plots.R
# Beading Plot Functions for Network Meta-Analysis Rankings
# ==============================================================================

#' Create Vertical Beading Plot
#'
#' Internal helper function to create a vertical layout beading plot showing
#' treatment rankings along the y-axis.
#'
#' @param plot_data Data frame with plot data including tx, metrics, colorTx
#' @param metrics_name Character string, name of the ranking metric (e.g., "SUCRA")
#' @param scale_type Character string: "Numeric" (actual metric values) or "Rank" (rank positions)
#' @param show_labels Logical, whether to show value labels
#' @param label_type Character string: "Metrics", "Effects", or "None"
#' @param point_size Numeric, size of beads
#' @param text_size Numeric, size of label text
#' @param colors Named vector of colors for treatments
#' @param legend_position Character string for legend position
#' @param title Character string for plot title
#' @param subtitle Character string for plot subtitle
#' @param custom_theme Optional ggplot2 theme object
#'
#' @return A ggplot2 object
#'
#' @keywords internal
create_vertical_beading <- function(plot_data, metrics_name, scale_type,
                                    show_labels, label_type, point_size,
                                    text_size, colors, legend_position,
                                    title, subtitle, custom_theme = NULL) {
  
  # Sort by metrics (descending)
  plot_data <- plot_data %>%
    dplyr::arrange(desc(metrics)) %>%
    dplyr::mutate(y_pos = row_number())
  
  # Prepare x-axis based on scale type
  if (scale_type == "Rank") {
    plot_data$x_pos <- plot_data$place
    x_label <- paste0("Rank according to ", metrics_name)
    x_breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
    x_axis_labels <- c("Last", "5th", "4th", "3rd", "2nd", "Best")
  } else {
    plot_data$x_pos <- plot_data$metrics
    x_label <- paste0(metrics_name, " Score")
    x_breaks <- seq(0, 1, 0.2)
    x_axis_labels <- scales::percent(x_breaks)
  }
  
  # Prepare labels
  if (show_labels && label_type == "Effects") {
    plot_data$label_text <- paste0(plot_data$measure, ": ", 
                                   round(plot_data$effect, 2))
    use_labels <- TRUE
  } else if (show_labels && label_type == "Metrics") {
    plot_data$label_text <- sprintf("%.1f%%", plot_data$metrics * 100)
    use_labels <- TRUE
  } else {
    plot_data$label_text <- ""
    use_labels <- FALSE
  }
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x_pos, y = y_pos)) +
    
    # Add horizontal lines from 0 to each bead
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = x_pos, y = y_pos, yend = y_pos),
                          color = "gray60", linewidth = 1, alpha = 0.7) +
    
    # Add the beads
    ggplot2::geom_point(ggplot2::aes(fill = tx),
                        shape = 21,
                        size = point_size,
                        color = "white",
                        stroke = 2,
                        show.legend = TRUE) +
    
    # Add labels without connector lines
    {if (show_labels && use_labels)
      ggrepel::geom_text_repel(
        ggplot2::aes(label = label_text),
        nudge_x = 0.05,
        direction = "x",
        hjust = 0,
        size = text_size,
        fontface = "bold",
        color = "gray25",
        segment.size = 0,
        segment.color = NA,
        box.padding = 0.3,
        point.padding = 0.2,
        min.segment.length = Inf
      )
    } +
    
    # Manual fill scale
    ggplot2::scale_fill_manual(
      values = setNames(plot_data$colorTx, plot_data$tx),
      name = "Treatment",
      guide = ggplot2::guide_legend(
        override.aes = list(
          size = 6,
          shape = 21,
          color = NA,
          stroke = 0
        ),
        nrow = 2,
        byrow = TRUE
      )
    ) +
    
    # X-axis
    ggplot2::scale_x_continuous(
      limits = c(0, 1.15),
      breaks = x_breaks,
      labels = x_axis_labels,
      expand = c(0.02, 0)
    ) +
    
    # Y-axis
    ggplot2::scale_y_continuous(
      breaks = plot_data$y_pos,
      labels = plot_data$tx,
      expand = c(0.05, 0)
    ) +
    
    # Labels
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = ""
    )
  
  # Apply custom theme or default theme
  if (!is.null(custom_theme)) {
    p <- p + custom_theme
  } else {
    p <- p + 
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 11, color = "gray40", hjust = 0,
                                              margin = ggplot2::margin(b = 15)),
        axis.text.y = ggplot2::element_text(size = 11, face = "bold", color = "black"),
        axis.text.x = ggplot2::element_text(size = 10),
        axis.title.x = ggplot2::element_text(size = 11, face = "bold", 
                                             margin = ggplot2::margin(t = 10)),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line(color = "gray90", linewidth = 0.3),
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        legend.position = legend_position,
        legend.title = ggplot2::element_text(face = "bold", size = 10),
        legend.text = ggplot2::element_text(size = 9),
        legend.box.background = ggplot2::element_rect(color = "gray70", linewidth = 0.5),
        legend.key = ggplot2::element_rect(fill = "white", color = NA),
        legend.margin = ggplot2::margin(10, 10, 10, 10)
      )
  }
  
  return(p)
}

#' Create Horizontal Beading Plot
#'
#' Internal helper function to create a horizontal layout beading plot showing
#' treatments along a single row.
#'
#' @inheritParams create_vertical_beading
#' @param label_angle Numeric, angle for label text rotation
#'
#' @return A ggplot2 object
#'
#' @keywords internal
create_horizontal_beading <- function(plot_data, metrics_name, scale_type,
                                      show_labels, label_type, point_size,
                                      text_size, label_angle, colors,
                                      legend_position, title, subtitle,
                                      custom_theme = NULL) {
  
  # Sort by metrics
  plot_data <- plot_data %>%
    dplyr::arrange(desc(metrics))
  
  # Prepare x-axis
  if (scale_type == "Rank") {
    plot_data$x_pos <- plot_data$place
    x_label <- paste0("Rank according to ", metrics_name)
  } else {
    plot_data$x_pos <- plot_data$metrics
    x_label <- paste0(metrics_name, " Score")
  }
  
  # Prepare labels
  if (show_labels && label_type == "Metrics") {
    plot_data$label_text <- sprintf("%.1f%%", plot_data$metrics * 100)
  } else if (show_labels && label_type == "Effects") {
    plot_data$label_text <- paste0(plot_data$measure, ": ", 
                                   round(plot_data$effect, 2))
  } else {
    plot_data$label_text <- ""
  }
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x_pos, y = 0.5)) +
    
    ggplot2::geom_hline(yintercept = 0.5, color = "gray70", linewidth = 0.5) +
    
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = x_pos, y = 0.5, yend = 0.5),
                          color = "gray60", linewidth = 1) +
    
    ggplot2::geom_point(ggplot2::aes(fill = tx),
                        shape = 21,
                        size = point_size,
                        color = "white",
                        stroke = 2,
                        show.legend = TRUE) +
    
    # Use ggrepel without connector lines
    {if (show_labels && label_type != "None")
      ggrepel::geom_text_repel(
        ggplot2::aes(label = label_text),
        nudge_y = -0.15,
        direction = "x",
        angle = label_angle,
        segment.size = 0,
        segment.color = NA,
        size = text_size,
        color = "gray25",
        fontface = "bold",
        box.padding = 0.5,
        point.padding = 0.3,
        min.segment.length = Inf
      )
    } +
    
    # Manual fill scale
    ggplot2::scale_fill_manual(
      values = setNames(plot_data$colorTx, plot_data$tx),
      name = "Treatment",
      guide = ggplot2::guide_legend(
        override.aes = list(
          size = 6,
          shape = 21,
          color = NA,
          stroke = 0
        ),
        nrow = 2,
        byrow = TRUE
      )
    ) +
    
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      labels = scales::percent,
      expand = c(0.02, 0)
    ) +
    
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    
    ggplot2::labs(title = title, subtitle = subtitle, x = x_label, y = "")
  
  # Apply custom theme or default
  if (!is.null(custom_theme)) {
    p <- p + custom_theme
  } else {
    p <- p + 
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5, color = "gray40",
                                              margin = ggplot2::margin(b = 15)),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(size = 11, face = "bold", 
                                             margin = ggplot2::margin(t = 10)),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        legend.position = legend_position,
        legend.title = ggplot2::element_text(face = "bold", size = 10),
        legend.text = ggplot2::element_text(size = 9),
        legend.box.background = ggplot2::element_rect(color = "gray70", linewidth = 0.5),
        legend.key = ggplot2::element_rect(fill = "white", color = NA)
      )
  }
  
  return(p)
}

#' Create Beading Plot for Network Meta-Analysis Rankings
#'
#' Creates a beading plot visualization showing treatment rankings with SUCRA or other
#' ranking metrics. Supports both vertical and horizontal layouts with flexible
#' color customization.
#'
#' @param data_rankinma A rankinma object from \code{\link{prepare_ranking_data}}
#' @param nma_obj Optional netmeta object for automatic color generation
#' @param scale_type Character string: "Numeric" (shows actual metric values 0-1) or
#'   "Rank" (shows rank positions). Default is "Numeric".
#' @param layout Character string: "vertical" (treatments on y-axis, recommended) or
#'   "horizontal" (treatments on single row). Default is "vertical".
#' @param show_labels Logical, whether to display value labels near beads. Default is TRUE.
#' @param label_type Character string: "Metrics" (shows SUCRA/P-score values),
#'   "Effects" (shows treatment effects), or "None". Default is "Metrics".
#' @param point_size Numeric, size of bead points. Default is 12.
#' @param text_size Numeric, size of label text. Default is 3.5.
#' @param label_angle Numeric, angle for label rotation in horizontal layout. Default is 45.
#' @param colors Optional named vector of colors for treatments. If NULL, colors are
#'   auto-generated using color_palette. Can be named (best) or unnamed vector.
#' @param color_palette Character string specifying auto-generation palette.
#'   Options: "default", "colorblind", "vibrant", "pastel", "dark", "rainbow",
#'   "viridis", "plasma", "tableau". Default is "default".
#' @param color_seed Integer for reproducible color generation. Default is 123.
#' @param legend_position Character string for legend position: "bottom", "top",
#'   "left", "right", or "none". Default is "bottom".
#' @param title Optional character string for plot title. If NULL, auto-generated.
#' @param subtitle Optional character string for plot subtitle. If NULL, auto-generated.
#' @param custom_theme Optional ggplot2 theme object to override default styling.
#'
#' @return A ggplot2 object
#'
#' @details
#' The beading plot displays treatments as colored beads positioned according to their
#' ranking metric (SUCRA, P-score, or P-best). Horizontal lines connect from zero
#' to each bead, providing visual reference for the metric values.
#'
#' Color handling:
#' \itemize{
#'   \item If \code{colors} is provided, uses those colors (validates against treatments)
#'   \item If \code{nma_obj} is provided without colors, auto-generates from palette
#'   \item If neither, uses treatment names from data to generate colors
#' }
#'
#' @examples
#' \dontrun{
#' # Prepare data
#' data_sucra <- prepare_ranking_data(nma_con, "MeanDifference", "small", "SUCRA")
#'
#' # Auto colors with colorblind palette
#' p1 <- plot_beading(data_sucra, nma_obj = nma_con, color_palette = "colorblind")
#'
#' # Manual colors
#' my_colors <- c("Drug A" = "#FF0000", "Drug B" = "#00FF00", "Placebo" = "#0000FF")
#' p2 <- plot_beading(data_sucra, nma_obj = nma_con, colors = my_colors)
#'
#' # Horizontal layout
#' p3 <- plot_beading(data_sucra, nma_obj = nma_con, layout = "horizontal")
#' }
#'
#' @seealso \code{\link{prepare_ranking_data}}, \code{\link{create_treatment_colors}}
#'
#' @export
plot_beading <- function(data_rankinma,
                         nma_obj = NULL,
                         scale_type = c("Numeric", "Rank"),
                         layout = c("vertical", "horizontal"),
                         show_labels = TRUE,
                         label_type = c("Metrics", "Effects", "None"),
                         point_size = 12,
                         text_size = 3.5,
                         label_angle = 45,
                         colors = NULL,
                         color_palette = "default",
                         color_seed = 123,
                         legend_position = "bottom",
                         title = NULL,
                         subtitle = NULL,
                         custom_theme = NULL) {
  
  # Match arguments
  scale_type <- match.arg(scale_type)
  layout <- match.arg(layout)
  label_type <- match.arg(label_type)
  
  # Extract data
  plot_data <- data_rankinma$data
  metrics_name <- data_rankinma$metrics.name
  treatment_names <- data_rankinma$ls.tx
  
  # Handle colors - flexible system
  if (!is.null(nma_obj)) {
    # Use smart dispatcher with nma object
    final_colors <- create_treatment_colors(
      nma_obj,
      colors = colors,
      palette = color_palette,
      seed = color_seed
    )
  } else {
    # Fallback without nma object
    if (!is.null(colors)) {
      if (is.null(names(colors))) {
        final_colors <- setNames(colors, treatment_names)
        message("Using user-provided unnamed colors")
      } else {
        final_colors <- colors
        message("Using user-provided named colors")
      }
    } else {
      final_colors <- generate_treatment_colors(
        treatment_names,
        palette = color_palette,
        seed = color_seed
      )
      message(paste0("Auto-generating colors using '", color_palette, "' palette"))
    }
  }
  
  # Apply colors to plot data
  plot_data <- plot_data %>%
    dplyr::mutate(colorTx = final_colors[tx])
  
  # Default titles
  if (is.null(title)) {
    title <- paste0("Beading Plot of ", metrics_name)
  }
  if (is.null(subtitle)) {
    subtitle <- data_rankinma$ls.outcome
  }
  
  # Create plot based on layout
  if (layout == "vertical") {
    p <- create_vertical_beading(
      plot_data = plot_data,
      metrics_name = metrics_name,
      scale_type = scale_type,
      show_labels = show_labels,
      label_type = label_type,
      point_size = point_size,
      text_size = text_size,
      colors = final_colors,
      legend_position = legend_position,
      title = title,
      subtitle = subtitle,
      custom_theme = custom_theme
    )
  } else {
    p <- create_horizontal_beading(
      plot_data = plot_data,
      metrics_name = metrics_name,
      scale_type = scale_type,
      show_labels = show_labels,
      label_type = label_type,
      point_size = point_size,
      text_size = text_size,
      label_angle = label_angle,
      colors = final_colors,
      legend_position = legend_position,
      title = title,
      subtitle = subtitle,
      custom_theme = custom_theme
    )
  }
  
  return(p)
}
