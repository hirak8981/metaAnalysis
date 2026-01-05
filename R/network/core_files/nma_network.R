# ==============================================================================
# Network Graph Visualization for Network Meta-Analysis (UPDATED)
# ==============================================================================

#' Create a Network Graph for Network Meta-Analysis
#'
#' @param nma A netmeta object from the netmeta package
#' @param treatment_classes Optional data frame containing treatment classifications
#' @param trt_col Column name (quoted string or unquoted) for treatment names
#' @param class_col Column name (quoted string or unquoted) for treatment class labels
#' @param use_treatment_colors Logical, color by individual treatments (TRUE) or classes (FALSE)
#' @param treatment_colors Optional named vector of colors for individual treatments
#' @param color_palette Character string specifying auto-generation palette
#' @param color_seed Integer for reproducible color generation
#' @param class_palette Named character vector of colors for treatment classes
#' @param edge_color Character string specifying edge color
#' @param node_border_color Character string specifying node border color
#' @param node_size_range Numeric vector of length 2 for min/max node sizes
#' @param edge_width_range Numeric vector of length 2 for min/max edge widths
#' @param label_size Numeric, size of treatment labels
#' @param title Character string for plot title
#' @param subtitle Optional character string for plot subtitle
#'
#' @return A ggplot2 object representing the network graph
#'
#' @export
ggplot_netgraph <- function(
    nma,
    treatment_classes = NULL,
    trt_col = NULL,
    class_col = NULL,
    use_treatment_colors = FALSE,
    treatment_colors = NULL,
    color_palette = "default",
    color_seed = 123,
    class_palette = c(
      "Control" = "#E0E0E0",
      "Active" = "#4DAF4A"
    ),
    edge_color = "grey60",
    node_border_color = "#333333",
    node_size_range = c(6, 14),
    edge_width_range = c(0.5, 3),
    label_size = 4,
    title = "Treatment Network",
    subtitle = NULL
) {
  
  # Validate input
  stopifnot(inherits(nma, "netmeta"))
  
  # ============================================================
  # 1. Compute network geometry
  # ============================================================
  
  ng <- netmeta::netgraph(nma, figure = FALSE)
  nodes <- ng$nodes
  
  # ============================================================
  # 2. Attach treatment classes
  # ============================================================
  
  if (!is.null(treatment_classes)) {
    if (is.null(trt_col) || is.null(class_col)) {
      stop("When 'treatment_classes' is provided, both 'trt_col' and 'class_col' must be specified.")
    }
    
    # Accept both quoted strings and unquoted names
    if (is.character(trt_col)) {
      trt_col_name <- trt_col
      class_col_name <- class_col
    } else {
      trt_col_name <- as.character(rlang::ensym(trt_col))
      class_col_name <- as.character(rlang::ensym(class_col))
    }
    
    # Verify columns exist
    if (!trt_col_name %in% names(treatment_classes)) {
      stop("Column '", trt_col_name, "' not found in treatment_classes")
    }
    if (!class_col_name %in% names(treatment_classes)) {
      stop("Column '", class_col_name, "' not found in treatment_classes")
    }
    
    # Rename columns to standard names
    class_df <- treatment_classes
    names(class_df)[names(class_df) == trt_col_name] <- "trts"
    names(class_df)[names(class_df) == class_col_name] <- "class"
    class_df <- class_df[, c("trts", "class"), drop = FALSE]
    
    # Join class information to nodes
    nodes <- dplyr::left_join(nodes, class_df, by = "trts")
    
  } else {
    # Automatic classification if not provided
    nodes$class <- dplyr::case_when(
      nodes$trts %in% c("Placebo", "Control") ~ "Control",
      TRUE ~ "Active"
    )
  }
  
  # Convert class to factor
  nodes$class <- factor(nodes$class, levels = names(class_palette))
  
  # ============================================================
  # 3. Handle treatment colors
  # ============================================================
  
  if (use_treatment_colors) {
    if (is.null(treatment_colors)) {
      final_treatment_colors <- create_treatment_colors(
        nma, colors = NULL, palette = color_palette, seed = color_seed
      )
    } else {
      final_treatment_colors <- create_treatment_colors(
        nma, colors = treatment_colors, palette = color_palette, seed = color_seed
      )
    }
    nodes <- nodes |> dplyr::mutate(color_fill = final_treatment_colors[trts])
  } else {
    nodes <- nodes |> dplyr::mutate(color_fill = class_palette[as.character(class)])
  }
  
  # ============================================================
  # 4. Label offsets
  # ============================================================
  
  nodes$label_dx <- cos(atan2(nodes$offset.y, nodes$offset.x)) * (nodes$cex * 0.035 + 0.05)
  nodes$label_dy <- sin(atan2(nodes$offset.y, nodes$offset.x)) * (nodes$cex * 0.035 + 0.05)
  
  # ============================================================
  # 5. Edge geometry
  # ============================================================
  
  edges <- ng$edges |>
    dplyr::left_join(nodes |> dplyr::select(trts, x1 = xpos, y1 = ypos), by = c("treat1" = "trts")) |>
    dplyr::left_join(nodes |> dplyr::select(trts, x2 = xpos, y2 = ypos), by = c("treat2" = "trts"))
  
  # ============================================================
  # 6. Build ggplot (UPDATED - with ggrepel for non-overlapping labels)
  # ============================================================
  
  # Check for ggrepel package
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    warning("Package 'ggrepel' not found. Labels may overlap. Install with: install.packages('ggrepel')")
    use_repel <- FALSE
  } else {
    use_repel <- TRUE
  }
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2, linewidth = n.stud),
      colour = edge_color, alpha = 0.8
    ) +
    ggplot2::geom_point(
      data = nodes,
      ggplot2::aes(x = xpos, y = ypos, size = cex, fill = color_fill),
      shape = 21, colour = node_border_color, stroke = 0.8
    )
  
  # Add labels with or without repel
  if (use_repel) {
    p <- p +
      ggrepel::geom_text_repel(
        data = nodes,
        ggplot2::aes(x = xpos, y = ypos, label = labels),
        size = label_size,
        fontface = "bold",
        box.padding = 1.0,        # Space between label and point
        point.padding = 0.3,      # Extra padding around point
        force = 2,                # Repulsion strength from other labels
        force_pull = 0.6,         # Attraction to original position
        max.overlaps = 20,        # Allow resolving many overlaps
        segment.color = NA,       # Color of connector line
        min.segment.length = 0,   # Always show connector
        seed = 123                # Reproducible positioning
      )
  } else {
    p <- p +
      ggplot2::geom_text(
        data = nodes,
        ggplot2::aes(x = xpos + label_dx, y = ypos + label_dy, label = labels, hjust = adj.x, vjust = adj.y),
        size = label_size, fontface = "bold"
      )
  }
  
  # Continue with scales and theme
  p <- p +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_size_continuous(name = "Studies per treatment", range = node_size_range) +
    ggplot2::scale_linewidth_continuous(name = "Studies per comparison", range = edge_width_range, breaks = scales::pretty_breaks(n = 4)) +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::expand_limits(x = range(nodes$xpos) * 1.35, y = range(nodes$ypos) * 1.35) +  # Increased from 1.25
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right",
      legend.box = "vertical",
      legend.title = ggplot2::element_text(size = 11, face = "bold"),
      legend.text = ggplot2::element_text(size = 10),
      legend.box.margin = ggplot2::margin(t = 0, r = 40, b = 0, l = 20),
      legend.margin = ggplot2::margin(5, 5, 5, 5),
      legend.key.height = grid::unit(8, "mm"),
      legend.key.width = grid::unit(8, "mm"),
      legend.spacing.y = grid::unit(4, "mm"),
      plot.margin = ggplot2::margin(15, 80, 15, 15),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
  
  
  # ============================================================
  # 7. Add legends (UPDATED - NO NODE SIZE)
  # ============================================================
  
  if (use_treatment_colors) {
    legend_data <- data.frame(
      treatment = names(final_treatment_colors),
      color = as.character(final_treatment_colors),
      stringsAsFactors = FALSE
    )
    
    p <- p +
      ggplot2::geom_point(
        data = legend_data,
        ggplot2::aes(x = -Inf, y = -Inf, fill = color),
        shape = 21, size = 6, alpha = 0
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          title = "Treatment",
          title.position = "top",
          override.aes = list(alpha = 1, size = 6, shape = 21, color = node_border_color, stroke = 0.8),
          order = 1
        ),
        size = "none",  # REMOVED
        linewidth = ggplot2::guide_legend(
          title = "Studies per comparison",
          title.position = "top",
          override.aes = list(color = edge_color),
          order = 2
        )
      )
    
  } else {
    legend_data <- data.frame(
      class = names(class_palette),
      color = as.character(class_palette),
      stringsAsFactors = FALSE
    )
    
    p <- p +
      ggplot2::geom_point(
        data = legend_data,
        ggplot2::aes(x = -Inf, y = -Inf, fill = color),
        shape = 21, size = 6, alpha = 0
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          title = "Treatment class",
          title.position = "top",
          override.aes = list(alpha = 1, size = 6, shape = 21, color = node_border_color, stroke = 0.8),
          order = 1
        ),
        size = "none",  # REMOVED
        linewidth = ggplot2::guide_legend(
          title = "Studies per comparison",
          title.position = "top",
          override.aes = list(color = edge_color),
          order = 2
        )
      )
  }
  
  return(p)
}
