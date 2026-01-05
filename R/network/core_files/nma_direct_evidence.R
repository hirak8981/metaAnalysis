#' Extract Direct Evidence Proportion from Network Meta-Analysis
#'
#' @description
#' Extracts network measures including proportion of direct evidence,
#' mean path length, and minimal parallelism from a netmeta object.
#'
#' @param nma A netmeta object from the netmeta package
#' @param model Character; "random" or "common" effects model (default: "random")
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item comparison: Treatment comparison
#'     \item proportion_direct: Proportion of direct evidence (0-1)
#'     \item proportion_indirect: Proportion of indirect evidence (0-1)
#'     \item mean_path_length: Average path length in network
#'     \item minimal_parallelism: Minimal parallelism measure
#'   }
#'
#' @details
#' Direct evidence proportion indicates how much of the evidence for a comparison
#' comes from direct head-to-head trials vs indirect comparisons through the network.
#' Mean path length and minimal parallelism are measures of network geometry and
#' evidence flow.
#'
#' @export
#'
#' @examples
#' library(netmeta)
#' data(smokingcessation)
#' nma <- netmeta(TE, seTE, treat1, treat2, studlab, data = smokingcessation, sm = "OR")
#' 
#' # Extract evidence measures
#' evidence_data <- extract_direct_evidence_netmeta(nma, model = "random")
#' print(evidence_data)
#' 
#' # View top 10 comparisons by direct evidence
#' head(evidence_data, 10)

extract_direct_evidence_netmeta <- function(
    nma,
    model = c("random", "common")
) {
  
  # Validate input
  if (!inherits(nma, "netmeta")) {
    stop("nma must be a netmeta object from the netmeta package")
  }
  
  # Match model argument
  model <- match.arg(model)
  random <- (model == "random")
  
  # Calculate network measures
  nm <- netmeta::netmeasures(nma, random = random)
  
  # Extract and format data
  df <- tibble::tibble(
    comparison = names(nm$proportion),
    proportion_direct   = as.numeric(nm$proportion),
    proportion_indirect = 1 - as.numeric(nm$proportion),
    mean_path_length    = as.numeric(nm$meanpath),
    minimal_parallelism = as.numeric(nm$minpar),
    model = model
  )
  
  # Sort by proportion of direct evidence (descending)
  df <- df |>
    dplyr::arrange(dplyr::desc(proportion_direct)) |>
    dplyr::mutate(
      comparison = factor(comparison, levels = comparison)
    )
  
  return(df)
}


#' Plot Network Evidence Measures
#'
#' @description
#' Creates visualization of direct vs indirect evidence proportions,
#' minimal parallelism, and mean path length for network meta-analysis comparisons.
#'
#' @param data Data frame from extract_direct_evidence_netmeta() or with columns:
#'   comparison, proportion_direct, proportion_indirect, mean_path_length, minimal_parallelism
#' @param direct_col Color for direct evidence bars (default: "#E69F00")
#' @param indirect_col Color for indirect evidence bars (default: "#56B4E9")
#' @param bar_col Color for metric bars (default: "grey80")
#' @param cutoff_col Color for mean path length cutoff line (default: "#0072B2")
#' @param meanpath_cutoff Cutoff value for mean path length (default: 2)
#' @param numeric_label_size Size of numeric labels on bars (default: 3)
#' @param theme_base ggplot2 theme function (default: theme_minimal(base_size = 11))
#' @param show_values Logical; show numeric values on bars (default: TRUE)
#'
#' @return A list with three ggplot2 objects:
#'   \itemize{
#'     \item direct_evidence: Stacked bar chart of direct vs indirect evidence
#'     \item minimal_parallelism: Bar chart of minimal parallelism
#'     \item mean_path_length: Bar chart of mean path length with cutoff line
#'   }
#'
#' @details
#' The function creates three complementary plots:
#' \itemize{
#'   \item Direct evidence: Shows proportion of direct vs indirect evidence
#'   \item Minimal parallelism: Lower values indicate more independent evidence paths
#'   \item Mean path length: Values >2 suggest reliance on indirect comparisons
#' }
#'
#' @export
#'
#' @examples
#' library(netmeta)
#' library(patchwork)
#' 
#' # Extract and plot
#' evidence_data <- extract_direct_evidence_netmeta(nma)
#' plots <- plot_direct_evidence_netmeta(evidence_data)
#' 
#' # Combine plots
#' plots$direct_evidence + plots$minimal_parallelism + plots$mean_path_length
#' 
#' # Custom colors
#' plots <- plot_direct_evidence_netmeta(
#'   evidence_data,
#'   direct_col = "darkgreen",
#'   indirect_col = "coral"
#' )
#' 
#' # Custom theme
#' my_theme <- theme_bw(base_size = 12)
#' plots <- plot_direct_evidence_netmeta(evidence_data, theme_base = my_theme)

plot_direct_evidence_netmeta <- function(
    data,
    
    # Colors
    direct_col   = "#E69F00",
    indirect_col = "#56B4E9",
    bar_col      = "grey80",
    cutoff_col   = "#0072B2",
    
    # Options
    meanpath_cutoff = 2,
    numeric_label_size = 3,
    theme_base = NULL,
    show_values = TRUE
) {
  
  # Validate input
  required_cols <- c("comparison", "proportion_direct", "proportion_indirect", 
                     "mean_path_length", "minimal_parallelism")
  if (!all(required_cols %in% names(data))) {
    stop("Data must contain columns: ", paste(required_cols, collapse = ", "))
  }
  
  # Set default theme if not provided
  if (is.null(theme_base)) {
    theme_base <- ggplot2::theme_minimal(base_size = 11)
  }
  
  # Create individual plots
  p_direct <- .plot_direct_evidence(
    data, direct_col, indirect_col, theme_base
  )
  
  p_parallel <- .plot_minimal_parallelism(
    data, bar_col, numeric_label_size, theme_base, show_values
  )
  
  p_meanpath <- .plot_mean_path_length(
    data, bar_col, cutoff_col, meanpath_cutoff, 
    numeric_label_size, theme_base, show_values
  )
  
  # Return list of plots
  list(
    direct_evidence     = p_direct,
    minimal_parallelism = p_parallel,
    mean_path_length    = p_meanpath
  )
}


#' Internal: Plot direct vs indirect evidence proportion
#' @keywords internal
.plot_direct_evidence <- function(data, direct_col, indirect_col, theme_base) {
  
  # Reshape data for stacked bar chart
  df_long <- data |>
    tidyr::pivot_longer(
      cols = c(proportion_direct, proportion_indirect),
      names_to = "type",
      values_to = "value"
    )
  
  # Create plot
  p <- ggplot2::ggplot(
    df_long,
    ggplot2::aes(x = comparison, y = value, fill = type)
  ) +
    ggplot2::geom_col(position = "fill", width = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c(
        proportion_direct   = direct_col,
        proportion_indirect = indirect_col
      ),
      labels = c("Direct evidence", "Indirect evidence"),
      name = NULL
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1)
    ) +
    ggplot2::labs(
      x = "Network comparison",
      y = "Evidence proportion"
    ) +
    theme_base +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "top",
      axis.text.y = ggplot2::element_text(hjust = 0, size = 12),
      axis.text.x = ggplot2::element_text(hjust = 0, size = 12)
    )
  
  return(p)
}


#' Internal: Plot minimal parallelism
#' @keywords internal
.plot_minimal_parallelism <- function(data, bar_col, label_size, theme_base, show_values) {
  
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = comparison, y = minimal_parallelism)
  ) +
    ggplot2::geom_col(fill = bar_col, width = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Minimal parallelism"
    ) +
    theme_base +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(hjust = 0, size = 12),
      panel.grid.major.y = ggplot2::element_blank()
    )
  
  # Add numeric labels if requested
  if (show_values) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = round(minimal_parallelism, 1)),
      hjust = -0.2,
      size = 5
    )
  }
  
  return(p)
}


#' Internal: Plot mean path length with cutoff line
#' @keywords internal
.plot_mean_path_length <- function(data, bar_col, cutoff_col, cutoff, 
                                   label_size, theme_base, show_values) {
  
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = comparison, y = mean_path_length)
  ) +
    ggplot2::geom_col(fill = bar_col, width = 0.8) +
    ggplot2::geom_hline(
      yintercept = cutoff,
      color = cutoff_col,
      linetype = "dashed",
      linewidth = 0.8
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Mean path length"
    ) +
    theme_base +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(hjust = 0, size = 8),
      panel.grid.major.y = ggplot2::element_blank()
    )
  
  # Add numeric labels if requested
  if (show_values) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = round(mean_path_length, 1)),
      hjust = -0.2,
      size = 5
    )
  }
  
  # Add cutoff annotation
  p <- p + ggplot2::annotate(
    "text",
    x = nrow(data) * 0.05,
    y = cutoff,
    label = paste("Cutoff =", cutoff),
    hjust = 0,
    vjust = -0.5,
    size = 3,
    color = cutoff_col,
    fontface = "italic"
  )
  
  return(p)
}


#' Combine Network Evidence Plots
#'
#' @description
#' Convenience wrapper to extract evidence data and create combined plot in one step.
#'
#' @param nma A netmeta object
#' @param model Character; "random" or "common" effects (default: "random")
#' @param layout patchwork layout specification (default: plots side-by-side)
#' @param ... Additional arguments passed to plot_direct_evidence_netmeta()
#'
#' @return A patchwork object combining the three plots
#' @export
#'
#' @examples
#' library(netmeta)
#' library(patchwork)
#' 
#' # One-step plotting
#' plot_network_evidence(nma, model = "random")
#' 
#' # With custom layout
#' plot_network_evidence(nma, layout = c(1, 3))  # Vertical stack
#' 
#' # With custom colors
#' plot_network_evidence(
#'   nma,
#'   direct_col = "darkgreen",
#'   indirect_col = "coral"
#' )

plot_network_evidence <- function(
    nma,
    model = c("random", "common"),
    layout = NULL,
    ...
) {
  
  # Validate patchwork is available
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required for this function. Install it with: install.packages('patchwork')")
  }
  
  # Extract evidence data
  evidence_data <- extract_direct_evidence_netmeta(nma, model = model)
  
  # Create plots
  plots <- plot_direct_evidence_netmeta(evidence_data, ...)
  
  # Combine plots
  if (is.null(layout)) {
    # Default: side-by-side
    combined <- plots$direct_evidence + 
      plots$minimal_parallelism + 
      plots$mean_path_length
  } else {
    combined <- patchwork::wrap_plots(
      plots$direct_evidence,
      plots$minimal_parallelism,
      plots$mean_path_length,
      ncol = layout[2],
      nrow = layout[1]
    )
  }
  
  return(combined)
}



