
#' Create Forest Plot for Network Meta-Analysis
#'
#' @description
#' Generates a forest plot from netmeta results with automatic data extraction
#' and proper handling of different effect measures (OR, RR, MD, SMD).
#'
#' @param x Either a netmeta object or a data frame with columns: treatment, TE, lower, upper
#' @param model Character; "random" or "common" effects model (default: "random")
#' @param ref Character; reference treatment. If NULL, uses netmeta's reference group
#' @param sm Character; summary measure - "OR", "RR", "MD", or "SMD"
#' @param title Character; plot title
#' @param point_color Color for point estimates (default: "#003049")
#' @param ci_color Color for confidence intervals (default: "#003049")
#' @param point_size Size of point estimates (default: 3)
#' @param ci_lwd Line width for confidence intervals (default: 0.6)
#' @param digits Number of decimal places (default: 2)
#' @param auto_detect_sm Logical; automatically detect summary measure from netmeta object (default: TRUE)
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' library(netmeta)
#' library(ggplot2)
#' 
#' # From netmeta object directly
#' nma <- netmeta(TE, seTE, treat1, treat2, studlab, data = smokcessation)
#' forest_netmeta(nma, title = "Smoking Cessation NMA")
#' 
#' # From extracted data
#' forest_data <- extract_forest_data_netmeta(nma)
#' forest_netmeta(forest_data, sm = "OR")

#' forest_netmeta <- function(
#'     x,
#'     model = "random",
#'     ref = NULL,
#'     sm = NULL,
#'     title = NULL,
#'     point_color = "#003049",
#'     ci_color = "#003049",
#'     point_size = 3,
#'     ci_lwd = 0.6,
#'     digits = 2,
#'     auto_detect_sm = TRUE
#' ) {
#'   
#'   # Validate and prepare data
#'   if (inherits(x, "netmeta")) {
#'     # Auto-detect summary measure if not provided
#'     if (is.null(sm) && auto_detect_sm) {
#'       sm <- x$sm
#'     }
#'     
#'     # Extract forest data
#'     df <- extract_forest_data_netmeta(x, model = model, ref = ref)
#'     
#'   } else if (is.data.frame(x)) {
#'     # Validate required columns
#'     required_cols <- c("treatment", "TE", "lower", "upper")
#'     if (!all(required_cols %in% names(x))) {
#'       stop("Data frame must contain columns: ", paste(required_cols, collapse = ", "))
#'     }
#'     df <- x
#'     
#'   } else {
#'     stop("x must be a netmeta object or a data frame with columns: treatment, TE, lower, upper")
#'   }
#'   
#'   # Validate summary measure
#'   if (is.null(sm)) {
#'     stop("sm must be specified when x is a data frame")
#'   }
#'   sm <- match.arg(sm, c("OR", "RR", "MD", "SMD", "HR", "IRR", "ROM"))
#'   
#'   # Create the plot
#'   .plot_forest_netmeta(
#'     df = df,
#'     sm = sm,
#'     title = title,
#'     point_color = point_color,
#'     ci_color = ci_color,
#'     point_size = point_size,
#'     ci_lwd = ci_lwd,
#'     digits = digits
#'   )
#' }
#' 
#' 
#' #' Extract Forest Plot Data from netmeta Object
#' #'
#' #' @description
#' #' Extracts treatment effects, confidence intervals, and treatment names
#' #' from a netmeta object for forest plot creation.
#' #'
#' #' @param x A netmeta object
#' #' @param model Character; "random" or "common" effects model (default: "random")
#' #' @param ref Character; reference treatment. If NULL, uses netmeta's reference group
#' #'
#' #' @return A tibble with columns: treatment, TE, lower, upper
#' #' @export
#' #'
#' #' @examples
#' #' library(netmeta)
#' #' nma <- netmeta(TE, seTE, treat1, treat2, studlab, data = smokcessation)
#' #' extract_forest_data_netmeta(nma)
#' #' extract_forest_data_netmeta(nma, model = "common", ref = "A")
#' 
#' extract_forest_data_netmeta <- function(x, model = "random", ref = NULL) {
#'   
#'   # Validate input
#'   if (!inherits(x, "netmeta")) {
#'     stop("x must be a netmeta object from netmeta package")
#'   }
#'   
#'   # Validate model
#'   model <- match.arg(model, c("random", "common"))
#'   
#'   # Extract matrices
#'   TE_mat    <- x[[paste0("TE.", model)]]
#'   lower_mat <- x[[paste0("lower.", model)]]
#'   upper_mat <- x[[paste0("upper.", model)]]
#'   
#'   # Determine reference treatment
#'   if (is.null(ref)) {
#'     ref <- x$reference.group
#'     if (is.null(ref)) {
#'       ref <- rownames(TE_mat)[1]
#'       message("Using '", ref, "' as reference treatment")
#'     }
#'   }
#'   
#'   # Validate reference treatment
#'   if (!ref %in% rownames(TE_mat)) {
#'     stop("Reference treatment '", ref, "' not found in network")
#'   }
#'   
#'   # Filter out reference treatment
#'   keep <- rownames(TE_mat) != ref
#'   
#'   # Build tidy data frame
#'   df <- tibble::tibble(
#'     treatment = rownames(TE_mat)[keep],
#'     TE        = TE_mat[keep, ref],
#'     lower     = lower_mat[keep, ref],
#'     upper     = upper_mat[keep, ref],
#'     reference = ref,
#'     model     = model
#'   )
#'   
#'   return(df)
#' }
#' 
#' 
#' #' Internal: Create forest plot from prepared data
#' #' @keywords internal
#' .plot_forest_netmeta <- function(
#'     df,
#'     sm,
#'     title,
#'     point_color,
#'     ci_color,
#'     point_size,
#'     ci_lwd,
#'     digits
#' ) {
#'   
#'   # Prepare data for plotting
#'   df <- df |>
#'     dplyr::mutate(
#'       # Format CI labels
#'       label = sprintf(
#'         paste0("%.", digits, "f [%.", digits, "f; %.", digits, "f]"),
#'         TE, lower, upper
#'       ),
#'       # Reverse factor levels for top-to-bottom plotting
#'       treatment = factor(treatment, levels = rev(treatment))
#'     )
#'   
#'   # Determine reference line position
#'   ref_line <- if (sm %in% c("OR", "RR", "HR", "IRR", "ROM")) 1 else 0
#'   
#'   # X-axis configuration
#'   if (sm %in% c("OR", "RR", "HR", "IRR", "ROM")) {
#'     # Log scale for ratio measures
#'     scale_x <- ggplot2::scale_x_log10(
#'       breaks = c(0.25, 0.5, 1, 2, 4),
#'       labels = scales::number_format(accuracy = 0.01)
#'     )
#'     # Position for text annotation (multiplicative)
#'     x_text <- max(df$upper, na.rm = TRUE) * 1.6
#'   } else {
#'     # Linear scale for difference measures
#'     scale_x <- ggplot2::scale_x_continuous()
#'     # Position for text annotation (additive)
#'     x_text <- max(df$upper, na.rm = TRUE) + 
#'       0.25 * diff(range(c(df$lower, df$upper), na.rm = TRUE))
#'   }
#'   
#'   # Create plot
#'   p <- ggplot2::ggplot(df, ggplot2::aes(y = treatment)) +
#'     # Confidence intervals
#'     ggplot2::geom_errorbar(
#'       ggplot2::aes(xmin = lower, xmax = upper),
#'       width = 0.15,
#'       linewidth = ci_lwd,
#'       color = ci_color,
#'       orientation = "y"
#'     ) +
#'     # Point estimates
#'     ggplot2::geom_point(
#'       ggplot2::aes(x = TE),
#'       size = point_size,
#'       color = point_color
#'     ) +
#'     # Reference line (null effect)
#'     ggplot2::geom_vline(
#'       xintercept = ref_line,
#'       linetype = "dashed",
#'       color = "grey40"
#'     ) +
#'     # CI text labels
#'     ggplot2::geom_text(
#'       ggplot2::aes(x = x_text, label = label),
#'       hjust = 0,
#'       size = 3.5
#'     ) +
#'     # Apply x-scale
#'     scale_x +
#'     # Labels
#'     ggplot2::labs(
#'       title = title,
#'       x = sm,
#'       y = NULL
#'     ) +
#'     # Extend plot area for text
#'     ggplot2::coord_cartesian(
#'       clip = "off",
#'       xlim = c(
#'         min(df$lower, na.rm = TRUE) * 0.95,
#'         x_text * 1.15
#'       )
#'     ) +
#'     # Theme
#'     ggplot2::theme_minimal(base_size = 12) +
#'     ggplot2::theme(
#'       panel.grid.major.y = ggplot2::element_blank(),
#'       panel.grid.minor = ggplot2::element_blank(),
#'       axis.text.y = ggplot2::element_text(hjust = 0),
#'       plot.margin = ggplot2::margin(5.5, 60, 5.5, 5.5),
#'       plot.title = element_text(hjust = 0.5, face = "bold")
#'     ) +
#'     # Column header
#'     ggplot2::annotate(
#'       "text",
#'       x = x_text,
#'       y = length(levels(df$treatment)) + 0.7,
#'       label = paste0(sm, " [95% CI]"),
#'       fontface = "bold",
#'       hjust = 0,
#'       size = 3.5
#'     )
#'   
#'   return(p)
#' }
#' 
#' 
#' #' Check if netmeta object uses log scale
#' #' @keywords internal
#' .needs_log_scale <- function(nma) {
#'   if (!inherits(nma, "netmeta")) return(FALSE)
#'   nma$sm %in% c("OR", "RR", "HR", "IRR", "ROM")
#' }
#' 
#' 
# ==============================================================================
# Forest Plot for Network Meta-Analysis
# ==============================================================================

#' Create Forest Plot for Network Meta-Analysis
#'
#' @description
#' Generates a forest plot from netmeta results with automatic data extraction
#' and proper handling of different effect measures (OR, RR, MD, SMD).
#'
#' @param x Either a netmeta object or a data frame with columns: treatment, TE, lower, upper
#' @param model Character; "random" or "common" effects model (default: "random")
#' @param ref Character; reference treatment. If NULL, uses netmeta's reference group
#' @param sm Character; summary measure - "OR", "RR", "MD", or "SMD"
#' @param title Character; plot title
#' @param point_color Color for point estimates (default: "#003049")
#' @param ci_color Color for confidence intervals (default: "#003049")
#' @param point_size Size of point estimates (default: 3)
#' @param ci_lwd Line width for confidence intervals (default: 0.6)
#' @param digits Number of decimal places (default: 2)
#' @param auto_detect_sm Logical; automatically detect summary measure from netmeta object (default: TRUE)
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' library(netmeta)
#' library(ggplot2)
#'
#' # From netmeta object directly
#' nma <- netmeta(TE, seTE, treat1, treat2, studlab, data = smokcessation)
#' forest_netmeta(nma, title = "Smoking Cessation NMA")
#'
#' # From extracted data
#' forest_data <- extract_forest_data_netmeta(nma)
#' forest_netmeta(forest_data, sm = "OR")
#' }
forest_netmeta <- function(
    x,
    model = "random",
    ref = NULL,
    sm = NULL,
    title = NULL,
    point_color = "#003049",
    ci_color = "#003049",
    point_size = 3,
    ci_lwd = 0.6,
    digits = 2,
    auto_detect_sm = TRUE
) {
  
  # Validate and prepare data
  if (inherits(x, "netmeta")) {
    # Auto-detect summary measure if not provided
    if (is.null(sm) && auto_detect_sm) {
      sm <- x$sm
    }
    
    # Extract forest data
    df <- extract_forest_data_netmeta(x, model = model, ref = ref)
    
  } else if (is.data.frame(x)) {
    # Validate required columns
    required_cols <- c("treatment", "TE", "lower", "upper")
    if (!all(required_cols %in% names(x))) {
      stop("Data frame must contain columns: ", paste(required_cols, collapse = ", "))
    }
    
    df <- x
    
  } else {
    stop("x must be a netmeta object or a data frame with columns: treatment, TE, lower, upper")
  }
  
  # Validate summary measure
  if (is.null(sm)) {
    stop("sm must be specified when x is a data frame")
  }
  
  sm <- match.arg(sm, c("OR", "RR", "MD", "SMD", "RD"))
  
  # Create the plot
  .plot_forest_netmeta(
    df = df,
    sm = sm,
    title = title,
    point_color = point_color,
    ci_color = ci_color,
    point_size = point_size,
    ci_lwd = ci_lwd,
    digits = digits
  )
}


#' Extract Forest Plot Data from netmeta Object
#'
#' @description
#' Extracts treatment effects, confidence intervals, and treatment names
#' from a netmeta object for forest plot creation.
#'
#' @param x A netmeta object
#' @param model Character; "random" or "common" effects model (default: "random")
#' @param ref Character; reference treatment. If NULL, uses netmeta's reference group
#'
#' @return A tibble with columns: treatment, TE, lower, upper
#' @export
#'
#' @examples
#' \dontrun{
#' library(netmeta)
#' nma <- netmeta(TE, seTE, treat1, treat2, studlab, data = smokcessation)
#' extract_forest_data_netmeta(nma)
#' extract_forest_data_netmeta(nma, model = "common", ref = "A")
#' }
extract_forest_data_netmeta <- function(x, model = "random", ref = NULL) {
  
  # Validate input
  if (!inherits(x, "netmeta")) {
    stop("x must be a netmeta object from netmeta package")
  }
  
  # Validate model
  model <- match.arg(model, c("random", "common"))
  
  # Extract matrices
  TE_mat <- x[[paste0("TE.", model)]]
  lower_mat <- x[[paste0("lower.", model)]]
  upper_mat <- x[[paste0("upper.", model)]]
  
  # Determine reference treatment
  if (is.null(ref)) {
    ref <- x$reference.group
    if (is.null(ref)) {
      ref <- rownames(TE_mat)[1]
      message("Using '", ref, "' as reference treatment")
    }
  }
  
  # Validate reference treatment
  if (!ref %in% rownames(TE_mat)) {
    stop("Reference treatment '", ref, "' not found in network")
  }
  
  # Filter out reference treatment
  keep <- rownames(TE_mat) != ref
  
  # Build tidy data frame
  df <- tibble::tibble(
    treatment = rownames(TE_mat)[keep],
    TE = TE_mat[keep, ref],
    lower = lower_mat[keep, ref],
    upper = upper_mat[keep, ref],
    reference = ref,
    model = model
  )
  
  return(df)
}


#' Internal: Create forest plot from prepared data
#' @keywords internal
#' Internal: Create forest plot from prepared data
#' @keywords internal
.plot_forest_netmeta <- function(
    df,
    sm,
    title,
    point_color,
    ci_color,
    point_size,
    ci_lwd,
    digits
) {
  
  # For ratio measures, netmeta stores log-transformed values
  # We need to exponentiate them for plotting
  if (sm %in% c("OR", "RR")) {
    df <- df |>
      dplyr::mutate(
        TE = exp(TE),
        lower = exp(lower),
        upper = exp(upper)
      )
  }
  
  # Prepare data for plotting
  df <- df |>
    dplyr::mutate(
      # Format CI labels
      label = sprintf(
        paste0("%.", digits, "f [%.", digits, "f; %.", digits, "f]"),
        TE, lower, upper
      ),
      # Reverse factor levels for top-to-bottom plotting
      treatment = factor(treatment, levels = rev(treatment))
    )
  
  # Determine reference line position
  ref_line <- if (sm %in% c("OR", "RR")) 1 else 0
  
  # X-axis configuration
  if (sm %in% c("OR", "RR")) {
    # Log scale for ratio measures (now exponentiated)
    scale_x <- ggplot2::scale_x_log10(
      breaks = c(0.25, 0.5, 1, 2, 4),
      labels = scales::number_format(accuracy = 0.01)
    )
    
    # Position for text annotation (multiplicative)
    x_text <- max(df$upper, na.rm = TRUE) * 1.4
    x_min <- min(df$lower, na.rm = TRUE) * 0.8
    
  } else {
    # Linear scale for difference measures
    scale_x <- ggplot2::scale_x_continuous()
    
    # Position for text annotation (additive)
    x_range <- diff(range(c(df$lower, df$upper), na.rm = TRUE))
    x_text <- max(df$upper, na.rm = TRUE) + 0.25 * x_range
    x_min <- min(df$lower, na.rm = TRUE) - 0.05 * x_range
  }
  
  # Create plot
  p <- ggplot2::ggplot(df, ggplot2::aes(y = treatment)) +
    # Confidence intervals
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = lower, xmax = upper),
      width = 0.15,
      linewidth = ci_lwd,
      color = ci_color
    ) +
    # Point estimates
    ggplot2::geom_point(
      ggplot2::aes(x = TE),
      size = point_size,
      color = point_color
    ) +
    # Reference line (null effect)
    ggplot2::geom_vline(
      xintercept = ref_line,
      linetype = "dashed",
      color = "grey40",
      linewidth = 0.8
    ) +
    # CI text labels
    ggplot2::geom_text(
      ggplot2::aes(x = x_text, label = label),
      hjust = 0,
      size = 3.5
    ) +
    # Apply x-scale
    scale_x +
    # Labels
    ggplot2::labs(
      title = title,
      x = sm,
      y = NULL
    ) +
    # Extend plot area for text
    ggplot2::coord_cartesian(
      clip = "off",
      xlim = c(x_min, x_text * 1.2)
    ) +
    # Theme
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(hjust = 0),
      plot.margin = ggplot2::margin(5.5, 60, 5.5, 5.5),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    ) +
    # Column header
    ggplot2::annotate(
      "text",
      x = x_text,
      y = length(levels(df$treatment)) + 0.7,
      label = paste0(sm, " [95% CI]"),
      fontface = "bold",
      hjust = 0,
      size = 3.5
    )
  
  return(p)
}



#' Check if netmeta object uses log scale
#' @keywords internal
.needs_log_scale <- function(nma) {
  if (!inherits(nma, "netmeta")) return(FALSE)
  nma$sm %in% c("OR", "RR")
}
