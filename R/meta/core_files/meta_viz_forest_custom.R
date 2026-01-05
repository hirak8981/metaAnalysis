library(ggplot2)
library(metafor)
library(dplyr)
library(rlang)

#' Create Custom Forest Plot for Meta-Analysis
#'
#' @description
#' Creates a customizable forest plot from an rma (random/mixed effects meta-analysis)
#' object with support for subgroups, heterogeneity statistics, and custom styling.
#' Handles both simple meta-analyses and meta-analyses with categorical moderators.
#'
#' @param x An rma object from metafor::rma() (required)
#' @param study_labels Study labels - can be bare column name (with data=) or character vector
#' @param data Optional data frame for tidy evaluation of study_labels
#' @param confidence_level Numeric; confidence level for intervals (default: 0.95)
#'
#' @section Study Aesthetics:
#' @param study_col Color for study points (default: "black")
#' @param study_shape Shape for study points (default: 15 - filled square)
#' @param study_size Size for study points (default: 3)
#' @param ci_col Color for study confidence intervals (default: "black")
#' @param ci_linewidth Line width for study CIs (default: 0.5)
#'
#' @section Summary Aesthetics:
#' @param summary_col Color for summary diamond/point (default: "red")
#' @param summary_shape Shape for summary point (default: 18 - filled diamond)
#' @param summary_size Size for summary point (default: 5)
#' @param summary_ci_col Color for summary CI (default: "red")
#' @param summary_ci_linewidth Line width for summary CI (default: 0.8)
#'
#' @section Subgroup Options:
#' @param show_overall_summary Logical; show overall summary when subgroups exist (default: TRUE)
#' @param subgroup_labels Character vector; custom labels for subgroups
#'
#' @section Labels and Text:
#' @param xlab Character; x-axis label (default: "Effect Size")
#' @param text_size Numeric; base text size (default: 3)
#'
#' @section Display Options:
#' @param annotate_CI Logical; show CI values as text (default: FALSE)
#' @param show_weights Logical; show study weights as text (default: FALSE)
#' @param x_trans_function Function; transform x-axis (e.g., exp for OR) (default: NULL = auto-detect)
#' @param x_breaks Numeric vector; custom x-axis breaks (default: NULL = automatic)
#'
#' @section Heterogeneity Options:
#' @param show_heterogeneity Logical; display heterogeneity statistics (default: TRUE)
#' @param het_position Character; "caption" or "subtitle" for heterogeneity display (default: "caption")
#' @param het_digits Numeric; decimal places for heterogeneity statistics (default: 2)
#'
#' @section Table Headers:
#' @param ci_header Character; header for CI column (default: auto-generated)
#' @param weight_header Character; header for weight column (default: "Weight")
#'
#' @section Theme:
#' @param theme_fun ggplot2 theme function (default: theme_bw())
#'
#' @return A ggplot2 object
#'
#' @details
#' The function automatically detects:
#' \itemize{
#'   \item Effect measure type (OR, RR, MD, SMD, etc.) for appropriate scaling
#'   \item Subgroups from categorical moderators in the rma model
#'   \item Appropriate null value (0 for differences, 1 for ratios)
#' }
#'
#' For models with categorical moderators, subgroup summaries are displayed
#' with an optional overall summary.
#'
#' @export
#'
#' @examples
#' library(metafor)
#' 
#' # Simple forest plot
#' data(dat.bcg)
#' res <- rma(yi, vi, data = dat.bcg)
#' custom_forest_plot(res)
#' 
#' # With custom labels
#' custom_forest_plot(res, study_labels = author, data = dat.bcg)
#' 
#' # With annotations
#' custom_forest_plot(res, annotate_CI = TRUE, show_weights = TRUE)
#' 
#' # With subgroups
#' res_mod <- rma(yi, vi, mods = ~ alloc, data = dat.bcg)
#' custom_forest_plot(res_mod)

custom_forest_plot <- function(
    x,                         # rma object (required)
    study_labels = NULL,       # Can be bare column name or vector
    data = NULL,               # Optional: data frame for tidy eval
    confidence_level = 0.95,
    
    # Study aesthetics
    study_col = "black",
    study_shape = 15,
    study_size = 3,
    ci_col = "black",
    ci_linewidth = 0.5,
    
    # Summary aesthetics  
    summary_col = "red",
    summary_shape = 18,
    summary_size = 5,
    summary_ci_col = "red",
    summary_ci_linewidth = 0.8,
    
    # Subgroup options
    show_overall_summary = TRUE,
    subgroup_labels = NULL,
    
    # Labels and text
    xlab = "Effect Size",
    text_size = 3,
    
    # Options
    annotate_CI = FALSE,
    show_weights = FALSE,
    x_trans_function = NULL,
    x_breaks = NULL,
    
    # NEW: Heterogeneity display options
    show_heterogeneity = TRUE,
    het_position = "caption",  # "caption" or "subtitle"
    het_digits = 2,
    
    # Table headers
    ci_header = NULL,
    weight_header = "Weight",
    
    # Theme
    theme_fun = theme_bw()
) {
  
  # Check input
  if (missing(x) || !("rma" %in% class(x))) {
    stop("x must be an rma object from metafor::rma()")
  }
  
  # ---- Display scale (measure-aware) ----
  disp <- get_display_scale(x, x_trans_function)
  if (is.null(x_trans_function)) {
    x_trans_function <- disp$trans
  }
  
  if (missing(xlab) || xlab == "Effect Size") {
    xlab <- disp$xlab
  }
  
  ci_label <- disp$ci_label
  null_value <- disp$null
  
  # Capture expression for tidy evaluation
  study_labels_expr <- enquo(study_labels)
  
  # Extract data from rma object
  es <- as.numeric(x$yi)
  se <- as.numeric(sqrt(x$vi))
  n <- length(es)
  method <- x$method
  
  # Extract study labels using tidy eval
  if (!quo_is_null(study_labels_expr)) {
    if (!is.null(data)) {
      # Try to evaluate as bare column name from data
      study_labels_vec <- tryCatch(
        pull(data, !!study_labels_expr),
        error = function(e) {
          # If fails, try as quoted expression
          tryCatch(
            eval_tidy(study_labels_expr, data = data),
            error = function(e2) {
              # Fall back to direct evaluation
              eval_tidy(study_labels_expr)
            }
          )
        }
      )
    } else {
      # No data provided, evaluate directly
      study_labels_vec <- eval_tidy(study_labels_expr)
    }
    
    # Check if it's already a vector (backward compatibility)
    if (is.character(study_labels_vec) || is.numeric(study_labels_vec)) {
      study_labels_vec <- study_labels_vec
    } else {
      study_labels_vec <- NULL
    }
  } else {
    study_labels_vec <- NULL
  }
  
  # Fall back to x$slab or defaults
  if (is.null(study_labels_vec)) {
    if (!is.null(x$slab)) {
      study_labels_vec <- x$slab
    } else {
      study_labels_vec <- paste("Study", 1:n)
    }
  } else if (length(study_labels_vec) != n) {
    warning("Length of study_labels does not match number of studies; using defaults.")
    if (!is.null(x$slab)) {
      study_labels_vec <- x$slab
    } else {
      study_labels_vec <- paste("Study", 1:n)
    }
  }
  
  # Extract subgroups from rma model (if moderators exist)
  group_vec <- NULL
  if (ncol(x$X) > 1) {
    if (!all(x$X == 1 | x$X == 0) || any(apply(as.matrix(x$X), 1, sum) > 1)) {
      warning("Cannot handle metafor output with continuous or multiple categorical moderators.")
    } else {
      # Handle models with intercept (mods = ~ group) or without (mods = ~ group - 1)
      if (colnames(x$X)[1] == "intrcpt") {
        # Model with intercept: mods = ~ group
        no.levels <- ncol(x$X) - 1
        group_matrix <- as.matrix(x$X[, -1])
        group_vec <- factor(apply(group_matrix * rep(1:no.levels, each = n), 1, sum))
        levels(group_vec) <- colnames(x$X)[-1]
      } else {
        # Model without intercept: mods = ~ group - 1
        group_matrix <- as.matrix(x$X)
        group_vec <- factor(apply(group_matrix * rep(1:ncol(group_matrix), each = n), 1, sum))
        levels(group_vec) <- colnames(x$X)
      }
    }
  }
  
  # Auto-generate CI header
  if (is.null(ci_header)) {
    ci_header <- paste0(
      ci_label,
      " [",
      round(confidence_level * 100),
      "% CI]"
    )
  }
  
  z_val <- qnorm(1 - (1 - confidence_level) / 2)
  
  # === NO SUBGROUPS ===
  if (is.null(group_vec)) {
    res_overall <- rma(yi = es, sei = se, method = method)
    weights <- weights(res_overall)
    
    summary_es <- as.numeric(res_overall$b)
    summary_se <- as.numeric(res_overall$se)
    
    study_lower <- es - z_val * se
    study_upper <- es + z_val * se
    summary_lower <- summary_es - z_val * summary_se
    summary_upper <- summary_es + z_val * summary_se
    
    plotdata <- data.frame(
      label = c(study_labels_vec, "Summary"),
      es = c(es, summary_es),
      lower = c(study_lower, summary_lower),
      upper = c(study_upper, summary_upper),
      se = c(se, summary_se),
      weight = c(weights, 100),
      type = factor(c(rep("Study", n), "Summary"), levels = c("Study", "Summary")),
      group_label = "",
      y = (n + 1):1
    )
    
  } else {
    # === WITH SUBGROUPS ===
    group_vec <- droplevels(group_vec)
    k <- length(levels(group_vec))
    
    if (is.null(subgroup_labels)) {
      subgroup_labels <- paste("Subgroup:", levels(group_vec))
    } else if (length(subgroup_labels) != k) {
      warning("Length of subgroup_labels does not match number of subgroups; using defaults.")
      subgroup_labels <- paste("Subgroup:", levels(group_vec))
    }
    
    current_y <- n + k * 2 + ifelse(show_overall_summary, 3, 0)
    plotdata_list <- list()
    
    for (i in 1:k) {
      grp <- levels(group_vec)[i]
      idx <- which(group_vec == grp)
      n_grp <- length(idx)
      
      es_grp <- es[idx]
      se_grp <- se[idx]
      labels_grp <- study_labels_vec[idx]
      
      res_grp <- rma(yi = es_grp, sei = se_grp, method = method)
      weights_grp <- weights(res_grp)
      
      summary_es_grp <- as.numeric(res_grp$b)
      summary_se_grp <- as.numeric(res_grp$se)
      
      study_lower_grp <- es_grp - z_val * se_grp
      study_upper_grp <- es_grp + z_val * se_grp
      summary_lower_grp <- summary_es_grp - z_val * summary_se_grp
      summary_upper_grp <- summary_es_grp + z_val * summary_se_grp
      
      y_studies <- seq(current_y, current_y - n_grp + 1, by = -1)
      y_summary <- current_y - n_grp - 1
      
      grp_data <- data.frame(
        label = c(labels_grp, subgroup_labels[i]),
        es = c(es_grp, summary_es_grp),
        lower = c(study_lower_grp, summary_lower_grp),
        upper = c(study_upper_grp, summary_upper_grp),
        se = c(se_grp, summary_se_grp),
        weight = c(weights_grp, NA),
        type = factor(c(rep("Study", n_grp), "Summary"), levels = c("Study", "Summary")),
        group_label = grp,
        y = c(y_studies, y_summary)
      )
      
      plotdata_list[[i]] <- grp_data
      current_y <- y_summary - 2
    }
    
    plotdata <- bind_rows(plotdata_list)
    
    if (show_overall_summary) {
      res_overall <- rma(yi = es, sei = se, method = method)
      summary_es_overall <- as.numeric(res_overall$b)
      summary_se_overall <- as.numeric(res_overall$se)
      summary_lower_overall <- summary_es_overall - z_val * summary_se_overall
      summary_upper_overall <- summary_es_overall + z_val * summary_se_overall
      
      overall_data <- data.frame(
        label = "Overall Summary",
        es = summary_es_overall,
        lower = summary_lower_overall,
        upper = summary_upper_overall,
        se = summary_se_overall,
        weight = NA,
        type = factor("Summary", levels = c("Study", "Summary")),
        group_label = "",
        y = 1
      )
      
      plotdata <- bind_rows(plotdata, overall_data)
    }
  }
  
  # === PLOTTING ===
  p <- ggplot(plotdata, aes(x = es, y = y))
  
  p <- p + geom_errorbar(
    data = plotdata[plotdata$type == "Study", ],
    aes(xmin = lower, xmax = upper),
    width = 0.2,
    color = ci_col,
    linewidth = ci_linewidth,
    orientation = "y"
  )
  
  p <- p + geom_point(
    data = plotdata[plotdata$type == "Study", ],
    color = study_col,
    shape = study_shape,
    size = study_size
  )
  
  p <- p + geom_errorbar(
    data = plotdata[plotdata$type == "Summary", ],
    aes(xmin = lower, xmax = upper),
    width = 0.3,
    color = summary_ci_col,
    linewidth = summary_ci_linewidth,
    orientation = "y"
  )
  
  p <- p + geom_point(
    data = plotdata[plotdata$type == "Summary", ],
    color = summary_col,
    shape = summary_shape,
    size = summary_size
  )
  
  p <- p + geom_vline(
    xintercept = null_value,
    linetype = "dashed",
    color = "gray50"
  )
  
  p <- p + scale_y_continuous(
    breaks = plotdata$y,
    labels = plotdata$label,
    expand = expansion(add = 0.5)
  )
  
  if (!is.null(x_trans_function)) {
    if (is.null(x_breaks)) {
      p <- p + scale_x_continuous(
        name = xlab,
        labels = function(x) round(x_trans_function(x), 2)
      )
    } else {
      p <- p + scale_x_continuous(
        name = xlab,
        breaks = x_breaks,
        labels = function(x) round(x_trans_function(x), 2)
      )
    }
  } else {
    if (!is.null(x_breaks)) {
      p <- p + scale_x_continuous(name = xlab, breaks = x_breaks)
    } else {
      p <- p + scale_x_continuous(name = xlab)
    }
  }
  
  p <- p + theme_fun +
    theme(
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 10),
      plot.subtitle = element_text(size = 10),
      text = element_text(size = 1/0.352777778 * text_size),
      axis.text.x = element_text(size = 10),   # x-axis tick labels
      axis.text.y = element_text(size = 10)    # y-axis tick labels
    )
  
  if (annotate_CI) {
    ci_text <- plotdata %>%
      mutate(
        es_text = if (!is.null(x_trans_function)) {
          sprintf("%.2f [%.2f, %.2f]", 
                  x_trans_function(es), 
                  x_trans_function(lower), 
                  x_trans_function(upper))
        } else {
          sprintf("%.2f [%.2f, %.2f]", es, lower, upper)
        }
      )
  }
  
  if (show_weights) {
    weight_text <- plotdata %>%
      mutate(
        weight_text = ifelse(is.na(weight), "", sprintf("%.1f%%", weight))
      )
  }
  
  x_range <- range(c(plotdata$lower, plotdata$upper))
  x_expand <- diff(x_range) * 0.1
  
  header_y <- max(plotdata$y) + 1
  hline_y <- max(plotdata$y) + 0.5
  
  if (show_weights || annotate_CI) {
    p <- p + geom_hline(yintercept = hline_y, 
                        color = "black", linewidth = 0.5)
  }
  
  if (show_weights) {
    p <- p + annotate("text", 
                      x = min(plotdata$lower) - x_expand * 1.5,
                      y = header_y,
                      label = weight_header,
                      hjust = 0.5,
                      size = text_size,
                      fontface = "bold")
    
    p <- p + geom_text(
      data = weight_text,
      aes(x = min(plotdata$lower) - x_expand * 1.5, y = y, label = weight_text),
      hjust = 0.5,
      size = text_size,
      fontface = ifelse(weight_text$type == "Summary", "bold", "plain")
    )
  }
  
  if (annotate_CI) {
    p <- p + annotate("text", 
                      x = max(plotdata$upper) + x_expand * 1.5,
                      y = header_y,
                      label = ci_header,
                      hjust = 0,
                      size = text_size,
                      fontface = "bold")
    
    p <- p + geom_text(
      data = ci_text,
      aes(x = max(plotdata$upper) + x_expand * 1.5, y = y, label = es_text),
      hjust = 0,
      size = text_size,
      fontface = ifelse(ci_text$type == "Summary", "bold", "plain")
    )
  }
  
  y_expand_top <- if (annotate_CI || show_weights) 1.5 else 1
  
  xlim_left <- if (show_weights) {
    min(plotdata$lower) - x_expand * 2
  } else {
    min(plotdata$lower) - x_expand * 0.5
  }
  
  xlim_right <- if (annotate_CI) {
    max(plotdata$upper) + x_expand * 4
  } else {
    max(plotdata$upper) + x_expand * 0.5
  }
  
  p <- p + coord_cartesian(
    xlim = c(xlim_left, xlim_right),
    ylim = c(min(plotdata$y) - 0.5, max(plotdata$y) + y_expand_top),
    clip = "off"
  )
  
  # === ADD HETEROGENEITY INFORMATION ===
  if (show_heterogeneity && !is.null(x$tau2) && !is.null(x$I2)) {
    
    # Extract heterogeneity statistics
    I2 <- round(x$I2, het_digits)
    tau2 <- round(x$tau2, het_digits + 2)  # More precision for tau²
    QE <- round(x$QE, het_digits)
    QEp <- x$QEp
    
    # Format p-value
    p_text <- if (QEp < 0.001) {
      "< 0.001"
    } else {
      sprintf("%.3f", QEp)
    }
    
    # Create heterogeneity text
    if (is.null(group_vec) || ncol(x$X) == 1) {
      # No moderators - simple heterogeneity
      het_text <- bquote(
        "Heterogeneity: " * tau^2 ~ "=" ~ .(tau2) * ", " * 
          I^2 ~ "=" ~ .(I2) * "%, " * 
          Q[.(x$k.f - 1)] ~ "=" ~ .(QE) * ", " * 
          italic(p) ~ "=" ~ .(p_text)
      )
    } else {
      # With moderators - add QM test
      QM <- round(x$QM, het_digits)
      QMp <- x$QMp
      
      qm_p_text <- if (QMp < 0.001) {
        "< 0.001"
      } else {
        sprintf("%.3f", QMp)
      }
      
      het_text <- bquote(
        "Heterogeneity: " * tau^2 ~ "=" ~ .(tau2) * ", " * 
          I^2 ~ "=" ~ .(I2) * "% | " *
          "Test for moderators: " * Q[M] ~ "=" ~ .(QM) * 
          " (df = " * .(x$QMdf) * "), " * 
          italic(p) ~ "=" ~ .(qm_p_text)
      )
    }
    
    # Add heterogeneity based on position
    if (het_position == "caption") {
      p <- p + labs(caption = het_text)
    } else if (het_position == "subtitle") {
      p <- p + labs(subtitle = het_text)
    }
  }
  
  return(p)
}


#' Forest Plot Wrapper with Simplified Interface
#'
#' @description
#' Convenient wrapper around custom_forest_plot() with simplified aesthetic
#' parameters and sensible defaults for quick plotting.
#'
#' @param ... Arguments passed to custom_forest_plot()
#' @param point_color Color for study points (default: "steelblue")
#' @param point_size Size for study points (default: 3)
#' @param point_shape Shape for study points (default: 15)
#' @param summary_color Color for summary estimates (default: "darkred")
#' @param summary_size Size for summary points (default: 5)
#' @param summary_shape Shape for summary points (default: 18)
#' @param ci_color Color for confidence intervals (default: "black")
#' @param ci_linewidth Line width for CIs (default: 0.5)
#' @param summary_ci_color Color for summary CI (default: NULL = matches summary_color)
#' @param summary_ci_linewidth Line width for summary CI (default: 0.8)
#' @param theme_fun ggplot2 theme (default: theme_minimal())
#' @param plot_title Plot title (default: "Forest Plot")
#' @param plot_subtitle Plot subtitle (default: NULL)
#' @param plot_caption Plot caption (default: NULL)
#' @param show_heterogeneity Show heterogeneity statistics (default: TRUE)
#' @param het_position Position for heterogeneity ("caption" or "subtitle")
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' library(metafor)
#' data(dat.bcg)
#' res <- rma(yi, vi, data = dat.bcg)
#' 
#' # Quick plot with defaults
#' forest_custom(res)
#' 
#' # Custom colors
#' forest_custom(res, point_color = "navy", summary_color = "firebrick")

forest_custom <- function(
    ...,
    point_color = "steelblue",
    point_size = 3,
    point_shape = 15,
    summary_color = "darkred",
    summary_size = 5,
    summary_shape = 18,
    ci_color = "black",
    ci_linewidth = 0.5,
    summary_ci_color = NULL,
    summary_ci_linewidth = 0.8,
    theme_fun = ggplot2::theme_minimal(),
    plot_title = "Forest Plot",
    plot_subtitle = NULL,
    plot_caption = NULL,
    show_heterogeneity = TRUE,
    het_position = "caption"
) {
  
  # Set summary CI color to match summary if not specified
  if (is.null(summary_ci_color)) {
    summary_ci_color <- summary_color
  }
  
  # Call the main forest plot function with aesthetic parameters
  p <- custom_forest_plot(
    ...,
    study_col = point_color,
    study_size = point_size,
    study_shape = point_shape,
    summary_col = summary_color,
    summary_size = summary_size,
    summary_shape = summary_shape,
    ci_col = ci_color,
    ci_linewidth = ci_linewidth,
    summary_ci_col = summary_ci_color,
    summary_ci_linewidth = summary_ci_linewidth,
    theme_fun = theme_fun,
    show_heterogeneity = show_heterogeneity,
    het_position = het_position
  )
  
  # Add title (if heterogeneity not in subtitle position)
  if (!is.null(plot_title)) {
    p <- p + ggtitle(plot_title)
  }
  
  # Add subtitle (if heterogeneity not using subtitle position)
  if (!is.null(plot_subtitle) && het_position != "subtitle") {
    p <- p + labs(subtitle = plot_subtitle)
  }
  
  # Add caption (if heterogeneity not using caption position)
  if (!is.null(plot_caption) && het_position != "caption") {
    p <- p + labs(caption = plot_caption)
  }
  
  return(p)
}
