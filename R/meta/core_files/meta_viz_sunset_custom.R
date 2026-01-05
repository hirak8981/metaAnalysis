library(ggplot2)
library(metafor)
library(rlang)

#' Create Custom Sunset Plot for Meta-Analysis Power Analysis
#'
#' @description
#' Creates a "sunset plot" (enhanced funnel plot with power contours) for visualizing
#' statistical power, publication bias, and the relationship between effect sizes,
#' standard errors, and statistical significance. The plot displays power contours
#' as colored regions and calculates power-related statistics.
#'
#' @param x Either an rma object, matrix/data.frame with effect sizes and SEs, or NULL (if using data=)
#' @param data Optional data frame for tidy evaluation (used with es= and se=)
#' @param es Effect size column - bare column name or vector (required when data is provided)
#' @param se Standard error column - bare column name or vector (required when data is provided)
#' @param y_axis Character; y-axis type - "se" (standard error) or "precision" (1/SE) (default: "se")
#' @param true_effect Numeric; assumed true effect size for power calculations (default: NULL = summary effect)
#' @param method Character; meta-analysis method for pooling (default: "FE")
#' @param sig_level Numeric; significance level for hypothesis tests (default: 0.05)
#'
#' @section Power Options:
#' @param power_stats Logical; display power statistics in caption (default: TRUE)
#' @param power_contours Character; "discrete" (10% bands) or "continuous" (gradient) (default: "discrete")
#' @param power_col Color palette - RColorBrewer palette name or color vector (default: "RdYlGn")
#'
#' @section Contour Options:
#' @param contours Logical; show 95% confidence interval contours (default: FALSE)
#' @param sig_contours Logical; show significance contours (p < 0.05, p < 0.01) (default: TRUE)
#' @param contour_col Color for significance contours (default: "grey80")
#'
#' @section Aesthetics:
#' @param text_size Numeric; base text size (default: 3)
#' @param point_size Numeric; size of study points (default: 2)
#' @param xlab Character; x-axis label (default: "Effect")
#' @param ylab Character; y-axis label (default: NULL = auto-generated)
#'
#' @section Axis Control:
#' @param x_trans_function Function; transform x-axis (e.g., exp for OR) (default: NULL = auto-detect)
#' @param x_breaks Numeric vector; custom x-axis breaks (default: NULL = automatic)
#' @param y_breaks Numeric vector; custom y-axis breaks (default: NULL = automatic)
#' @param x_limit Numeric vector of length 2; x-axis limits (default: NULL = automatic)
#' @param y_limit Numeric vector of length 2; y-axis limits (default: NULL = automatic)
#'
#' @return A ggplot2 object
#'
#' @details
#' The sunset plot extends the funnel plot concept by overlaying power contours that show
#' the statistical power to detect the assumed true effect for studies of different
#' precision levels.
#'
#' **Power Statistics Calculated:**
#' \itemize{
#'   \item Median Power: Median statistical power across studies
#'   \item d_33%: Effect size detectable with 33% median power
#'   \item d_66%: Effect size detectable with 66% median power
#'   \item E: Expected number of significant results
#'   \item O: Observed number of significant results
#'   \item p_TES: Test of Excess Significance p-value
#'   \item R-Index: Replication index
#' }
#'
#' **Power Contours:**
#' \itemize{
#'   \item Discrete: Bands showing 0-10%, 10-20%, ..., 90-100% power
#'   \item Continuous: Smooth gradient from low to high power
#' }
#'
#' @export
#'
#' @examples
#' library(metafor)
#' 
#' # From rma object
#' data(dat.bcg)
#' res <- rma(yi, vi, data = dat.bcg)
#' viz_sunset_custom(res)
#' 
#' # With tidy evaluation
#' viz_sunset_custom(data = dat.bcg, es = yi, se = sqrt(vi))
#' 
#' # With custom true effect
#' viz_sunset_custom(res, true_effect = 0.5)
#' 
#' # With continuous power contours
#' viz_sunset_custom(res, power_contours = "continuous")
#' 
#' # Without power statistics
#' viz_sunset_custom(res, power_stats = FALSE)

viz_sunset_custom <- function(
    x = NULL,              # Can be rma object OR NULL if using data
    data = NULL,           # Optional: data frame for tidy eval
    es = NULL,             # Effect size column (bare name or vector)
    se = NULL,             # Standard error column (bare name or vector)
    y_axis = "se",
    true_effect = NULL,
    method = "FE",
    sig_level = 0.05,
    power_stats = TRUE,
    power_contours = "discrete",
    contours = FALSE,
    sig_contours = TRUE,
    text_size = 4,
    point_size = 4,
    xlab = "Effect",
    ylab = NULL,
    x_trans_function = NULL,
    x_breaks = NULL,
    y_breaks = NULL,
    x_limit = NULL,
    y_limit = NULL,
    contour_col = "grey80",
    power_col = "RdYlGn"
) {
  
  # Handle tidy evaluation for es and se
  if (!is.null(data)) {
    es_quo <- enquo(es)
    se_quo <- enquo(se)
    
    if (quo_is_null(es_quo) || quo_is_null(se_quo)) {
      stop("When data is provided, both es and se arguments must be specified.")
    }
    
    es_vals <- eval_tidy(es_quo, data)
    se_vals <- eval_tidy(se_quo, data)
    
    # Check for missing values
    if (sum(is.na(es_vals)) != 0 || sum(is.na(se_vals)) != 0) {
      warning("The effect sizes or standard errors contain missing values, only complete cases are used.")
      complete_cases <- complete.cases(es_vals, se_vals)
      es_vals <- es_vals[complete_cases]
      se_vals <- se_vals[complete_cases]
    }
    
    # Validate numeric
    if (!is.numeric(es_vals) || !is.numeric(se_vals)) {
      stop("Effect sizes and standard errors must be numeric.")
    }
    
    # Check positive SE
    if (!all(se_vals > 0)) {
      stop("Non-positive standard errors supplied")
    }
    
    es_final <- es_vals
    se_final <- se_vals
    method_final <- method
    
  } else if ("rma" %in% class(x)) {
    # Extract from rma object
    es_final <- as.numeric(x$yi)
    se_final <- as.numeric(sqrt(x$vi))
    method_final <- x$method
    
    # ---- Display scale (measure-aware) ----
    disp <- get_display_scale(x, x_trans_function)
    if (is.null(x_trans_function)) {
      x_trans_function <- disp$trans
    }
    
    if (missing(xlab) || xlab == "Effect") {
      xlab <- disp$xlab
    }
    
  } else if ((is.data.frame(x) || is.matrix(x)) && ncol(x) >= 2) {
    # Legacy support: x as matrix/data.frame
    if (sum(is.na(x[, 1])) != 0 || sum(is.na(x[, 2])) != 0) {
      warning("The effect sizes or standard errors contain missing values, only complete cases are used.")
      x <- x[stats::complete.cases(x), ]
    }
    
    if (!is.numeric(x[, 1]) || !is.numeric(x[, 2])) {
      stop("Input argument has to be numeric.")
    }
    
    if (!all(x[, 2] > 0)) {
      stop("Non-positive standard errors supplied")
    }
    
    es_final <- x[, 1]
    se_final <- x[, 2]
    method_final <- method
    
  } else {
    stop("Must provide either: (1) rma object as x, (2) data + es + se with tidy eval, or (3) x as matrix/data.frame")
  }
  
  k <- length(es_final)
  summary_es <- metafor::rma.uni(yi = es_final, sei = se_final, method = method_final)$b[[1]]
  summary_se <- sqrt(metafor::rma.uni(yi = es_final, sei = se_final, method = method_final)$vb[[1]])
  
  plotdata <- data.frame(es = es_final, se = se_final)
  
  min_x <- min(plotdata$es)
  max_x <- max(plotdata$es)
  
  if (!is.numeric(sig_level) || length(sig_level) != 1 ||
      sig_level <= 0 || sig_level >= 1)
    stop("sig_level must be a numeric value greater than 0 and smaller than 1")
  
  # Y-axis handling (se vs precision)
  if (y_axis == "se") {
    plotdata$y <- se_final
    
    if (is.null(y_limit)) {
      max_se <- max(se_final) + ifelse(diff(range(se_final)) != 0,
                                       diff(range(se_final)) * 0.1, max(se_final) * 0.1)
      y_limit <- c(0, max_se)
    } else {
      max_se <- max(y_limit)
    }
    
    if (is.null(ylab)) {
      ylab <- "Standard Error"
    }
    
    if (sig_contours == TRUE) {
      sig_funneldata <- data.frame(
        x = c(-stats::qnorm(0.975) * max_se, 0, stats::qnorm(0.975) * max_se,
              stats::qnorm(0.995) * max_se, 0, -stats::qnorm(0.995) * max_se),
        y = c(max_se, 0, max_se, max_se, 0, max_se)
      )
      min_x <- min(c(min_x, min(sig_funneldata$x)))
      max_x <- max(c(max_x, max(sig_funneldata$x)))
    }
    
    if (contours == TRUE) {
      funneldata <- data.frame(
        x = c(summary_es - stats::qnorm(0.975) * sqrt(max_se^2), summary_es,
              summary_es, summary_es + stats::qnorm(0.975) * sqrt(max_se^2)),
        y = c(max_se, 0, 0, max_se)
      )
      min_x <- min(c(min_x, min(funneldata$x)))
      max_x <- max(c(max_x, max(funneldata$x)))
    }
    
  } else if (y_axis == "precision") {
    plotdata$y <- 1/se_final
    
    if (is.null(y_limit)) {
      max_y <- max(1/se_final) + ifelse(diff(range(se_final)) != 0,
                                        diff(range(1/se_final)) * 0.05, 1/se_final * 0.05)
      min_y <- min(1/se_final) - ifelse(diff(range(se_final)) != 0,
                                        diff(range(1/se_final)) * 0.05, 1/se_final * 0.05)
    } else {
      max_y <- max(y_limit)
      min_y <- min(y_limit)
    }
    
    if (is.null(ylab)) {
      ylab <- "Precision (1/SE)"
    }
    
    if (sig_contours == TRUE) {
      n_support <- 200
      prec <- seq(from = min_y, to = max_y, length.out = n_support)
      x_prec_0.05 <- stats::qnorm(0.975) * (1/prec)
      x_prec_0.01 <- stats::qnorm(0.995) * (1/prec)
      sig_funneldata <- data.frame(
        x = c(-x_prec_0.01, rev(x_prec_0.01), x_prec_0.05, rev(-x_prec_0.05)),
        y = c(prec, rev(prec), prec, rev(prec))
      )
      
      if (is.null(x_limit)) {
        min_x <- min(c(min_x, min(sig_funneldata$x)))
        max_x <- max(c(max_x, max(sig_funneldata$x)))
      } else {
        min_x <- min(x_limit)
        max_x <- max(x_limit)
      }
    }
    
    if (contours == TRUE) {
      n_support <- 200
      prec <- seq(from = min_y, to = max_y, length.out = n_support)
      x_prec <- stats::qnorm(0.975) * sqrt((1/prec)^2)
      funneldata <- data.frame(
        x = rep(summary_es, times = n_support * 2) + c(-x_prec, rev(x_prec)),
        y = c(prec, rev(prec))
      )
      
      if (is.null(x_limit)) {
        min_x <- min(c(min_x, min(funneldata$x)))
        max_x <- max(c(max_x, max(funneldata$x)))
      } else {
        min_x <- min(x_limit)
        max_x <- max(x_limit)
      }
    }
    
    if (is.null(y_limit))
      y_limit <- c(min_y, max_y)
    
  } else {
    stop("y_axis argument must be either se or precision")
  }
  
  if (is.null(x_limit)) {
    x_limit <- c(min_x - diff(c(min_x, max_x)) * 0.05, max_x +
                   diff(c(min_x, max_x)) * 0.05)
  }
  
  if (is.null(true_effect)) {
    true_effect <- summary_es
  }
  
  # Power calculations
  yseq <- seq(from = y_limit[1], to = y_limit[2], length.out = 1000)
  
  if (y_axis == "se") {
    power <- (1 - stats::pnorm(stats::qnorm(1 - sig_level/2) *
                                 yseq, abs(true_effect), yseq)) +
      stats::pnorm(stats::qnorm(sig_level/2) * yseq, abs(true_effect), yseq)
  } else {
    power <- 1 - stats::pnorm(stats::qnorm(1 - sig_level/2) *
                                1/yseq, abs(true_effect), 1/yseq) +
      stats::pnorm(stats::qnorm(sig_level/2) * 1/yseq, abs(true_effect), 1/yseq)
  }
  
  # Power contours
  if (power_contours == "discrete") {
    power_y <- numeric(10)
    steps <- c(1:4, 6:10)
    
    for (i in 1:9) {
      power_y[i + 1] <- yseq[which.min(abs(power - steps[i]/10))]
    }
    
    if (y_axis == "se") {
      power_y[1] <- max(y_limit)
    } else {
      power_y[10] <- max(y_limit)
    }
    
    if (true_effect == 0) {
      power_y[1] <- y_limit[1]
      power_y[2:10] <- y_limit[2]
    }
    
    power_recs <- data.frame(
      xstart = x_limit[1], xend = x_limit[2],
      ystart = power_y[1:9], yend = power_y[2:10],
      fill = factor(paste("Power", c(0, steps[-length(steps)]) * 10, "-", steps * 10))
    )
    
  } else if (power_contours == "continuous") {
    power_grid <- data.frame(
      x = rep(x_limit, each = 1000),
      y = rep(yseq, times = 2),
      fill = rep(power, times = 2)
    )
    
  } else {
    stop("Argument for power_contours must be either \"discrete\" or \"continuous\".")
  }
  
  # Handle power_col color flexibility
  if (length(power_col) == 1 && power_col %in% rownames(RColorBrewer::brewer.pal.info)) {
    power_col <- RColorBrewer::brewer.pal(n = 9, name = power_col)
  }
  
  # Power statistics helper function
  dpower <- function(se, sig_level = 0.05, target_power = 0.33) {
    if (sig_level >= target_power) {
      "n.a."
    } else {
      d <- stats::uniroot(
        function(x) stats::median((1 - stats::pnorm(stats::qnorm(1 - sig_level/2) *
                                                      se, abs(x), se)) +
                                    stats::pnorm(stats::qnorm(sig_level/2) * se, abs(x), se)) - target_power,
        lower = 0, upper = 10, extendInt = "upX", tol = 0.001, maxiter = 2000
      )$root
      d <- round(d, 2)
      ifelse(d >= 10, "> 10", ifelse(d <= -10, "< -10", d))
    }
  }
  
  # Calculate power statistics
  if (power_stats == TRUE) {
    study_power <- (1 - stats::pnorm(stats::qnorm(1 - sig_level/2) *
                                       se_final, abs(true_effect), se_final)) +
      stats::pnorm(stats::qnorm(sig_level/2) * se_final, abs(true_effect), se_final)
    
    med_power <- paste(round(stats::median(study_power) * 100, 1), "%", sep = "")
    expected <- sum(study_power)
    observed <- sum(2 * (1 - stats::pnorm(abs(es_final/se_final))) <= sig_level)
    c2 <- (observed - expected)^2/expected + (observed - expected)^2/(length(study_power) - expected)
    p_tes <- round(1 - stats::pchisq(c2, df = 1), 3)
    R <- 2 * stats::median(study_power) - observed/length(se_final)
    R <- ifelse(R < 0, 0, ifelse(R > 1, 1, R))
    R <- paste0(round(R * 100, 1), "%")
    d33 <- dpower(se_final, sig_level = sig_level, target_power = 0.33)
    d66 <- dpower(se_final, sig_level = sig_level, target_power = 0.66)
  }
  
  # Check x_trans_function
  if (!is.null(x_trans_function) && !is.function(x_trans_function)) {
    warning("Argument x_trans_function must be a function; input ignored.")
    x_trans_function <- NULL
  }
  
  # Avoid global variable warnings
  y <- NULL
  fill <- NULL
  xstart <- NULL
  xend <- NULL
  ystart <- NULL
  yend <- NULL
  
  # Build plot
  p <- ggplot(data = plotdata, aes(x = es, y = y))
  
  # Add power contours
  if (power_contours == "continuous") {
    p <- p + geom_raster(data = power_grid, aes(x = x, y = y, fill = fill), alpha = 1) +
      scale_fill_gradientn(
        name = "Power",
        colours = power_col,
        limits = c(0.0499, 1),
        breaks = c(0.05, 0.2, 0.4, 0.6, 0.8, 1),
        guide = guide_colorbar(draw.ulim = FALSE, draw.llim = FALSE, barwidth = 10)
      )
  } else {
    p <- p + geom_rect(
      inherit.aes = FALSE, data = power_recs,
      aes(xmin = xstart, xmax = xend, ymin = ystart, ymax = yend, fill = fill), alpha = 1
    ) +
      scale_fill_manual(name = "", values = power_col)
  }
  
  # Add significance contours
  if (sig_contours == TRUE) {
    p <- p + geom_polygon(data = sig_funneldata, aes(x = x, y = y),
                          fill = contour_col, alpha = 0.6) +
      geom_path(data = sig_funneldata, aes(x = x, y = y))
  }
  
  # Add 95% CI contours
  if (contours == TRUE) {
    p <- p + geom_path(data = funneldata, aes(x = x, y = y)) +
      geom_vline(xintercept = summary_es)
  }
  
  # Add true effect line
  if (true_effect != summary_es) {
    p <- p + geom_vline(xintercept = true_effect, linetype = "dashed")
  }
  
  # Y-axis formatting
  if (y_axis == "se") {
    if (is.null(y_breaks)) {
      p <- p + scale_y_reverse(
        name = ylab,
        sec.axis = dup_axis(~., name = "Power", labels = function(x) {
          paste(round((1 - stats::pnorm((stats::qnorm(1 - sig_level/2) * x - true_effect)/x) +
                         stats::pnorm((-stats::qnorm(1 - sig_level/2) * x - true_effect)/x)) * 100, 1),
                "%", sep = "")
        })
      )
    } else {
      p <- p + scale_y_reverse(
        name = ylab, breaks = y_breaks,
        sec.axis = dup_axis(~., name = "Power", labels = function(x) {
          paste(round((1 - stats::pnorm((stats::qnorm(1 - sig_level/2) * x - true_effect)/x) +
                         stats::pnorm((-stats::qnorm(1 - sig_level/2) * x - true_effect)/x)) * 100, 1),
                "%", sep = "")
        })
      )
    }
    y_limit <- rev(y_limit)
    
  } else {
    if (is.null(y_breaks)) {
      p <- p + scale_y_continuous(
        name = ylab,
        sec.axis = dup_axis(~., name = "Power", labels = function(x) {
          paste(round((1 - stats::pnorm((stats::qnorm(1 - sig_level/2) * 1/x - true_effect)/(1/x)) +
                         stats::pnorm((-stats::qnorm(1 - sig_level/2) * 1/x - true_effect)/(1/x))) * 100, 1),
                "%", sep = "")
        })
      )
    } else {
      p <- p + scale_y_continuous(
        name = ylab, breaks = y_breaks,
        sec.axis = dup_axis(~., name = "Power", labels = function(x) {
          paste(round((1 - stats::pnorm((stats::qnorm(1 - sig_level/2) * 1/x - true_effect)/(1/x)) +
                         stats::pnorm((-stats::qnorm(1 - sig_level/2) * 1/x - true_effect)/(1/x))) * 100, 1),
                "%", sep = "")
        })
      )
    }
  }
  
  # Add points
  p <- p + geom_point(size = point_size, fill = "white", shape = 21, col = "black", alpha = 1)
  
  # X-axis transformation
  if (!is.null(x_trans_function)) {
    if (is.null(x_breaks)) {
      p <- p + scale_x_continuous(name = xlab, labels = function(x) {
        round(x_trans_function(x), 3)
      })
    } else {
      p <- p + scale_x_continuous(name = xlab, labels = function(x) {
        round(x_trans_function(x), 3)
      }, breaks = x_breaks)
    }
  } else {
    if (is.null(x_breaks)) {
      p <- p + scale_x_continuous(name = xlab)
    } else {
      p <- p + scale_x_continuous(breaks = x_breaks, name = xlab)
    }
  }
  
  # Coordinate limits
  p <- p + coord_cartesian(xlim = x_limit, ylim = y_limit, expand = F)
  
  # Add power statistics caption
  if (power_stats == TRUE) {
    if (is.null(x_trans_function)) {
      p <- p + labs(caption = bquote(paste(
        alpha, " = ", .(sig_level), ", ", delta, " = ", .(round(true_effect, 2)),
        " | ", med[power], " = ", .(med_power), ", ", d[33 * "%"], " = ", .(d33),
        ", ", d[66 * "%"], " = ", .(d66), " | ", "E = ", .(round(expected, 2)),
        ", ", "O = ", .(observed), ", ", p[TES], .(ifelse(p_tes == 0, " < ", " = ")),
        .(ifelse(p_tes == 0, "0.001", p_tes)), ", ", "R-Index = ", .(R), sep = ""
      )))
    } else {
      p <- p + labs(caption = bquote(paste(
        alpha, " = ", .(sig_level), ", ", delta, " = ", .(round(x_trans_function(true_effect), 2)),
        " | ", med[power], " = ", .(med_power), ", ", d[33 * "%"], " = ",
        .(round(x_trans_function(d33), 2)), ", ", d[66 * "%"], " = ",
        .(round(x_trans_function(d66), 2)), " | ", "E = ", .(round(expected, 2)),
        ", ", "O = ", .(observed), ", ", p[TES], .(ifelse(p_tes == 0, " < ", " = ")),
        .(ifelse(p_tes == 0, "0.001", p_tes)), ", ", "R-Index = ", .(R), sep = ""
      )))
    }
  }
  
  # Theme
  p <- p + theme_bw() + theme(
    text = element_text(size = 3.4 * text_size),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 9),
    plot.subtitle = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
  
  p
}


#' Sunset Plot Wrapper with Simplified Interface
#'
#' @description
#' Convenient wrapper around viz_sunset_custom() with additional
#' customization options for points, theme, and title.
#'
#' @param ... Arguments passed to viz_sunset_custom()
#' @param point_color Color for study points (default: "#ffb5a7")
#' @param point_size Size for study points (default: 2)
#' @param point_shape Shape for study points (default: 19)
#' @param theme_fun ggplot2 theme (default: theme_minimal() with legend at bottom)
#' @param plot_title Plot title (default: "Sunset Plot")
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' library(metafor)
#' data(dat.bcg)
#' res <- rma(yi, vi, data = dat.bcg)
#' 
#' # Quick sunset plot
#' sunset_custom(res)
#' 
#' # With custom styling
#' sunset_custom(res, 
#'               point_color = "navy", 
#'               plot_title = "Power Analysis",
#'               power_contours = "continuous")

sunset_custom <- function(
    ...,
    point_color = "#ffb5a7",
    point_size = 2,
    point_shape = 19,
    theme_fun = ggplot2::theme_minimal() + theme(legend.position = "bottom"),
    plot_title = "Sunset Plot"
) {
  
  p <- viz_sunset_custom(...)
  
  # Add custom points on top
  p <- p + geom_point(color = point_color, size = point_size, shape = point_shape)
  
  # Add theme
  p <- p + theme_fun
  
  p <- p + theme(legend.position = "bottom",
                 plot.title = element_text(face = "bold", hjust = 0.5))
  
  # Add title
  p <- p + ggtitle(plot_title)
  
  return(p)
}
