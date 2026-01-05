library(ggplot2)
library(metafor)
library(rlang)
#' Create Custom Funnel Plot for Meta-Analysis
#'
#' @description
#' Creates a customizable funnel plot for detecting publication bias and small-study
#' effects in meta-analysis. Supports multiple input formats, significance contours,
#' trim-and-fill analysis, Egger's regression, and added-variable contours.
#'
#' @param x Either an rma object, matrix/data.frame with effect sizes and SEs, or NULL (if using data=)
#' @param data Optional data frame for tidy evaluation (used with es= and se=)
#' @param es Effect size column - bare column name or vector (required when data is provided)
#' @param se Standard error column - bare column name or vector (required when data is provided)
#' @param group Group/subgroup variable - bare column name or vector (optional)
#' @param y_axis Character; y-axis type - "se" (standard error) or "precision" (1/SE) (default: "se")
#' @param method Character; meta-analysis method for pooling (default: "FE")
#'
#' @section Contour Options:
#' @param contours Logical; show 95% confidence interval contours (default: TRUE)
#' @param sig_contours Logical; show significance contours (p < 0.05, p < 0.01) (default: TRUE)
#' @param addev_contours Logical; show added-variable contours (default: FALSE)
#' @param contours_col Color palette for contours - RColorBrewer palette name or color vector (default: "Blues")
#' @param contours_type Character; "FEM" (fixed) or "REM" (random) effects for contours (default: "FEM")
#' @param detail_level Numeric; detail level for contour calculations (0.1-10) (default: 1)
#'
#' @section Publication Bias Tests:
#' @param egger Logical; add Egger's regression line (only for y_axis = "se") (default: FALSE)
#' @param trim_and_fill Logical; perform trim-and-fill analysis (default: FALSE)
#' @param trim_and_fill_side Character; "left" or "right" for trim-and-fill (default: "left")
#'
#' @section Aesthetics:
#' @param text_size Numeric; base text size (default: 3)
#' @param point_size Numeric; size of study points (default: 2)
#' @param xlab Character; x-axis label (default: "Effect")
#' @param ylab Character; y-axis label (default: NULL = auto-generated)
#'
#' @section Group Options:
#' @param group_legend Logical; show legend for groups (default: FALSE)
#' @param group_legend_title Character; title for group legend (default: "")
#'
#' @section Transformation:
#' @param x_trans_function Function; transform x-axis (e.g., exp for OR) (default: NULL = auto-detect)
#' @param x_breaks Numeric vector; custom x-axis breaks (default: NULL = automatic)
#'
#' @return A ggplot2 object
#'
#' @details
#' The function supports three input methods:
#' \enumerate{
#'   \item rma object from metafor
#'   \item data frame with tidy evaluation (data=, es=, se=)
#'   \item Matrix/data frame with effect sizes in column 1, SEs in column 2
#' }
#'
#' **Contour Types:**
#' \itemize{
#'   \item Significance contours: Show regions of statistical significance
#'   \item 95% CI contours: Show expected funnel shape under fixed/random effects
#'   \item Added-variable contours: Show impact of hypothetical studies on significance
#' }
#'
#' **Publication Bias Methods:**
#' \itemize{
#'   \item Egger's regression: Tests for funnel plot asymmetry
#'   \item Trim-and-fill: Estimates and imputes missing studies
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
#' viz_funnel_custom(res)
#' 
#' # With tidy evaluation
#' viz_funnel_custom(data = dat.bcg, es = yi, se = sqrt(vi))
#' 
#' # With significance contours and Egger's test
#' viz_funnel_custom(res, sig_contours = TRUE, egger = TRUE)
#' 
#' # With trim-and-fill
#' viz_funnel_custom(res, trim_and_fill = TRUE)
#' 
#' # With groups
#' viz_funnel_custom(res, group = alloc, group_legend = TRUE)

viz_funnel_custom <- function(
    x = NULL,              # Can be rma object OR NULL if using data
    data = NULL,           # Optional: data frame for tidy eval
    es = NULL,             # Effect size column (bare name or vector)
    se = NULL,             # Standard error column (bare name or vector)
    group = NULL,          # Group variable (bare name or vector)
    y_axis = "se",
    method = "FE",
    contours = TRUE,
    sig_contours = TRUE,
    addev_contours = FALSE,
    contours_col = "Blues",
    contours_type = "FEM",
    detail_level = 1,
    egger = FALSE,
    trim_and_fill = FALSE,
    trim_and_fill_side = "left",
    text_size = 5,
    point_size = 2,
    xlab = "Effect",
    ylab = NULL,
    group_legend = FALSE,
    group_legend_title = "",
    x_trans_function = NULL,
    x_breaks = NULL,
    group_colors = NULL   # <--- NEW ARGUMENT
) {
  
  
  # Handle tidy evaluation for es, se, and group
  if (!is.null(data)) {
    es_quo <- enquo(es)
    se_quo <- enquo(se)
    group_quo <- enquo(group)
    
    if (quo_is_null(es_quo) || quo_is_null(se_quo)) {
      stop("When data is provided, both es and se arguments must be specified.")
    }
    
    es_vals <- eval_tidy(es_quo, data)
    se_vals <- eval_tidy(se_quo, data)
    
    # Handle group with tidy eval
    if (!quo_is_null(group_quo)) {
      group_vals <- eval_tidy(group_quo, data)
      if (!is.null(group_vals) && !is.factor(group_vals)) {
        group_vals <- as.factor(group_vals)
      }
    } else {
      group_vals <- NULL
    }
    
    # Check for missing values
    if (sum(is.na(es_vals)) != 0 || sum(is.na(se_vals)) != 0) {
      warning("The effect sizes or standard errors contain missing values, only complete cases are used.")
      complete_cases <- complete.cases(es_vals, se_vals)
      es_vals <- es_vals[complete_cases]
      se_vals <- se_vals[complete_cases]
      if (!is.null(group_vals)) {
        group_vals <- group_vals[complete_cases]
      }
    }
    
    # Validate numeric
    if (!is.numeric(es_vals) || !is.numeric(se_vals)) {
      stop("Effect sizes and standard errors must be numeric.")
    }
    
    # Check positive SE
    if (!all(se_vals > 0)) {
      stop("Non-positive standard errors supplied")
    }
    
    # Validate group length
    if (!is.null(group_vals) && length(group_vals) != length(es_vals)) {
      warning("length of supplied group vector does not correspond to the number of studies; group argument is ignored")
      group_vals <- NULL
    }
    
    es_final <- es_vals
    se_final <- se_vals
    group_final <- group_vals
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
    
    # Extract group from rma moderators if not provided
    group_quo <- enquo(group)
    if (quo_is_null(group_quo) && ncol(x$X) > 1) {
      if (!all(x$X == 1 | x$X == 0) || any(apply(as.matrix(x$X[, -1]), 1, sum) > 1)) {
        stop("Can not deal with metafor output object with continuous and/or more than one categorical moderator variable(s).")
      }
      no.levels <- ncol(x$X) - 1
      group_final <- factor(apply(as.matrix(x$X[, -1]) * rep(1:no.levels, each = length(es_final)), 1, sum))
    } else if (!quo_is_null(group_quo)) {
      group_final <- eval_tidy(group_quo)
      if (!is.null(group_final) && !is.factor(group_final)) {
        group_final <- as.factor(group_final)
      }
      if (!is.null(group_final) && length(group_final) != length(es_final)) {
        warning("length of supplied group vector does not correspond to the number of studies; group argument is ignored")
        group_final <- NULL
      }
    } else {
      group_final <- NULL
    }
    
  } else if ((is.data.frame(x) || is.matrix(x)) && ncol(x) >= 2) {
    # Legacy support: x as matrix/data.frame
    group_quo <- enquo(group)
    
    if (sum(is.na(x[, 1])) != 0 || sum(is.na(x[, 2])) != 0) {
      warning("The effect sizes or standard errors contain missing values, only complete cases are used.")
      if (!quo_is_null(group_quo)) {
        group_temp <- eval_tidy(group_quo)
        if (!is.null(group_temp)) {
          group_temp <- group_temp[stats::complete.cases(x)]
        }
      }
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
    
    # Handle group
    if (!quo_is_null(group_quo)) {
      group_final <- eval_tidy(group_quo)
      if (!is.null(group_final) && !is.factor(group_final)) {
        group_final <- as.factor(group_final)
      }
      if (!is.null(group_final) && length(group_final) != length(es_final)) {
        warning("length of supplied group vector does not correspond to the number of studies; group argument is ignored")
        group_final <- NULL
      }
    } else {
      group_final <- NULL
    }
    
  } else {
    stop("Must provide either: (1) rma object as x, (2) data + es + se with tidy eval, or (3) x as matrix/data.frame")
  }
  
  k <- length(es_final)
  summary_es <- metafor::rma.uni(yi = es_final, sei = se_final, method = method_final)$b[[1]]
  summary_se <- sqrt(metafor::rma.uni(yi = es_final, sei = se_final, method = method_final)$vb[[1]])
  
  if (contours_type == "FEM") {
    summary_tau2 <- 0
  } else if (contours_type == "REM") {
    summary_tau2 <- metafor::rma.uni(yi = es_final, sei = se_final, method = method_final)$tau2
  } else {
    warning("Supported arguments for contours_type are FEM or REM. FEM (the default) is used.")
    summary_tau2 <- 0
  }
  
  if (is.null(group_final)) {
    plotdata <- data.frame(es = es_final, se = se_final)
  } else {
    plotdata <- data.frame(es = es_final, se = se_final, group = group_final)
  }
  
  # Handle contours_col color flexibility
  if (length(contours_col) == 1 && contours_col %in% rownames(RColorBrewer::brewer.pal.info)) {
    col <- RColorBrewer::brewer.pal(n = 9, name = contours_col)
  } else if (length(contours_col) == 1 && grepl("^#|^[[:alpha:]]", contours_col)) {
    col <- rep(contours_col, 9)
  } else if (is.character(contours_col) && length(contours_col) > 1) {
    col <- rep(contours_col, length.out = 9)
  } else {
    warning("contours_col not recognized, fallback to Blues.")
    col <- RColorBrewer::brewer.pal(n = 9, name = "Blues")
  }
  
  if (detail_level < 0.1) {
    detail_level <- 0.1
    warning("Argument detail_level too low. Set to minimum value (0.1)")
  }
  
  if (detail_level > 10) {
    detail_level <- 10
    warning("Argument detail_level too high. Set to maximum value (10)")
  }
  
  min_x <- min(plotdata$es)
  max_x <- max(plotdata$es)
  
  # Trim and fill
  if (trim_and_fill == TRUE) {
    trimnfill <- function(es, se, group = NULL, side = "left") {
      if (side == "right") {
        es <- -es
      }
      if (side != "right" && side != "left") {
        stop("trim_and_fill_side argument must be either left or right")
      }
      
      mean_func <- function(es, se) {
        metafor::rma.uni(yi = es, sei = se, method = method_final)$b[1]
      }
      
      k0_func <- function(es, se, summary_es) {
        n <- length(es)
        Tn <- sum(rank(abs(es - summary_es))[sign(es - summary_es) > 0])
        round(max((4 * Tn - n * (n + 1))/(2 * n - 1), 0), 0)
      }
      
      summary_es_init <- mean_func(es, se)
      k0 <- k0_func(es = es, se = se, summary_es = summary_es_init)
      eps <- 1
      iter <- 0
      
      while (eps > 0.01 && iter < 20) {
        iter <- iter + 1
        es_ord <- es[order(es, decreasing = TRUE)]
        se_ord <- se[order(es, decreasing = TRUE)]
        
        if (k0 > 0) {
          es_ord <- es_ord[-(1:k0)]
          se_ord <- se_ord[-(1:k0)]
        }
        
        summary_es_new <- mean_func(es_ord, se_ord)
        k0 <- k0_func(es = es, se = se, summary_es = summary_es_new)
        eps <- abs(summary_es_init - summary_es_new)
        summary_es_init <- summary_es_new
      }
      
      if (iter == 19) {
        warning("Trim and fill algorithm did not converge after 20 iterations")
      }
      
      if (k0 > 0) {
        es_ord <- es[order(es, decreasing = TRUE)]
        se_ord <- se[order(es, decreasing = TRUE)]
        
        if (!is.null(group)) {
          group_ord <- group[order(es, decreasing = TRUE)]
          group_fill <- group_ord[1:k0]
        }
        
        if (side == "right") {
          es_fill <- -(summary_es_new + (summary_es_new - es_ord[1:k0]))
          summary_es_init <- -summary_es_init
        } else {
          es_fill <- summary_es_new + (summary_es_new - es_ord[1:k0])
        }
        
        se_fill <- se_ord[1:k0]
        
        if (is.null(group)) {
          data.frame(es_fill, se_fill, summary_es_init)
        } else {
          data.frame(es_fill, se_fill, group_fill, summary_es_init)
        }
      } else {
        if (is.null(group)) {
          data.frame(es_fill = NULL, se_fill = NULL, summary_es_init = NULL)
        } else {
          data.frame(es_fill = NULL, se_fill = NULL, group_fill = NULL, summary_es_init = NULL)
        }
      }
    }
    
    side <- trim_and_fill_side
    if (is.null(group_final)) {
      tnfdata <- trimnfill(es_final, se_final, side = side)
    } else {
      tnfdata <- trimnfill(es_final, se_final, group_final, side = side)
    }
    
    if (nrow(tnfdata) > 0) {
      if (is.null(group_final)) {
        names(tnfdata) <- c("es", "se", "tnf_summary")
      } else {
        names(tnfdata) <- c("es", "se", "group", "tnf_summary")
      }
      min_x <- min(c(min_x, min(tnfdata$es)))
      max_x <- max(c(max_x, max(tnfdata$es)))
    } else {
      trim_and_fill <- FALSE
    }
  }
  
  # Random effects helper for addev contours
  if (method_final == "DL" && addev_contours == TRUE) {
    rem_dl <- function(es, se) {
      summary_es_FEM <- sum((1/se^2) * es)/sum(1/se^2)
      n <- length(es)
      if (n == 1) {
        t2 <- 0
      } else {
        Q <- sum((1/se^2) * (es - summary_es_FEM)^2)
        t2 <- max(c(0, (Q - (n - 1))/(sum(1/se^2) - sum((1/se^2)^2)/sum(1/se^2))))
      }
      w <- 1/(se^2 + t2)
      c(sum(w * es)/sum(w), sqrt(1/sum(w)))
    }
  }
  
  # Y-axis handling
  if (y_axis == "se") {
    plotdata$y <- se_final
    max_se <- max(se_final) + ifelse(diff(range(se_final)) != 0,
                                     diff(range(se_final)) * 0.1, max(se_final) * 0.1)
    y_limit <- c(0, max_se)
    
    if (is.null(ylab)) {
      ylab <- "Standard Error"
    }
    
    if (trim_and_fill == TRUE && nrow(tnfdata) > 0) {
      tnfdata$y <- tnfdata$se
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
        x = c(summary_es - stats::qnorm(0.975) * sqrt(max_se^2 + summary_tau2),
              summary_es - stats::qnorm(0.975) * sqrt(summary_tau2),
              summary_es + stats::qnorm(0.975) * sqrt(summary_tau2),
              summary_es + stats::qnorm(0.975) * sqrt(max_se^2 + summary_tau2)),
        y = c(max_se, 0, 0, max_se)
      )
      min_x <- min(c(min_x, min(funneldata$x)))
      max_x <- max(c(max_x, max(funneldata$x)))
    }
    
    if (egger == TRUE) {
      plotdata <- data.frame(plotdata, z = (plotdata$es)/plotdata$y)
      plotdata <- data.frame(plotdata, prec = 1/plotdata$y)
      radial_intercept <- stats::coef(stats::lm(z ~ prec, data = plotdata))[1]
      radial_slope <- stats::coef(stats::lm(z ~ prec, data = plotdata))[2]
      eggerdata <- data.frame(
        intercept = radial_slope/radial_intercept,
        slope = -1/radial_intercept
      )
    }
    
  } else if (y_axis == "precision") {
    plotdata$y <- 1/se_final
    max_y <- max(1/se_final) + ifelse(diff(range(se_final)) != 0,
                                      diff(range(1/se_final)) * 0.05, max(1/se_final) * 0.05)
    min_y <- min(1/se_final) - ifelse(diff(range(se_final)) != 0,
                                      diff(range(1/se_final)) * 0.05, min(1/se_final) * 0.05)
    
    if (is.null(ylab)) {
      ylab <- "Precision (1/SE)"
    }
    
    if (trim_and_fill == TRUE && exists("tnfdata") && nrow(tnfdata) > 0) {
      tnfdata$y <- 1/tnfdata$se
    }
    
    if (sig_contours == TRUE) {
      n_support <- 200 * detail_level
      prec <- seq(from = min_y, to = max_y, length.out = n_support)
      x_prec_0.05 <- stats::qnorm(0.975) * (1/prec)
      x_prec_0.01 <- stats::qnorm(0.995) * (1/prec)
      sig_funneldata <- data.frame(
        x = c(-x_prec_0.01, rev(x_prec_0.01), x_prec_0.05, rev(-x_prec_0.05)),
        y = c(prec, rev(prec), prec, rev(prec))
      )
      min_x <- min(c(min_x, min(sig_funneldata$x)))
      max_x <- max(c(max_x, max(sig_funneldata$x)))
    }
    
    if (contours == TRUE) {
      n_support <- 200 * detail_level
      prec <- seq(from = min_y, to = max_y, length.out = n_support)
      x_prec <- stats::qnorm(0.975) * sqrt((1/prec)^2 + summary_tau2)
      funneldata <- data.frame(
        x = rep(summary_es, times = n_support * 2) + c(-x_prec, rev(x_prec)),
        y = c(prec, rev(prec))
      )
      min_x <- min(c(min_x, min(funneldata$x)))
      max_x <- max(c(max_x, max(funneldata$x)))
    }
    
    if (egger == TRUE) {
      warning("Note: egger = TRUE ignored: Egger's regression line can only be plotted for y_axis = se")
    }
    
    y_limit <- c(min_y, max_y)
    
  } else {
    stop("y_axis argument must be either se or precision")
  }
  
  x_limit <- c(min_x - diff(c(min_x, max_x)) * 0.05, max_x + diff(c(min_x, max_x)) * 0.05)
  
  # Added-variable contours
  if (addev_contours == TRUE) {
    if (y_axis == "se") {
      y_range <- c(0.001, max_se + diff(range(y_limit)) * 0.2)
      x_range <- c(min_x - diff(range(x_limit)) * 0.2, max_x + diff(range(x_limit)) * 0.2)
      step <- abs(summary_es - x_range[1])/(150 * detail_level - 1)
      x_add <- c(seq(from = x_range[1], to = summary_es, length.out = 150 * detail_level),
                 seq(from = summary_es + step, to = x_range[2], by = step))
      y_add <- seq(from = y_range[1], to = y_range[2], length.out = length(x_add))
    } else {
      y_range <- c(max_y + diff(range(y_limit)) * 0.2, min_y - diff(range(y_limit)) * 0.2)
      x_range <- c(min_x - diff(range(x_limit)) * 0.2, max_x + diff(range(x_limit)) * 0.2)
      step <- abs(summary_es - x_range[1])/(150 * detail_level - 1)
      x_add <- c(seq(from = x_range[1], to = summary_es, length.out = 150 * detail_level),
                 seq(from = summary_es + step, to = x_range[2], by = step))
      y_add <- 1/seq(from = y_range[1], to = y_range[2], length.out = length(x_add))
    }
    
    study_grid <- expand.grid(x_add, y_add)
    names(study_grid) <- c("x_add", "y_add")
    
    addev_data <- apply(study_grid, 1, function(x) {
      if (method_final == "FE") {
        M_new <- sum((1/c(se_final, x[2])^2) * c(es_final, x[1]))/sum(1/c(se_final, x[2])^2)
        Mse_new <- sqrt(1/sum(1/c(se_final, x[2])^2))
        p.val <- stats::pnorm(M_new/Mse_new)
        c(M_new, p.val)
      } else if (method_final == "DL") {
        res_dl <- rem_dl(es = c(es_final, x[1]), se = c(se_final, x[2]))
        M_new <- res_dl[1]
        p.val <- stats::pnorm(res_dl[1]/res_dl[2])
        c(M_new, p.val)
      } else {
        mod <- metafor::rma.uni(yi = c(es_final, x[1]), sei = c(se_final, x[2]),
                                method = method_final,
                                control = list(stepadj = 0.5, maxiter = 1000))
        p.val <- stats::pnorm(mod$z)
        M_new <- mod$b[[1]]
        c(M_new, p.val)
      }
    })
    
    addev_data <- t(addev_data)
    addev_data <- data.frame(
      study_grid,
      M = addev_data[, 1],
      sig_group = factor(
        ifelse(addev_data[, 2] < 0.025, "sig.neg. ",
               ifelse(addev_data[, 2] > 0.975, "sig.pos. ", "not sig. ")),
        levels = c("sig.neg. ", "not sig. ", "sig.pos. ")
      )
    )
    
    addev_data <- addev_data[order(addev_data$x_add, decreasing = FALSE), ]
    
    if (y_axis == "precision") {
      addev_data$y_add <- 1/addev_data$y_add
    }
  }
  
  # Check x_trans_function
  if (!is.null(x_trans_function) && !is.function(x_trans_function)) {
    warning("Argument x_trans_function must be a function; input ignored.")
    x_trans_function <- NULL
  }
  
  # Avoid global variable warnings
  y <- NULL
  sig_group <- NULL
  tnf_summary <- NULL
  intercept <- NULL
  slope <- NULL
  
  # Build plot
  p <- ggplot(data = plotdata, aes(x = es, y = y))
  
  # Add added-variable contours
  if (addev_contours == TRUE) {
    p <- p + geom_raster(data = addev_data, aes(x = x_add, y = y_add, fill = sig_group), alpha = 0.4) +
      scale_fill_manual(name = "", values = c(col[9], col[1], col[4]), drop = FALSE)
  }
  
  # Add significance contours
  if (sig_contours == TRUE) {
    p <- p + geom_polygon(data = sig_funneldata, aes(x = x, y = y),
                          fill = col[9], alpha = 0.6) +
      geom_path(data = sig_funneldata, aes(x = x, y = y))
  }
  
  # Add 95% CI contours
  if (contours == TRUE) {
    p <- p + geom_path(data = funneldata, aes(x = x, y = y)) +
      geom_vline(xintercept = summary_es)
  }
  
  # Y-axis formatting
  if (y_axis == "se") {
    p <- p + scale_y_reverse(name = ylab)
    y_limit <- rev(y_limit)
  } else {
    p <- p + scale_y_continuous(name = ylab)
  }
  
  # Add trim and fill points
  if (trim_and_fill == TRUE && exists("tnfdata") && nrow(tnfdata) > 0) {
    if (is.null(group_final)) {
      p <- p + geom_point(data = tnfdata, aes(x = es, y = y),
                          size = point_size, col = "black", alpha = 1)
    } else {
      p <- p + geom_point(data = tnfdata, aes(x = es, y = y, shape = group),
                          size = point_size, col = "black", alpha = 1)
    }
    if (contours == TRUE) {
      p <- p + geom_vline(data = tnfdata, aes(xintercept = tnf_summary), lty = "dashed")
    }
  }
  
  # Add study points
  if (is.null(group_final)) {
    p <- p + geom_point(size = point_size, fill = "white", shape = 21, col = "black", alpha = 1)
  } else {
    p <- p + geom_point(aes(col = group, shape = group), size = point_size, alpha = 1)
  }
  
  # Add Egger regression line
  if (egger == TRUE && y_axis == "se" && exists("eggerdata")) {
    p <- p + geom_abline(data = eggerdata, aes(intercept = intercept, slope = slope),
                         lty = "dashed", lwd = 1, color = "firebrick")
  }
  
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
  
  # Coordinate limits and aesthetics
  p <- p + coord_cartesian(xlim = x_limit, ylim = y_limit, expand = FALSE) +
    scale_shape_manual(values = 15:19, name = group_legend_title)
  
  # Group color scale: use user-specified colors if provided, otherwise default Set1
  if (!is.null(group_final) && group_legend) {
    if (!is.null(group_colors)) {
      # names(group_colors) must match levels(group_final)
      p <- p + scale_color_manual(
        name   = group_legend_title,
        values = group_colors
      )
    } else {
      p <- p + scale_color_brewer(
        name    = group_legend_title,
        palette = "Set1",
        type    = "qual"
      )
    }
  }
  
  # Hide legend if not requested
  if (group_legend == FALSE) {
    p <- p + guides(color = "none", shape = "none")
  }
  
  # Legend key styling
  if (addev_contours == TRUE) {
    legend.key <- element_rect(color = "black")
  } else {
    legend.key <- element_rect(color = "white")
  }
  
  # Theme
  p <- p + theme_bw() + theme(
    text = element_text(size = 2.5 * text_size),
    legend.position = "bottom",
    legend.key = legend.key,
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 9),
    plot.subtitle = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
  
  return(p)
  
}


#' Funnel Plot Wrapper with Simplified Interface
#'
#' @description
#' Convenient wrapper around viz_funnel_custom() with simplified
#' aesthetic parameters for quick plotting.
#'
#' @param ... Arguments passed to viz_funnel_custom()
#' @param point_color Color for study points (default: NULL = uses shape/group colors)
#' @param point_size Size for study points (default: 2)
#' @param point_shape Shape for study points (default: 19)
#' @param plot_title Plot title (default: "Funnel Plot")
#' @param group_legend Show group legend (default: FALSE)
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' library(metafor)
#' data(dat.bcg)
#' res <- rma(yi, vi, data = dat.bcg)
#' 
#' # Quick funnel plot
#' funnel_custom(res)
#' 
#' # With custom styling
#' funnel_custom(res, point_color = "navy", plot_title = "Publication Bias Check")

funnel_custom <- function(
    ...,
    point_color = NULL,
    point_size = 2,
    point_shape = 19,
    plot_title = "Funnel Plot",
    group_legend = FALSE
) {
  
  # Call the main funnel plot function (without theme_fun)
  p <- viz_funnel_custom(
    ...,
    point_size = point_size,
    group_legend = group_legend
  )
  
  # Only add custom point layer if point_color is specified AND no groups
  if (!is.null(point_color) && !group_legend) {
    p <- p + geom_point(color = point_color, size = point_size, shape = point_shape)
  }
  
  # Add title
  if (!is.null(plot_title)) {
    p <- p + ggtitle(plot_title)
  }
  
  return(p)
}

