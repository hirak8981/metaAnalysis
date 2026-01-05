# ==============================================================================
# SCRIPT 1: nma_plot_utils.R
# Color Generation and Data Extraction Utilities for Network Meta-Analysis
# ==============================================================================

#' Get Treatment Information from netmeta Object
#'
#' Extracts treatment names and count from a netmeta network meta-analysis object.
#'
#' @param nma_obj A netmeta object from the netmeta package
#'
#' @return A list containing:
#'   \item{names}{Character vector of treatment names}
#'   \item{n}{Integer, number of treatments}
#'
#' @examples
#' \dontrun{
#' nma_con <- netmeta(TE, seTE, treat1, treat2, studlab, data = pw_data)
#' tx_info <- get_treatments_from_nma(nma_con)
#' print(tx_info$names)
#' print(tx_info$n)
#' }
#'
#' @export
get_treatments_from_nma <- function(nma_obj) {
  
  # Extract treatment names from netmeta object
  treatment_names <- nma_obj$trts
  
  # Number of treatments
  n_treatments <- length(treatment_names)
  
  # Return as list
  return(list(
    names = treatment_names,
    n = n_treatments
  ))
}

#' Generate Treatment Colors from Names
#'
#' Generates a named vector of colors for treatments based on various color palettes.
#' Supports multiple palettes including colorblind-friendly options.
#'
#' @param treatment_names Character vector of treatment names
#' @param palette Character string specifying color palette. Options:
#'   - `default` - ColorBrewer Set1 palette
#'   - `colorblind` - Okabe-Ito colorblind-friendly palette  
#'   - `vibrant` - Vibrant color scheme
#'   - `pastel` - Pastel color scheme
#'   - `dark` - Dark ColorBrewer palette
#'   - `rainbow` - Rainbow colors (use cautiously)
#'   - `viridis` - Viridis color scale
#'   - `plasma` - Plasma color scale
#'   - `tableau` - Tableau color palette
#'   - `magma` - Magma color scale (NEW)
#'   - `cividis` - Cividis colorblind-friendly scale (NEW)
#'   - `turbo` - Turbo high-contrast scale (NEW)
#'   - `muted` - Muted soft colors (NEW)
#' @param seed Integer for random seed to ensure reproducibility
#' @return Named character vector of hex color codes
#' @export
generate_treatment_colors <- function(treatment_names, palette = "default", seed = 123) {
  
  n_treatments <- length(treatment_names)
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Generate colors based on palette choice
  colors <- switch(
    palette,
    
    # Default: Colorbrewer Set1 + additional colors
    "default" = {
      if (n_treatments <= 9) {
        RColorBrewer::brewer.pal(max(3, n_treatments), "Set1")[1:n_treatments]
      } else {
        colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n_treatments)
      }
    },
    
    # Colorblind-friendly palette (Okabe-Ito)
    "colorblind" = {
      okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                     "#0072B2", "#D55E00", "#CC79A7", "#999999")
      if (n_treatments <= 8) {
        okabe_ito[1:n_treatments]
      } else {
        colorRampPalette(okabe_ito)(n_treatments)
      }
    },
    
    # Vibrant colors
    "vibrant" = {
      colorRampPalette(c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", 
                         "#98D8C8", "#F7DC6F", "#BB8FCE", "#85C1E2"))(n_treatments)
    },
    
    # Pastels
    "pastel" = {
      colorRampPalette(c("#FFB3BA", "#BAFFC9", "#BAE1FF", "#FFFFBA", 
                         "#FFDFBA", "#E0BBE4", "#FFDFD3", "#C7CEEA"))(n_treatments)
    },
    
    # Dark colors
    "dark" = {
      colorRampPalette(RColorBrewer::brewer.pal(max(3, min(8, n_treatments)), "Dark2"))(n_treatments)
    },
    
    # Rainbow (use cautiously)
    "rainbow" = {
      rainbow(n_treatments, s = 0.8, v = 0.8)
    },
    
    # Viridis
    "viridis" = {
      viridis::viridis(n_treatments, option = "D")
    },
    
    # Plasma
    "plasma" = {
      viridis::plasma(n_treatments)
    },
    
    # Tableau
    "tableau" = {
      if (n_treatments <= 10) {
        scales::hue_pal()(n_treatments)
      } else {
        colorRampPalette(scales::hue_pal()(10))(n_treatments)
      }
    },
    
    # ✅ NEW: Magma
    "magma" = {
      viridis::magma(n_treatments)
    },
    
    # ✅ NEW: Cividis (colorblind-friendly)
    "cividis" = {
      viridis::cividis(n_treatments)
    },
    
    # ✅ NEW: Turbo (high contrast)
    "turbo" = {
      viridis::turbo(n_treatments)
    },
    
    # ✅ NEW: Muted colors (Paul Tol)
    "muted" = {
      colorRampPalette(c("#88CCEE", "#CC6677", "#DDCC77", "#117733",
                         "#332288", "#AA4499", "#44AA99", "#999933"))(n_treatments)
    },
    
    # Default fallback
    RColorBrewer::brewer.pal(max(3, min(n_treatments, 9)), "Set1")[1:n_treatments]
  )
  
  # Create named vector
  color_vector <- setNames(colors, treatment_names)
  
  return(color_vector)
}


#' Generate Colors Directly from netmeta Object
#'
#' Convenience wrapper that extracts treatment names from a netmeta object
#' and generates corresponding colors.
#'
#' @param nma_obj A netmeta object
#' @param palette Character string specifying color palette (see \code{\link{generate_treatment_colors}})
#' @param seed Integer for random seed
#'
#' @return Named character vector of hex color codes
#'
#' @examples
#' \dontrun{
#' colors <- generate_colors_from_nma(nma_con, palette = "colorblind")
#' print(colors)
#' }
#'
#' @seealso \code{\link{generate_treatment_colors}}
#'
#' @export
generate_colors_from_nma <- function(nma_obj, 
                                     palette = "default",
                                     seed = 123) {
  
  # Extract treatment info
  tx_info <- get_treatments_from_nma(nma_obj)
  
  # Generate colors using existing function
  colors <- generate_treatment_colors(
    treatment_names = tx_info$names,
    palette = palette,
    seed = seed
  )
  
  return(colors)
}

#' Create Treatment Colors with Auto or Manual Options
#'
#' Smart dispatcher function that handles both automatic color generation
#' and manual color specification. Validates manual colors and provides
#' helpful error messages.
#'
#' @param nma_obj A netmeta object
#' @param colors Optional named or unnamed vector of colors for manual specification.
#'   If NULL, colors are auto-generated. If named, names must match treatment names.
#'   If unnamed, vector length must equal number of treatments.
#' @param palette Character string specifying auto-generation palette (used only if colors is NULL)
#' @param seed Integer for random seed (used only if colors is NULL)
#'
#' @return Named character vector of hex color codes matching treatment order
#'
#' @details
#' This function provides three ways to specify colors:
#' \enumerate{
#'   \item Auto-generation: Set colors = NULL and specify palette
#'   \item Manual named vector: Provide colors with treatment names
#'   \item Manual unnamed vector: Provide colors in treatment order
#' }
#'
#' @examples
#' \dontrun{
#' # Auto-generate
#' colors1 <- create_treatment_colors(nma_con, palette = "colorblind")
#'
#' # Manual named
#' colors2 <- create_treatment_colors(
#'   nma_con,
#'   colors = c("Drug A" = "#FF0000", "Drug B" = "#00FF00", "Placebo" = "#0000FF")
#' )
#'
#' # Manual unnamed
#' colors3 <- create_treatment_colors(nma_con, colors = c("#FF0000", "#00FF00", "#0000FF"))
#' }
#'
#' @export
create_treatment_colors <- function(nma_obj,
                                    colors = NULL,
                                    palette = "default",
                                    seed = 123) {
  
  # Get treatment info from nma object
  tx_info <- get_treatments_from_nma(nma_obj)
  treatment_names <- tx_info$names
  n_treatments <- tx_info$n
  
  # CASE 1: User provides explicit colors
  if (!is.null(colors)) {
    
    # If colors have names (best practice)
    if (!is.null(names(colors))) {
      # Check if all treatment names are present
      missing_treatments <- setdiff(treatment_names, names(colors))
      
      if (length(missing_treatments) == 0) {
        message("\u2713 Using user-provided named colors")
        return(colors[treatment_names])  # Return in correct order
      } else {
        stop(paste0("\u2717 Missing colors for treatments: ", 
                    paste(missing_treatments, collapse = ", "),
                    "\n  Available treatments: ", 
                    paste(treatment_names, collapse = ", ")))
      }
    }
    
    # If colors are unnamed vector
    else {
      if (length(colors) == n_treatments) {
        message("\u2713 Using user-provided colors (assigning to treatments in order)")
        return(setNames(colors, treatment_names))
      } else {
        stop(paste0("\u2717 Number of colors (", length(colors), 
                    ") must match number of treatments (", n_treatments, ")",
                    "\n  Treatments: ", paste(treatment_names, collapse = ", ")))
      }
    }
  }
  
  # CASE 2: Auto-generate from palette
  else {
    message(paste0("\u2713 Auto-generating colors using '", palette, 
                   "' palette for ", n_treatments, " treatments"))
    return(generate_colors_from_nma(nma_obj, palette, seed))
  }
}

#' Prepare Ranking Data from netmeta Object
#'
#' Prepares ranking metrics data from a netmeta object using the rankinma package.
#' Supports SUCRA, P-score, and P-best metrics.
#'
#' @param nma_obj A netmeta object
#' @param outcome_name Character string describing the outcome (e.g., "MeanDifference", "OddsRatio")
#' @param prefer Character string indicating preference direction: "small" or "large"
#' @param metrics_type Character string specifying metric type: "SUCRA", "P-score", or "P-best"
#' @param model Character string specifying model: "random" or "common"
#' @param nsim Integer specifying number of simulations for ranking calculations
#'
#' @return A rankinma object containing ranking metrics and treatment information
#'
#' @details
#' This function uses the rankinma package to calculate ranking metrics.
#' The metrics_type parameter determines which ranking metric is calculated:
#' \itemize{
#'   \item SUCRA: Surface Under the Cumulative Ranking curve
#'   \item P-score: Frequentist analogue of SUCRA
#'   \item P-best: Probability of being the best treatment
#' }
#'
#' @examples
#' \dontrun{
#' data_sucra <- prepare_ranking_data(
#'   nma_con,
#'   outcome_name = "MeanDifference",
#'   prefer = "small",
#'   metrics_type = "SUCRA",
#'   model = "random",
#'   nsim = 10000
#' )
#' }
#'
#' @export
prepare_ranking_data <- function(nma_obj, 
                                 outcome_name,
                                 prefer = "small",
                                 metrics_type = "SUCRA",
                                 model = "random",
                                 nsim = 10000) {
  
  # Validate metrics type
  metrics_type <- match.arg(metrics_type, c("SUCRA", "P-score", "P-best"))
  
  # Get metrics using rankinma
  nma_metrics <- rankinma::GetMetrics(
    nma_obj,
    outcome = outcome_name,
    prefer = prefer,
    metrics = metrics_type,
    model = model,
    simt = nsim
  )
  
  # Set data in rankinma format
  # Note: Column names are converted (e.g., "P-score" becomes "P.score")
  if (metrics_type == "SUCRA") {
    data_rankinma <- rankinma::SetMetrics(
      nma_metrics,
      tx = tx,
      outcome = outcome,
      metrics = SUCRA,
      metrics.name = "SUCRA"
    )
  } else if (metrics_type == "P-score") {
    data_rankinma <- rankinma::SetMetrics(
      nma_metrics,
      tx = tx,
      outcome = outcome,
      metrics = P.score,  # Note: dot, not hyphen
      metrics.name = "P-score"
    )
  } else {  # P-best
    data_rankinma <- rankinma::SetMetrics(
      nma_metrics,
      tx = tx,
      outcome = outcome,
      metrics = P.best,  # Note: dot, not hyphen
      metrics.name = "P-best"
    )
  }
  
  return(data_rankinma)
}
