# =============================================================================
# Meta-Analysis Engine - Unified Analysis Runner
# =============================================================================

#' Run meta-analysis based on data type and settings
#' @param data Data frame with meta-analysis data
#' @param data_type Type of data: "continuous", "dichotomous", or "correlation"
#' @param measure Effect size measure (OR, RR, MD, SMD, ZCOR, etc.)
#' @param method Meta-analysis method (REML, ML, DL, FE, HS)
#' @param transform Transformation function (exp, tanh, or NULL)
#' @param subgroup Column name for subgroup analysis (NULL for none)
#' @param confidence_level Confidence level (default 0.95)
#' @return List with meta-analysis results
run_meta_analysis <- function(data, 
                              data_type, 
                              measure, 
                              method = "REML",
                              transform = NULL,
                              subgroup = NULL,
                              confidence_level = 0.95) {
  
  # Standardize column names to lowercase
  names(data) <- tolower(names(data))
  
  # Determine transform name
  transform_name <- if (is.null(transform)) {
    "None"
  } else if (identical(transform, exp)) {
    "Exponential"
  } else if (identical(transform, tanh)) {
    "Hyperbolic Tangent"
  } else {
    "Custom"
  }
  
  # Prepare moderator argument (handle NULL properly)
  moderator_arg <- if (!is.null(subgroup)) {
    rlang::sym(subgroup)
  } else {
    NULL
  }
  
  # Run analysis based on data type
  if (data_type == "continuous") {
    
    # Build arguments list
    args <- list(
      data = data,
      measure = measure,
      mean_exp = rlang::sym("mean.e"),
      sd_exp = rlang::sym("sd.e"),
      n_exp = rlang::sym("n.e"),
      mean_ctrl = rlang::sym("mean.c"),
      sd_ctrl = rlang::sym("sd.c"),
      n_ctrl = rlang::sym("n.c"),
      method = method,
      transform = transform,
      transform_name = transform_name,
      conf_level = confidence_level
    )
    
    # Add moderator if provided
    if (!is.null(moderator_arg)) {
      args$moderator <- moderator_arg
    }
    
    results <- do.call(perform_meta_analysis2, args)
    
  } else if (data_type == "dichotomous") {
    
    # Build arguments list
    args <- list(
      data = data,
      measure = measure,
      event_exp = rlang::sym("event.e"),
      n_exp = rlang::sym("n.e"),
      event_ctrl = rlang::sym("event.c"),
      n_ctrl = rlang::sym("n.c"),
      method = method,
      transform = transform,
      transform_name = transform_name,
      conf_level = confidence_level
    )
    
    # Add moderator if provided
    if (!is.null(moderator_arg)) {
      args$moderator <- moderator_arg
    }
    
    results <- do.call(perform_meta_analysis2, args)
    
  } else if (data_type == "correlation") {
    
    # Build arguments list
    args <- list(
      data = data,
      measure = measure,
      ri = rlang::sym("r"),
      ni = rlang::sym("n"),
      method = method,
      transform = transform,
      transform_name = transform_name,
      conf_level = confidence_level
    )
    
    # Add moderator if provided
    if (!is.null(moderator_arg)) {
      args$moderator <- moderator_arg
    }
    
    results <- do.call(perform_meta_analysis2, args)
    
  } else {
    stop("Unsupported data type: ", data_type)
  }
  
  return(results)
}
