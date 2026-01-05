# =============================================================================
# Meta-Analysis Data Validator
# =============================================================================

#' Validate Meta-Analysis Dataset
#' @param data Data frame to validate
#' @return List with 'valid' (TRUE/FALSE), 'errors' (character vector), 'data_type' (detected type)
validate_meta_data <- function(data) {
  errors <- character(0)
  detected_type <- NULL
  
  # Check 1: Data exists
  if (is.null(data) || nrow(data) == 0) {
    return(list(valid = TRUE, errors = character(0), data_type = NULL))
  }
  
  # Check 2: Study column exists
  if (!"study" %in% tolower(names(data))) {
    errors <- c(errors, "Missing required column: 'study' (case-insensitive)")
  }
  
  # Detect data type and validate specific columns
  detected_type <- detect_meta_data_type(data)
  
  if (is.null(detected_type)) {
    errors <- c(errors, "Cannot determine data type. Expected columns for:
                         Continuous: n.e, mean.e, sd.e, n.c, mean.c, sd.c
                         Dichotomous: event.e, n.e, event.c, n.c
                         Correlation: r, n")
  } else {
    # Validate based on detected type
    if (detected_type == "continuous") {
      required <- c("n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c")
      missing <- setdiff(tolower(required), tolower(names(data)))
      if (length(missing) > 0) {
        errors <- c(errors, paste("Missing columns for continuous data:", paste(missing, collapse = ", ")))
      }
    } else if (detected_type == "dichotomous") {
      required <- c("event.e", "n.e", "event.c", "n.c")
      missing <- setdiff(tolower(required), tolower(names(data)))
      if (length(missing) > 0) {
        errors <- c(errors, paste("Missing columns for dichotomous data:", paste(missing, collapse = ", ")))
      }
    } else if (detected_type == "correlation") {
      required <- c("r", "n")
      missing <- setdiff(tolower(required), tolower(names(data)))
      if (length(missing) > 0) {
        errors <- c(errors, paste("Missing columns for correlation data:", paste(missing, collapse = ", ")))
      }
    }
  }
  
  # Check 3: No missing values in critical columns
  if (length(errors) == 0 && !is.null(detected_type)) {
    study_col <- names(data)[which(tolower(names(data)) == "study")]
    if (any(is.na(data[[study_col]]) | trimws(as.character(data[[study_col]])) == "")) {
      errors <- c(errors, "Study column contains empty or missing values")
    }
  }
  
  # Return validation result
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    data_type = detected_type
  ))
}

#' Detect meta-analysis data type from columns
#' @param data Data frame
#' @return Data type ("continuous", "dichotomous", "correlation") or NULL
detect_meta_data_type <- function(data) {
  col_names_lower <- tolower(names(data))
  
  # Check for continuous data
  continuous_cols <- c("n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c")
  if (all(continuous_cols %in% col_names_lower)) {
    return("continuous")
  }
  
  # Check for dichotomous data
  dichotomous_cols <- c("event.e", "n.e", "event.c", "n.c")
  if (all(dichotomous_cols %in% col_names_lower)) {
    return("dichotomous")
  }
  
  # Check for correlation data
  correlation_cols <- c("r", "n")
  if (all(correlation_cols %in% col_names_lower)) {
    return("correlation")
  }
  
  return(NULL)
}

#' Show validation error modal
#' @param errors Character vector of error messages
show_meta_validation_error <- function(errors) {
  showModal(
    modalDialog(
      title = tagList(
        icon("exclamation-triangle", style = "color: #dc3545;"),
        " Invalid Meta-Analysis Dataset"
      ),
      div(
        style = "padding: 15px;",
        
        # Error header
        div(
          style = "background: #f8d7da; padding: 15px; border-radius: 6px; 
                   border-left: 4px solid #dc3545; margin-bottom: 20px;",
          h5(
            icon("times-circle", style = "color: #721c24;"),
            " Data Validation Failed",
            style = "color: #721c24; margin-top: 0;"
          ),
          p(
            "Your dataset does not meet the required format. Please correct the following issues:",
            style = "color: #721c24; margin: 0;"
          )
        ),
        
        # Error list
        div(
          style = "background: white; padding: 15px; border-radius: 6px; margin-bottom: 20px;",
          h6("Issues Found:", style = "color: #1d3557; margin-top: 0;"),
          tags$ul(
            style = "color: #5a6169; margin: 0; padding-left: 25px;",
            lapply(errors, function(err) {
              tags$li(err)
            })
          )
        ),
        
        # Help section
        div(
          style = "background: #E8F4F8; padding: 15px; border-radius: 6px; 
                   border-left: 4px solid #457b9d;",
          p(
            icon("info-circle", style = "color: #457b9d;"),
            strong(" Need Help?"),
            style = "color: #1d3557; margin: 0 0 10px 0;"
          ),
          p(
            "Click the ",
            tags$strong("'Data Format Guide'"),
            " button in the sidebar for detailed instructions on formatting your meta-analysis dataset.",
            style = "color: #5a6169; margin: 0;"
          )
        )
      ),
      footer = tagList(
        modalButton("Close", icon = icon("times"))
      ),
      size = "l",
      easyClose = TRUE,
      fade = TRUE
    )
  )
}
