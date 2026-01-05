# =============================================================================
# Network Meta-Analysis Data Validator
# =============================================================================

#' Validate Network Meta-Analysis Dataset
#' @param data Data frame to validate
#' @return List with 'valid' (TRUE/FALSE), 'errors' (character vector), 'data_type' (detected type)
validate_nma_data <- function(data) {
  errors <- character(0)
  detected_type <- NULL
  
  # Check 1: Data exists
  if (is.null(data) || nrow(data) == 0) {
    return(list(valid = TRUE, errors = character(0), data_type = NULL))
  }
  
  # Check 2: Required base columns
  required_base <- c("study", "treatment", "n")
  col_names_lower <- tolower(names(data))
  
  for (col in required_base) {
    if (!col %in% col_names_lower) {
      errors <- c(errors, paste0("Missing required column: '", col, "' (case-insensitive)"))
    }
  }
  
  # Detect data type and validate specific columns
  detected_type <- detect_nma_data_type(data)
  
  if (is.null(detected_type)) {
    errors <- c(errors, "Cannot determine data type. Expected columns for:
Continuous: study, treatment, mean, sd, n
Dichotomous: study, treatment, events, n")
  } else {
    # Validate based on detected type
    if (detected_type == "continuous") {
      required <- c("study", "treatment", "mean", "sd", "n")
      missing <- setdiff(tolower(required), col_names_lower)
      if (length(missing) > 0) {
        errors <- c(errors, paste("Missing columns for continuous data:", paste(missing, collapse = ", ")))
      }
    } else if (detected_type == "dichotomous") {
      required <- c("study", "treatment", "events", "n")
      missing <- setdiff(tolower(required), col_names_lower)
      if (length(missing) > 0) {
        errors <- c(errors, paste("Missing columns for dichotomous data:", paste(missing, collapse = ", ")))
      }
    }
  }
  
  # Check 3: No missing values in critical columns
  if (length(errors) == 0 && !is.null(detected_type)) {
    study_col <- names(data)[which(tolower(names(data)) == "study")]
    treatment_col <- names(data)[which(tolower(names(data)) == "treatment")]
    
    if (any(is.na(data[[study_col]]) | trimws(as.character(data[[study_col]])) == "")) {
      errors <- c(errors, "Study column contains empty or missing values")
    }
    
    if (any(is.na(data[[treatment_col]]) | trimws(as.character(data[[treatment_col]])) == "")) {
      errors <- c(errors, "Treatment column contains empty or missing values")
    }
  }
  
  # Check 4: Network requirements (minimum studies and treatments)
  if (length(errors) == 0) {
    n_studies <- length(unique(data[[names(data)[which(tolower(names(data)) == "study")]]]))
    n_treatments <- length(unique(data[[names(data)[which(tolower(names(data)) == "treatment")]]]))
    
    if (n_studies < 2) {
      errors <- c(errors, "Network meta-analysis requires at least 2 studies")
    }
    
    if (n_treatments < 3) {
      errors <- c(errors, "Network meta-analysis requires at least 3 treatments")
    }
  }
  
  # Return validation result
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    data_type = detected_type
  ))
}

#' Detect network meta-analysis data type from columns
#' @param data Data frame
#' @return Data type ("continuous", "dichotomous") or NULL
detect_nma_data_type <- function(data) {
  col_names_lower <- tolower(names(data))
  
  # Check for continuous data
  continuous_cols <- c("study", "treatment", "mean", "sd", "n")
  if (all(continuous_cols %in% col_names_lower)) {
    return("continuous")
  }
  
  # Check for dichotomous data
  dichotomous_cols <- c("study", "treatment", "events", "n")
  if (all(dichotomous_cols %in% col_names_lower)) {
    return("dichotomous")
  }
  
  return(NULL)
}

#' Show validation error modal for NMA
#' @param errors Character vector of error messages
show_nma_validation_error <- function(errors) {
  showModal(
    modalDialog(
      title = tagList(
        icon("exclamation-triangle", style = "color: #dc3545;"),
        " Invalid Network Meta-Analysis Dataset"
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
            "Your dataset does not meet the required format for network meta-analysis. Please correct the following issues:",
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
            strong(" Expected Format:"),
            style = "color: #1d3557; margin: 0 0 10px 0;"
          ),
          p(
            strong("Continuous:"), " study, treatment, mean, sd, n", br(),
            strong("Dichotomous:"), " study, treatment, events, n", br(), br(),
            "Network meta-analysis requires at least 2 studies and 3 treatments.",
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
