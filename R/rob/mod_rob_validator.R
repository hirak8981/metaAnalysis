# =============================================================================
# ROB Data Validator Module (FIXED - Better Tool Detection)
# =============================================================================

#' Validate ROB Dataset
#' @param data Data frame to validate
#' @return List with 'valid' (TRUE/FALSE), 'errors' (character vector), 'tool' (detected tool name)

validate_rob_data <- function(data) {
  
  errors <- character(0)
  detected_tool <- NULL
  
  # Check 1: Data exists
  if (is.null(data) || nrow(data) == 0) {
    return(list(valid = TRUE, errors = character(0), tool = NULL))  # Empty data is OK (not invalid)
  }
  
  # Check 2: Study column exists
  if (!"Study" %in% names(data)) {
    errors <- c(errors, "Missing required column: 'Study' (case-sensitive)")
  }
  
  # Check 3: Overall column exists
  if (!"Overall" %in% names(data)) {
    errors <- c(errors, "Missing required column: 'Overall' (case-sensitive)")
  }
  
  # Check 4: Domain columns exist (D1, D2, etc.)
  domain_cols <- grep("^D[0-9]+$", names(data), value = TRUE)
  
  if (length(domain_cols) == 0) {
    errors <- c(errors, "No domain columns found (expected: D1, D2, D3, etc.)")
  } else {
    # Check domain columns are sequential
    domain_nums <- as.numeric(gsub("D", "", domain_cols))
    domain_nums_sorted <- sort(domain_nums)
    expected_sequence <- 1:length(domain_nums_sorted)
    
    if (!all(domain_nums_sorted == expected_sequence)) {
      errors <- c(errors, "Domain columns must be sequential (D1, D2, D3, etc. with no gaps)")
    }
  }
  
  # Detect tool based on ACTUAL VALUES (not just column count)
  if (length(errors) == 0) {
    detected_tool <- detect_rob_tool_smart(data, domain_cols)
    
    if (is.null(detected_tool)) {
      n_domains <- length(domain_cols)
      errors <- c(errors, sprintf(
        "Cannot determine ROB tool from data values. Found %d domain columns. Check your risk values match a supported tool.",
        n_domains
      ))
    }
  }
  
  # Check 5: No missing values in critical columns
  if (length(errors) == 0) {
    critical_cols <- c("Study", domain_cols, "Overall")
    
    for (col in critical_cols) {
      if (any(is.na(data[[col]]) | trimws(as.character(data[[col]])) == "")) {
        errors <- c(errors, sprintf("Column '%s' contains empty or missing values", col))
        break
      }
    }
  }
  
  # Check 6: Valid risk values (case-insensitive)
  if (length(errors) == 0 && !is.null(detected_tool)) {
    valid_values <- get_valid_values(detected_tool)
    
    for (col in c(domain_cols, "Overall")) {
      unique_vals <- unique(trimws(as.character(data[[col]])))
      
      # Case-insensitive comparison
      invalid_vals <- unique_vals[!tolower(unique_vals) %in% tolower(valid_values)]
      
      if (length(invalid_vals) > 0) {
        errors <- c(errors, sprintf(
          "Column '%s' contains invalid values: %s. Expected for %s: %s",
          col,
          paste(invalid_vals, collapse = ", "),
          detected_tool,
          paste(valid_values, collapse = ", ")
        ))
        break
      }
    }
  }
  
  # Return validation result
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    tool = detected_tool
  ))
}

#' Smart tool detection based on actual risk values in data
#' @param data Data frame
#' @param domain_cols Vector of domain column names
#' @return Tool name or NULL

detect_rob_tool_smart <- function(data, domain_cols) {
  # Get all unique risk values from domain columns (case-insensitive)
  all_risk_values <- data %>%
    select(all_of(c(domain_cols, "Overall"))) %>%
    unlist() %>%
    unique() %>%
    na.omit() %>%
    trimws() %>%
    tolower()
  
  n_domains <- length(domain_cols)
  
  # Signature values for each tool
  has_unclear <- "unclear" %in% all_risk_values
  has_some_concerns <- "some concerns" %in% all_risk_values
  has_moderate <- "moderate" %in% all_risk_values
  has_serious <- "serious" %in% all_risk_values
  has_critical <- "critical" %in% all_risk_values
  has_very_high <- "very high" %in% all_risk_values
  
  # QUADAS-2: 4 domains, uses "Unclear"
  if (n_domains == 4 && has_unclear) {
    return("QUADAS-2")
  }
  
  # ROBINS-I: 7 domains, uses "Moderate", "Serious", "Critical"
  if (n_domains == 7 && has_moderate && has_serious && has_critical) {
    return("ROBINS-I")
  }
  
  # ROBINS-E: 7 domains, uses "Some concerns", "Very high"
  if (n_domains == 7 && has_some_concerns && has_very_high) {
    return("ROBINS-E")
  }
  
  # ROB 2: 5 domains, uses "Some concerns" (not "Moderate" or "Serious")
  if (n_domains == 5 && has_some_concerns && !has_moderate && !has_serious) {
    return("ROB 2")
  }
  
  # QUIPS: 6 domains, uses "Moderate"
  if (n_domains == 6 && has_moderate) {
    return("QUIPS")
  }
  
  # Fallback: couldn't detect
  return(NULL)
}

#' Get valid values for a tool
#' @param tool_name Tool name
#' @return Character vector of valid values

get_valid_values <- function(tool_name) {
  valid_map <- list(
    "QUADAS-2" = c("Low", "High", "Unclear"),
    "ROB 2" = c("Low", "Some concerns", "High", "No information"),
    "QUIPS" = c("Low", "Moderate", "High", "No information"),
    "ROBINS-I" = c("Low", "Moderate", "Serious", "Critical", "No information"),
    "ROBINS-E" = c("Low", "Some concerns", "High", "Very high", "No information")
  )
  
  return(valid_map[[tool_name]])
}

#' Show validation error modal
#' @param errors Character vector of error messages

show_validation_error <- function(errors) {
  showModal(
    modalDialog(
      title = tagList(
        icon("exclamation-triangle", style = "color: #dc3545;"),
        " Invalid Risk of Bias Dataset"
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
            " button in the sidebar for detailed instructions on formatting your Risk of Bias dataset.",
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
