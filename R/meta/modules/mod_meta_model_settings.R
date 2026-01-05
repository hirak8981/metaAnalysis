# =============================================================================
# Meta-Analysis Model Settings Module
# =============================================================================

metaModelSettingsUI <- function(id) {
  ns <- NS(id)
  # This module uses a modal, so no UI element needed here
  NULL
}

metaModelSettingsServer <- function(id, current_data = reactive(NULL), data_type = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive to store current settings
    settings <- reactiveVal(list(
      measure = NULL,
      method = "REML",
      transform = NULL,
      transform_char = "none",
      subgroup = NULL,
      confidence_level = 0.95
    ))
    
    # Show modal when triggered
    show_settings_modal <- function() {
      
      current_settings <- isolate(settings())
      current_dtype <- isolate(data_type())
      current_dat <- isolate(current_data())
      
      # Get available measures based on data type
      measure_choices <- get_measure_choices(current_dtype)
      # Get subgroup columns from data
      subgroup_choices <- get_subgroup_choices(current_dat)
      
      showModal(
        modalDialog(
          title = tagList(
            icon("sliders", style = "color: #457b9d;"),
            " Meta-Analysis Model Settings"
          ),
          size = "l",
          easyClose = FALSE,
          fade = TRUE,
          
          div(
            style = "padding: 15px;",
            
            # Data type indicator
            div(
              style = "background: #E8F4F8; padding: 12px; border-radius: 6px; 
                       border-left: 4px solid #457b9d; margin-bottom: 20px;",
              h6(
                icon("info-circle", style = "color: #457b9d;"),
                " Detected Data Type",
                style = "color: #1d3557; margin: 0 0 5px 0;"
              ),
              p(
                strong(format_data_type_name(data_type())),
                style = "color: #5a6169; margin: 0;"
              )
            ),
            
            # Effect Measure Selection
            div(
              style = "margin-bottom: 20px;",
              h5("Effect Size Measure", style = "color: #1d3557; margin-bottom: 10px;"),
              selectInput(
                session$ns("measure"),
                NULL,
                choices = measure_choices,
                selected = current_settings$measure
              ),
              tags$small(
                "Select the effect size metric for your analysis",
                style = "color: #6c757d;"
              )
            ),
            
            # Transformation Selection
            div(
              style = "margin-bottom: 20px;",
              h5("Display Transformation", style = "color: #1d3557; margin-bottom: 10px;"),
              selectInput(
                session$ns("transform"),
                NULL,
                choices = c(
                  "None (analysis scale)" = "none",
                  "Exponential (exp) - for OR/RR/ROM" = "exp",
                  "Hyperbolic tangent (tanh) - for Fisher's Z" = "tanh"
                ),
                selected = if (!is.null(current_settings$transform_char)) {
                  current_settings$transform_char
                } else if (!is.null(current_settings$transform)) {
                  if (is.function(current_settings$transform)) {
                    if (identical(current_settings$transform, exp)) "exp"
                    else if (identical(current_settings$transform, tanh)) "tanh"
                    else "none"
                  } else "none"
                } else "none"
                
              ),
              tags$small(
                "Transform results for display (does not affect analysis)",
                style = "color: #6c757d;"
              )
            ),
            
            # Method Selection
            div(
              style = "margin-bottom: 20px;",
              h5("Meta-Analysis Method", style = "color: #1d3557; margin-bottom: 10px;"),
              selectInput(
                session$ns("method"),
                NULL,
                choices = c(
                  "REML (Restricted Maximum Likelihood)" = "REML",
                  "ML (Maximum Likelihood)" = "ML",
                  "DL (DerSimonian-Laird)" = "DL",
                  "FE (Fixed Effects)" = "FE",
                  "HS (Hunter-Schmidt)" = "HS"
                ),
                selected = current_settings$method
              ),
              tags$small(
                "REML is recommended for most meta-analyses",
                style = "color: #6c757d;"
              )
            ),
            
            # Subgroup Analysis (if applicable)
            if (length(subgroup_choices) > 0) {
              div(
                style = "margin-bottom: 20px;",
                h5("Subgroup Analysis (Optional)", style = "color: #1d3557; margin-bottom: 10px;"),
                selectInput(
                  session$ns("subgroup"),
                  NULL,
                  choices = c("None" = "", subgroup_choices),
                  selected = ifelse(is.null(current_settings$subgroup), "", current_settings$subgroup)
                  
                ),
                tags$small(
                  "Select a categorical variable for subgroup analysis",
                  style = "color: #6c757d;"
                )
              )
            } else {
              NULL
            },
            
            # Confidence Level
            div(
              style = "margin-bottom: 20px;",
              h5("Confidence Level", style = "color: #1d3557; margin-bottom: 10px;"),
              numericInput(
                session$ns("confidence"),
                NULL,
                value = current_settings$confidence_level,
                min = 0.80,
                max = 0.99,
                step = 0.01
              ),
              tags$small(
                "Typically 0.95 (95% CI)",
                style = "color: #6c757d;"
              )
            )
          ),
          
          footer = tagList(
            modalButton("Cancel", icon = icon("times")),
            actionButton(
              session$ns("save_settings"),
              "Save Settings",
              icon = icon("check"),
              class = "btn-primary"
            )
          )
        )
      )
    }
    
    # ========================================================================
    # SAVE SETTINGS OBSERVER - FIXED
    # ========================================================================
    observeEvent(input$save_settings, {
      # ✅ FIX: Use req() to ensure inputs exist
      req(input$measure, input$method, input$transform, input$confidence)
      
      # Get transform value (store as CHARACTER, not function)
      transform_char <- input$transform  # ✅ Store the character value
      
      # Also get the actual function for the analysis
      transform_func <- switch(input$transform,
                               "none" = NULL,
                               "exp" = exp,
                               "tanh" = tanh,
                               NULL
      )
      
      # Get subgroup value
      subgroup_val <- NULL
      if (!is.null(input$subgroup) && nchar(input$subgroup) > 0 && input$subgroup != "") {
        subgroup_val <- input$subgroup
      }
      
      # ✅ FIX: Update settings with BOTH character and function
      new_settings <- list(
        measure = input$measure,
        method = input$method,
        transform = transform_func,  # Function for analysis
        transform_char = transform_char,  # ✅ NEW: Character for display
        transform_name = input$transform,  # Keep for compatibility
        subgroup = subgroup_val,
        confidence_level = input$confidence
      )
      
      settings(new_settings)
      
      removeModal()
      
      showNotification(
        "Model settings saved successfully!",
        type = "message",
        duration = 3
      )
    })
    
    
    
    
    # Return reactive values and modal trigger
    return(list(
      settings = settings,
      show_modal = show_settings_modal
    ))
  })
}

# Helper: Get measure choices based on data type
get_measure_choices <- function(data_type) {
  if (is.null(data_type)) {
    return(c("Please load data first" = ""))
  }
  
  if (data_type == "continuous") {
    return(c(
      "MD - Mean Difference" = "MD",
      "SMD - Standardized Mean Difference (Cohen's d)" = "SMD",
      "SMDH - Hedges' g (bias-corrected SMD)" = "SMDH",
      "SMD1 - SMD using SD of Second Group" = "SMD1",           
      "SMD1H - SMD1 with Heteroscedastic Variances" = "SMD1H",  
      "ROM - Ratio of Means (log scale)" = "ROM"
    ))
  } else if (data_type == "dichotomous") {
    return(c(
      "OR - Odds Ratio (log scale)" = "OR",
      "RR - Risk Ratio (log scale)" = "RR",
      "RD - Risk Difference" = "RD",
      "PETO - Peto's Odds Ratio (log scale)" = "PETO",
      "AS - Arcsine Square Root Transformed Risk Difference" = "AS"
    ))
  } else if (data_type == "correlation") {
    return(c(
      "ZCOR - Fisher's Z Transformation" = "ZCOR",
      "COR - Raw Correlation Coefficient" = "COR"
    ))
  } else {
    return(c("Unknown data type" = ""))
  }
}

# Helper: Get subgroup column choices
get_subgroup_choices <- function(data) {
  if (is.null(data)) return(character(0))
  
  # Find categorical/character columns (excluding 'study')
  potential_subgroups <- names(data)[sapply(data, function(x) {
    (is.character(x) || is.factor(x)) && length(unique(x)) > 1 && length(unique(x)) < nrow(data)
  })]
  
  # Remove 'study' column
  potential_subgroups <- setdiff(tolower(potential_subgroups), "study")
  
  # Return with proper names
  cols <- names(data)[tolower(names(data)) %in% potential_subgroups]
  setNames(cols, paste0(cols, " (", sapply(data[cols], function(x) length(unique(x))), " groups)"))
}

# Helper: Format data type name
#' Format Data Type Name for Display
#' @param data_type Character: data type identifier
#' @return Formatted display string (always returns character, never errors)
format_data_type_name <- function(data_type) {
  # ========================================
  # DEFENSIVE PROGRAMMING: Handle ALL edge cases
  # ========================================
  
  # Handle NULL
  if (is.null(data_type)) {
    return("Unknown Data Type")
  }
  
  # Handle empty vector
  if (length(data_type) == 0) {
    return("Unknown Data Type")
  }
  
  # Handle vector input - take first element only
  if (length(data_type) > 1) {
    warning("format_data_type_name received vector of length ", length(data_type), 
            ". Using first element only.")
    data_type <- data_type[1]
  }
  
  # Convert to character and handle NA
  data_type <- as.character(data_type)
  if (is.na(data_type) || nchar(trimws(data_type)) == 0) {
    return("Unknown Data Type")
  }
  
  # Normalize: lowercase and trim whitespace
  data_type <- tolower(trimws(data_type))
  
  # Use switch with proper fallback
  result <- switch(
    data_type,
    "continuous" = "Continuous Outcomes (Two-group comparisons)",
    "dichotomous" = "Dichotomous Outcomes (Binary events)",
    "correlation" = "Correlation Coefficients",
    "Unknown Data Type"  # Default fallback
  )
  
  return(result)
}

