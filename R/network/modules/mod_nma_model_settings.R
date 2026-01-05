# =============================================================================
# Network Meta-Analysis Model Settings Module
# =============================================================================

nmaModelSettingsUI <- function(id) {
  ns <- NS(id)
  # Modal-based, no UI needed
  NULL
}

nmaModelSettingsServer <- function(id, current_data = reactive(NULL), data_type = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive to store current settings
    settings <- reactiveVal(list(
      effect_measure = "MD",
      reference_group = NULL,
      model_type = "random",
      method_tau = "REML"
    ))
    
    # Show modal when triggered
    show_settings_modal <- function() {
      current_settings <- isolate(settings())
      current_dtype <- isolate(data_type())
      current_dat <- isolate(current_data())
      
      # Get available measures based on data type
      measure_choices <- get_nma_measure_choices(current_dtype)
      
      # Get treatments from data for reference selection
      treatments <- get_treatments(current_dat)
      
      showModal(
        modalDialog(
          title = tagList(
            icon("sliders", style = "color: #457b9d;"),
            " Network Meta-Analysis Model Settings"
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
                strong(format_nma_data_type_name(data_type())),
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
                selected = current_settings$effect_measure
              ),
              tags$small(
                "Select the effect size metric for your network meta-analysis",
                style = "color: #6c757d;"
              )
            ),
            
            # Reference Treatment Selection (NEW for NMA)
            div(
              style = "margin-bottom: 20px;",
              h5("Reference Treatment", style = "color: #1d3557; margin-bottom: 10px;"),
              selectInput(
                session$ns("reference"),
                NULL,
                choices = if (!is.null(treatments)) {
                  setNames(treatments, treatments)
                } else {
                  c("Load data first..." = "")
                },
                selected = current_settings$reference_group
              ),
              tags$small(
                "Select the reference treatment for comparison (e.g., Placebo, Control)",
                style = "color: #6c757d;"
              )
            ),
            
            # Model Type Selection (NEW for NMA)
            div(
              style = "margin-bottom: 20px;",
              h5("Model Type", style = "color: #1d3557; margin-bottom: 10px;"),
              selectInput(
                session$ns("model_type"),
                NULL,
                choices = c(
                  "Random Effects" = "random",
                  "Common Effects (Fixed)" = "common"
                ),
                selected = current_settings$model_type
              ),
              tags$small(
                "Random effects recommended for most networks",
                style = "color: #6c757d;"
              )
            ),
            
            # Method Tau Selection
            div(
              style = "margin-bottom: 20px;",
              h5("Heterogeneity Estimation Method", style = "color: #1d3557; margin-bottom: 10px;"),
              selectInput(
                session$ns("method_tau"),
                NULL,
                choices = c(
                  "REML (Restricted Maximum Likelihood)" = "REML",
                  "ML (Maximum Likelihood)" = "ML",
                  "DL (DerSimonian-Laird)" = "DL",
                  "HS (Hunter-Schmidt)" = "HS",
                  "PM (Paule-Mandel)" = "PM"
                ),
                selected = current_settings$method_tau
              ),
              tags$small(
                "REML is recommended for most network meta-analyses",
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
    
    # Save settings observer
    observeEvent(input$save_settings, {
      req(input$measure, input$reference, input$model_type, input$method_tau)
      
      # Validate reference group selection
      if (is.null(input$reference) || input$reference == "") {
        showNotification(
          "Please select a reference treatment",
          type = "warning",
          duration = 3
        )
        return()
      }
      
      # Update settings
      new_settings <- list(
        effect_measure = input$measure,
        reference_group = input$reference,
        model_type = input$model_type,
        method_tau = input$method_tau
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
get_nma_measure_choices <- function(data_type) {
  if (is.null(data_type)) {
    return(c("Please load data first" = ""))
  }
  
  if (data_type == "continuous") {
    return(c(
      "MD - Mean Difference" = "MD",
      "SMD - Standardized Mean Difference" = "SMD"
    ))
  } else if (data_type == "dichotomous") {
    return(c(
      "OR - Odds Ratio (log scale)" = "OR",
      "RR - Risk Ratio (log scale)" = "RR",
      "RD - Risk Difference" = "RD"
    ))
  } else {
    return(c("Unknown data type" = ""))
  }
}

#' Format NMA Data Type Name for Display
#' @param data_type Character: data type identifier
#' @return Formatted display string
format_nma_data_type_name <- function(data_type) {
  # Handle NULL
  if (is.null(data_type)) {
    return("Unknown Data Type")
  }
  
  # Handle empty vector
  if (length(data_type) == 0) {
    return("Unknown Data Type")
  }
  
  # Handle vector input - take first element
  if (length(data_type) > 1) {
    warning("format_nma_data_type_name received vector of length ", length(data_type), 
            ". Using first element only.")
    data_type <- data_type[1]
  }
  
  # Convert and validate
  data_type <- as.character(data_type)
  if (is.na(data_type) || nchar(trimws(data_type)) == 0) {
    return("Unknown Data Type")
  }
  
  # Normalize and switch
  data_type <- tolower(trimws(data_type))
  
  switch(
    data_type,
    "continuous" = "Continuous Outcomes (Network format)",
    "dichotomous" = "Dichotomous Outcomes (Network format)",
    "Unknown Data Type"  # Default
  )
}

