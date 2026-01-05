#===============================================================================
# MODULE: Network Meta-Analysis Data Loader
#===============================================================================

nmaDataLoaderUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = "margin-bottom: 20px;",
      
      # Dataset info display FIRST (above dropdown)
      uiOutput(ns("dataset_info")),
      
      # Dropdown label
      tags$label(
        "SELECT DATASET",
        style = "color: #1d3557; font-size: 13px; font-weight: 600; 
                 margin-top: 15px; margin-bottom: 8px; display: block;"
      ),
      
      # Dropdown SECOND (below info box)
      selectInput(
        ns("selected_dataset"),
        NULL,
        choices = c(
          "Select a dataset..." = "",
          "Continuous Data" = "NMA_continuous",
          "Dichotomous Data" = "NMA_dichotomous"
        ),
        selected = character(0),
        width = "100%"
      )
    )
  )
}

#===============================================================================
# SERVER
#===============================================================================

nmaDataLoaderServer <- function(id, nma_folder = "data/network",
                                reset_trigger = reactive(0)) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values
    rv <- reactiveValues(
      data = NULL,
      data_type = NULL,
      dataset_name = NULL
    )
    
    # ✅ Track if this is the initial load
    initial_load <- reactiveVal(TRUE)
    
    # ✅ NEW: Reset the flag when reset is triggered
    observeEvent(reset_trigger(), {
      if (reset_trigger() > 0) {
        initial_load(TRUE)  # Reset flag so no notification on auto-reload
      }
    })
    
    # =========================================================================
    # ✅ FIX: Auto-load default dataset on initialization AND after reset
    # =========================================================================
    observe({
      # This observer runs whenever rv$data becomes NULL
      if (is.null(rv$data)) {
        # Small delay to ensure UI is ready
        Sys.sleep(0.1)
        updateSelectInput(session, "selected_dataset", selected = "NMA_continuous")
      }
    })
    
    # =========================================================================
    # Dataset info display - always shown
    # =========================================================================
    output$dataset_info <- renderUI({
      datasets <- c("NMA_continuous", "NMA_dichotomous")
      
      # Check which datasets exist
      available_datasets <- character()
      for (ds in datasets) {
        filepath <- file.path(nma_folder, paste0(ds, ".xlsx"))
        if (file.exists(filepath)) {
          available_datasets <- c(available_datasets, ds)
        }
      }
      
      n_datasets <- length(available_datasets)
      
      # Dataset type labels
      dataset_labels <- c(
        "NMA_continuous" = "Continuous",
        "NMA_dichotomous" = "Dichotomous"
      )
      
      div(
        style = "background: #E8F4F8; padding: 12px; border-radius: 6px; 
                 border-left: 4px solid #457b9d; margin-bottom: 0px;",
        h6(
          icon("database", style = "color: #457b9d;"),
          " Available Datasets",
          style = "color: #1d3557; margin: 0 0 8px 0; font-weight: 600;"
        ),
        tags$small(
          strong("Total: "), n_datasets, " datasets", br(),
          tags$ul(
            style = "margin: 5px 0 0 0; padding-left: 20px; color: #5a6169;",
            lapply(available_datasets, function(ds) {
              tags$li(dataset_labels[[ds]])
            })
          ),
          style = "color: #1d3557;"
        )
      )
    })
    
    # =========================================================================
    # Load data when selection changes
    # =========================================================================
    
    observeEvent(input$selected_dataset, {
      if (is.null(input$selected_dataset) || input$selected_dataset == "") {
        rv$data <- NULL
        rv$data_type <- NULL
        rv$dataset_name <- NULL
        return()
      }
      
      # Construct file path
      filepath <- file.path(nma_folder, paste0(input$selected_dataset, ".xlsx"))
      
      # Get friendly name
      friendly_name <- names(which(c(
        "Continuous Data" = "NMA_continuous",
        "Dichotomous Data" = "NMA_dichotomous"
      ) == input$selected_dataset))
      
      tryCatch({
        # Load data
        data <- readxl::read_excel(filepath)
        
        # Validate
        validation <- validate_nma_data(data)
        if (!validation$valid) {
          show_nma_validation_error(validation$errors)
          rv$data <- NULL
          rv$data_type <- NULL
          rv$dataset_name <- NULL
          updateSelectInput(session, "selected_dataset", selected = character(0))
          return()
        }
        
        # Store data
        rv$data <- data
        rv$data_type <- validation$data_type
        rv$dataset_name <- friendly_name
        
        # ✅ FIXED: Only show notification if NOT initial load
        if (!initial_load()) {
          showNotification(
            paste("Dataset loaded:", friendly_name),
            type = "message",
            duration = 3
          )
        } else {
          # After first load, set flag to FALSE
          initial_load(FALSE)
        }
        
      }, error = function(e) {
        showNotification(
          paste("Error loading dataset:", e$message),
          type = "error",
          duration = 5
        )
        rv$data <- NULL
        rv$data_type <- NULL
        rv$dataset_name <- NULL
      })
    })
    
    # Return reactive values
    return(rv)
  })
}
