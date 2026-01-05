# =============================================================================
# Module: Meta-Analysis Data Loader
# =============================================================================

#' Meta-Analysis Data Loader Module UI
#' @param id Module namespace ID
metaDataLoaderUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h5("Meta-Analysis Datasets", style = "color: #1d3557; font-weight: 700;"),
    
    # Dataset info display
    uiOutput(ns("dataset_info")),
    
    # Dataset selector
    div(
      style = "margin-top: 15px;",
      h6("Select Dataset:", style = "color: #1d3557; font-weight: 600; font-size: 13px;"),
      selectInput(
        ns("selected_dataset"),
        NULL,
        choices = c(
          "Continuous Data" = "continuous",
          "Dichotomous Data" = "dichotomous",
          "Correlation Data" = "correlation"
        ),
        selected = "continuous"
      )
    )
  )
}

#' Meta-Analysis Data Loader Module Server
#' @param id Module namespace ID
#' @param meta_folder Path to folder containing meta-analysis datasets
metaDataLoaderServer <- function(id, meta_folder = "data/meta") {
  moduleServer(id, function(input, output, session) {
    
    # Available datasets
    datasets <- list(
      continuous = list(
        name = "Continuous Outcomes",
        file = "meta_continuous",
        description = "Two-group continuous outcomes (means, SDs)",
        columns = "study, n.e, mean.e, sd.e, n.c, mean.c, sd.c, group",
        measures = "MD, SMD, SMDH"
      ),
      dichotomous = list(
        name = "Dichotomous Outcomes",
        file = "meta_dichotomous",
        description = "Binary outcomes (events and totals)",
        columns = "study, event.e, n.e, event.c, n.c, group",
        measures = "OR, RR, RD, PETO, AS"
      ),
      correlation = list(
        name = "Correlation Data",
        file = "meta_correlation",
        description = "Correlation coefficients",
        columns = "study, r, n, group",
        measures = "COR, ZCOR"
      )
    )
    
    # Helper function to find file with any extension
    find_data_file <- function(base_name, folder) {
      extensions <- c(".xlsx", ".xls", ".csv")
      for (ext in extensions) {
        file_path <- file.path(folder, paste0(base_name, ext))
        if (file.exists(file_path)) {
          return(list(path = file_path, ext = ext))
        }
      }
      return(NULL)
    }
    
    # Reactive dataset info
    current_dataset_info <- reactive({
      req(input$selected_dataset)
      datasets[[input$selected_dataset]]
    })
    
    # Display dataset information
    # output$dataset_info <- renderUI({
    #   info <- current_dataset_info()
    #   
    #   div(
    #     style = "background: #E8F4F8; padding: 12px; border-radius: 6px; 
    #              border-left: 4px solid #457b9d;  margin-bottom: 0px;",
    #     div(
    #       style = "font-weight: 700; color: #1d3557; margin-bottom: 5px;",
    #       paste("Total:", length(datasets), "datasets")
    #     ),
    #     tags$ul(
    #       style = "margin: 0; padding-left: 20px; font-size: 12px; color: #5A6169;",
    #       tags$li("Continuous"),
    #       tags$li("Dichotomous"),
    #       tags$li("Correlation")
    #     )
    #   )
    # })
    output$dataset_info <- renderUI({
      
      # datasets is already defined in the parent scope: c("continuous", "dichotomous", "correlation")
      
      div(
        style = "background: #E8F4F8; padding: 12px; border-radius: 6px; 
             border-left: 4px solid #457b9d; margin-bottom: 0px;",
        h6(
          icon("database", style = "color: #457b9d;"),
          " Available Datasets",
          style = "color: #1d3557; margin: 0 0 8px 0; font-weight: 600;"
        ),
        tags$small(
          strong("Total: ", length(datasets), " datasets"), br(),
          tags$ul(
            style = "margin: 5px 0 0 0; padding-left: 20px; color: #5a6169;",
            tags$li("Continuous"),
            tags$li("Dichotomous"),
            tags$li("Correlation")
          ),
          style = "color: #1d3557;"
        )
      )
    })
    
    
    # Load selected dataset
    current_data <- reactive({
      req(input$selected_dataset)
      dataset_info <- datasets[[input$selected_dataset]]
      
      # Find file with any extension
      file_info <- find_data_file(dataset_info$file, meta_folder)
      
      if (is.null(file_info)) {
        showNotification(
          paste("File not found:", dataset_info$file, "(tried .xlsx, .xls, .csv)"),
          type = "error",
          duration = 5
        )
        return(NULL)
      }
      
      # Load data based on file extension
      data <- tryCatch({
        if (file_info$ext %in% c(".xlsx", ".xls")) {
          readxl::read_excel(file_info$path)
        } else if (file_info$ext == ".csv") {
          read.csv(file_info$path, stringsAsFactors = FALSE)
        } else {
          stop("Unsupported file format")
        }
      }, error = function(e) {
        showNotification(
          paste("Error loading", dataset_info$name, ":", e$message),
          type = "error",
          duration = 5
        )
        return(NULL)
      })
      
      # Convert to data frame if tibble
      if (!is.null(data)) {
        data <- as.data.frame(data)
      }
      
      return(data)
    })
    
    # Return reactive values
    return(reactive({
      list(
        current_data = current_data(),
        dataset_name = current_dataset_info()$name,
        dataset_info = current_dataset_info(),
        data_type = input$selected_dataset
      )
    }))
  })
}
