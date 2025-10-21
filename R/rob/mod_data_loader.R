# =============================================================================
# Module: Data Loader (Support CSV and XLSX)
# =============================================================================

#' Data Loader Module UI
#' @param id Module namespace ID
dataLoaderUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h5("ROB Datasets", style = "color: #1d3557; font-weight: 700;"),
    
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
          "QUADAS-2" = "quadas",
          "QUIPS" = "quips",
          "ROB 2" = "rob2",
          "ROBINS-E" = "robins_e",
          "ROBINS-I" = "robins_i"
        ),
        selected = "quadas"
      )
    )
  )
}

#' Data Loader Module Server
#' @param id Module namespace ID
#' @param rob_folder Path to folder containing ROB datasets
dataLoaderServer <- function(id, rob_folder = "data/rob") {
  moduleServer(id, function(input, output, session) {
    
    # Available datasets
    datasets <- list(
      quadas = list(
        name = "QUADAS-2",
        file = "quadas",  # Without extension
        description = "Quality Assessment of Diagnostic Accuracy Studies",
        categories = 4
      ),
      quips = list(
        name = "QUIPS",
        file = "quips",
        description = "Quality in Prognostic Studies",
        categories = 4
      ),
      rob2 = list(
        name = "ROB 2",
        file = "rob2",
        description = "Cochrane Risk of Bias tool for Randomized Trials (version 2)",
        categories = 4
      ),
      robins_e = list(
        name = "ROBINS-E",
        file = "robins_e",
        description = "Risk Of Bias In Non-randomized Studies - of Exposures",
        categories = 5
      ),
      robins_i = list(
        name = "ROBINS-I",
        file = "robins_i",
        description = "Risk Of Bias In Non-randomized Studies - of Interventions",
        categories = 5
      )
    )
    
    # Helper function to find file with any extension
    find_data_file <- function(base_name, folder) {
      # Try different extensions
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
    output$dataset_info <- renderUI({
      info <- current_dataset_info()
      
      div(
        style = "background: #E8F4F8; padding: 12px; border-radius: 6px; 
                 border-left: 4px solid #457b9d;",
        div(
          style = "font-weight: 700; color: #1d3557; margin-bottom: 5px;",
          paste("Total:", length(datasets), "datasets")
        ),
        tags$ul(
          style = "margin: 0; padding-left: 20px; font-size: 12px; color: #5A6169;",
          tags$li("quadas"),
          tags$li("quips"),
          tags$li("rob2"),
          tags$li("robins_e"),
          tags$li("robins_i")
        )
      )
    })
    
    # Load selected dataset
    current_data <- reactive({
      req(input$selected_dataset)
      
      dataset_info <- datasets[[input$selected_dataset]]
      
      # Find file with any extension
      file_info <- find_data_file(dataset_info$file, rob_folder)
      
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
        dataset_info = current_dataset_info()
      )
    }))
  })
}
