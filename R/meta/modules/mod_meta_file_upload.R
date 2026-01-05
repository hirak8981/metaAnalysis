# =============================================================================
# Meta-Analysis File Upload Module (WITH EXTERNAL RESET)
# =============================================================================

metaFileUploadUI <- function(id) {
  ns <- NS(id)
  
  div(
    style = "margin-bottom: 20px;",
    div(
      style = "margin-bottom: 15px;",
      h5("Upload Custom Dataset",
         style = "color: #1d3557; font-weight: 700; margin-bottom: 10px;"),
      p("Upload your own meta-analysis dataset (.xlsx or .csv)",
        style = "color: #7F8C8D; font-size: 13px; margin: 0;")
    ),
    fileInput(
      ns("file"),
      NULL,
      accept = c(".xlsx", ".csv"),
      buttonLabel = "Browse...",
      placeholder = "No file selected"
    ),
    metaFormatHelperUI(ns("format_help"), button_style = "link")
  )
}

metaFileUploadServer <- function(id, reset_trigger = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    
    # Initialize format helper
    metaFormatHelperServer("format_help")
    
    # Store uploaded data
    uploaded_data <- reactiveVal(NULL)
    
    # Handle file upload
    observeEvent(input$file, {
      req(input$file)
      
      file_path <- input$file$datapath
      file_ext <- tools::file_ext(input$file$name)
      
      data <- tryCatch({
        if (file_ext == "csv") {
          read.csv(file_path, stringsAsFactors = FALSE)
        } else if (file_ext %in% c("xlsx", "xls")) {
          readxl::read_excel(file_path)
        } else {
          NULL
        }
      }, error = function(e) {
        showNotification(
          paste("Error reading file:", e$message),
          type = "error",
          duration = 5
        )
        NULL
      })
      
      if (!is.null(data)) {
        uploaded_data(data)
      }
    })
    
    # Listen for external reset trigger
    observeEvent(reset_trigger(), {
      req(reset_trigger())
      uploaded_data(NULL)
      shinyjs::reset("file")
    })
    
    # Return reactive with data
    return(reactive({
      list(
        data = uploaded_data()
      )
    }))
  })
}
