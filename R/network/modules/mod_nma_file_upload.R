#===============================================================================
# MODULE: Network Meta-Analysis File Upload
#===============================================================================

nmaFileUploadUI <- function(id) {
  ns <- NS(id)
  
  div(
    style = "margin-bottom: 20px;",
    
    # Header with description
    div(
      style = "margin-bottom: 15px;",
      h5("Upload Custom Dataset",
         style = "color: #1d3557; font-weight: 700; margin-bottom: 10px;"),
      p("Upload your own network meta-analysis dataset (.xlsx or .csv)",
        style = "color: #7F8C8D; font-size: 13px; margin: 0;")
    ),
    
    # File input
    fileInput(
      ns("file"),
      NULL,
      accept = c(".xlsx", ".csv"),
      buttonLabel = "Browse...",
      placeholder = "No file selected"
    ),
    
    # ✅ ADD: Format Helper Link
    nmaFormatHelperUI(ns("format_help"), button_style = "link")
  )
}

#===============================================================================
# SERVER
#===============================================================================

nmaFileUploadServer <- function(id, reset_trigger = reactive(0)) {
  moduleServer(id, function(input, output, session) {
    
    # ✅ Initialize format helper module
    nmaFormatHelperServer("format_help")
    
    # Reactive values
    uploaded_data <- reactiveVal(NULL)
    
    # Reset file input when triggered
    observeEvent(reset_trigger(), {
      if (reset_trigger() > 0) {
        uploaded_data(NULL)
        shinyjs::reset("file")
      }
    })
    
    # Process uploaded file
    observeEvent(input$file, {
      req(input$file)
      
      tryCatch({
        # Read file based on extension
        if (grepl(".csv", input$file$name, ignore.case = TRUE)) {
          data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
        } else if (grepl(".xlsx", input$file$name, ignore.case = TRUE)) {
          data <- readxl::read_excel(input$file$datapath)
        } else {
          showNotification(
            "Unsupported file type. Please upload .xlsx or .csv",
            type = "error",
            duration = 5
          )
          return()
        }
        
        # Validate data
        validation <- validate_nma_data(data)
        if (!validation$valid) {
          show_nma_validation_error(validation$errors)
          uploaded_data(NULL)
          shinyjs::reset("file")
          return()
        }
        
        # ✅ FIXED: Store data with detected data_type (consistent with Meta module)
        uploaded_data(list(
          data = data,
          filename = input$file$name,
          data_type = validation$data_type
        ))
        
        # ✅ FIXED: Simple notification only (no green info box - consistent with Meta)
        showNotification(
          paste("File uploaded successfully:", input$file$name),
          type = "message",
          duration = 3
        )
        
      }, error = function(e) {
        showNotification(
          paste("Error reading file:", e$message),
          type = "error",
          duration = 5
        )
        uploaded_data(NULL)
      })
    })
    
    # Return uploaded data
    return(uploaded_data)
  })
}