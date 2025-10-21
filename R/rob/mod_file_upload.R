# =============================================================================
# Module: File Upload (Updated with format helper)
# Handles custom CSV/XLSX file uploads
# =============================================================================

#' File Upload Module UI
#' @param id Module namespace ID
fileUploadUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h5("Upload Custom Data"),
    
    fileInput(
      ns("custom_file"),
      NULL,
      accept = c(".csv", ".xlsx", ".xls"),
      buttonLabel = "Browse...",
      placeholder = "No file selected"
    ),
    
    # Upload status
    textOutput(ns("upload_status")),
    
    # Format helper (modal tooltip)
    div(
      style = "margin-top: 10px; text-align: center;",
      formatHelperUI(
        ns("format_helper"),
        button_label = "View Format Requirements",
        button_icon = "circle-question",
        button_style = "link"
      )
    )
  )
}

#' File Upload Module Server
#' @param id Module namespace ID
fileUploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Initialize format helper modal
    formatHelperServer(
      "format_helper",
      modal_title = "Custom Dataset Format Requirements",
      modal_content = default_format_content()  # Uses default, you can customize later
    )
    
    # Reactive values
    uploaded_data <- reactiveVal(NULL)
    uploaded_name <- reactiveVal(NULL)
    
    # Handle file upload
    observeEvent(input$custom_file, {
      req(input$custom_file)
      
      file_path <- input$custom_file$datapath
      file_ext <- tools::file_ext(input$custom_file$name)
      file_name <- tools::file_path_sans_ext(input$custom_file$name)
      
      tryCatch({
        # Read file based on extension
        if (file_ext %in% c("xlsx", "xls")) {
          data <- readxl::read_excel(file_path)
        } else if (file_ext == "csv") {
          data <- read.csv(file_path, stringsAsFactors = FALSE)
        } else {
          stop("Unsupported file format")
        }
        
        uploaded_data(data)
        uploaded_name(file_name)
        
        showNotification(
          paste("Uploaded:", file_name),
          type = "message",
          duration = 3
        )
        
      }, error = function(e) {
        showNotification(
          paste("Upload error:", e$message),
          type = "error",
          duration = 5
        )
      })
    })
    
    # Upload status
    output$upload_status <- renderText({
      if (!is.null(uploaded_name())) {
        paste0("✓ ", uploaded_name(), " (", nrow(uploaded_data()), " rows)")
      } else {
        ""
      }
    })
    
    # Return reactive data
    return(reactive({
      list(
        data = uploaded_data(),
        name = uploaded_name()
      )
    }))
  })
}
