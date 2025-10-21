# =============================================================================
# Module: Plot Display (Fixed loader hiding)
# =============================================================================

#' Plot Display UI
plotDisplayUI <- function(id, title) {
  ns <- NS(id)
  
  bslib::card(
    full_screen = TRUE,
    
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      style = "background: #1d3557 !important; color: #ffffff !important;",
      div(h5(title, style = "margin: 0; color: white!important;")),
      div(
        downloadButton(ns("download_png"), "PNG", class = "btn-sm", 
                       style = "margin-left: 5px; font-size: 11px;"),
        downloadButton(ns("download_jpg"), "JPG", class = "btn-sm", 
                       style = "margin-left: 5px; font-size: 11px;")
      )
    ),
    
    bslib::card_body(
      style = "position: relative; min-height: 500px;",
      
      # Plot output
      plotOutput(ns("plot"), height = "500px"),
      
      # Loading indicator overlay (positioned on top)
      loadingIndicatorUI(
        ns("loader"),
        style = "modern-circle",
        message = "Generating plot..."
      )
    )
  )
}

#' Plot Display Server (Fixed hiding logic)
plotDisplayServer <- function(id, plot_function, plot_data, plot_settings) {
  moduleServer(id, function(input, output, session) {
    
    # Initialize loading indicator
    loader <- loadingIndicatorServer("loader")
    
    # Store plot
    current_plot <- reactiveVal(NULL)
    
    # Show loader when data changes
    observeEvent(plot_data(), {
      loader$show()
    }, ignoreInit = TRUE)
    
    # Render plot and hide loader when done
    output$plot <- renderPlot({
      req(plot_data())
      req(plot_settings())
      
      # Show loader
      loader$show()
      
      # Small delay to ensure loader displays
      Sys.sleep(0.1)
      
      # Generate plot
      p <- plot_function(plot_data(), plot_settings())
      current_plot(p)
      
      # Minimum display time
      Sys.sleep(0.8)
      
      # Hide loader after plot is ready
      shinyjs::delay(200, loader$hide())
      
      p
    })
    
    # Downloads
    output$download_png <- downloadHandler(
      filename = function() {
        paste0(id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      },
      content = function(file) {
        req(current_plot())
        settings <- plot_settings()
        ggsave(file, current_plot(), device = "png", width = settings$width, 
               height = settings$height, dpi = 300, bg = "white")
      }
    )
    
    output$download_jpg <- downloadHandler(
      filename = function() {
        paste0(id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
      },
      content = function(file) {
        req(current_plot())
        settings <- plot_settings()
        ggsave(file, current_plot(), device = "jpeg", width = settings$width, 
               height = settings$height, dpi = 300, bg = "white", quality = 95)
      }
    )
  })
}
