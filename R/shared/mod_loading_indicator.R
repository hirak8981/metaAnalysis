# =============================================================================
# Module: Loading Indicator (With more loader types)
# =============================================================================

#' Loading Indicator UI
#' @param id Module namespace ID
#' @param style Loading style: "spinner", "bar", "dots", "pulse", "dots-wave", "dual-ring", "orbit", "ripple"
#' @param message Loading message to display
loadingIndicatorUI <- function(id, style = "dots-wave", message = "Generating plots...") {
  ns <- NS(id)
  
  # Choose loader HTML based on style
  loader_html <- switch(style,
                        "spinner" = div(class = "loader-spinner"),
                        
                        "dots" = div(class = "loader-dots", 
                                     tags$span(), tags$span(), tags$span()),
                        
                        "dots-wave" = div(class = "loader-dots-wave",  # NEW - recommended!
                                          tags$span(), tags$span(), tags$span(), 
                                          tags$span(), tags$span()),
                        
                        "bar" = div(class = "loader-bar-container",
                                    div(class = "loader-bar")),
                        
                        "pulse" = div(class = "loader-pulse"),
                        
                        "dual-ring" = div(class = "loader-dual-ring"),  # NEW
                        
                        "orbit" = div(class = "loader-orbit",  # NEW
                                      tags$span(), tags$span(), tags$span(), tags$span()),
                        
                        "ripple" = div(class = "loader-ripple",  # NEW
                                       tags$span(), tags$span()),
                        
                        # Default to dots-wave
                        div(class = "loader-dots-wave",
                            tags$span(), tags$span(), tags$span(), 
                            tags$span(), tags$span())
  )
  
  # Return loading overlay
  div(
    id = ns("loading_overlay"),
    class = "loading-overlay",
    loader_html,
    div(class = "loading-message", message)
  )
}

#' Loading Indicator Server
#' @param id Module namespace ID
loadingIndicatorServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    show_loading <- function() {
      session$sendCustomMessage(
        type = "show_loading",
        message = list(id = session$ns("loading_overlay"))
      )
    }
    
    hide_loading <- function() {
      session$sendCustomMessage(
        type = "hide_loading",
        message = list(id = session$ns("loading_overlay"))
      )
    }
    
    return(list(
      show = show_loading,
      hide = hide_loading
    ))
  })
}
