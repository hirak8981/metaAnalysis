# =============================================================================
# Module: Format Helper Modal
# Displays dataset format instructions via modal dialog
# =============================================================================

#' Format Helper UI
#' @param id Module namespace ID
#' @param button_label Label for the trigger button/link
#' @param button_icon Icon for the trigger button
#' @param button_style Style: "link" (default), "button", or "icon"
formatHelperUI <- function(id, 
                           button_label = "Data Format Guide", 
                           button_icon = "circle-info",
                           button_style = "link") {
  ns <- NS(id)
  
  # Different button styles
  if (button_style == "link") {
    # Text link with icon
    actionLink(
      ns("show_modal"),
      label = tagList(icon(button_icon), button_label),
      style = "color: #1d3557; font-weight: 600; font-size: 13px; text-decoration: none;"
    )
  } else if (button_style == "button") {
    # Small button
    actionButton(
      ns("show_modal"),
      label = button_label,
      icon = icon(button_icon),
      class = "btn-sm btn-outline-primary",
      style = "font-size: 12px;"
    )
  } else {
    # Icon only (compact)
    actionLink(
      ns("show_modal"),
      label = icon(button_icon, class = "fa-lg"),
      style = "color: #1d3557; margin-left: 8px;",
      title = "Click for data format instructions"
    )
  }
}

#' Format Helper Server
#' @param id Module namespace ID
#' @param modal_title Title for the modal dialog
#' @param modal_content Reactive or static HTML content for the modal body
formatHelperServer <- function(id, 
                               modal_title = "Dataset Format Instructions",
                               modal_content = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Show modal when button/link is clicked
    observeEvent(input$show_modal, {
      
      # Get content (can be reactive or static)
      content <- if (is.reactive(modal_content)) {
        modal_content()
      } else if (!is.null(modal_content)) {
        modal_content
      } else {
        # Default placeholder content
        default_format_content()
      }
      
      # Show the modal
      showModal(
        modalDialog(
          title = tagList(
            icon("circle-info", class = "fa-lg", style = "color: #1d3557;"),
            " ",
            modal_title
          ),
          
          # Modal body content
          div(
            style = "font-size: 14px; line-height: 1.6;",
            content
          ),
          
          # Footer with close button
          footer = tagList(
            modalButton("Close", icon = icon("times"))
          ),
          
          size = "l",  # Large modal
          easyClose = TRUE,
          fade = TRUE
        )
      )
    })
  })
}

#' Default format content (placeholder)
#' @return HTML content for modal
default_format_content <- function() {
  tagList(
    div(
      style = "background: #E8F4F8; padding: 15px; border-radius: 6px; 
               border-left: 4px solid #457b9d; margin-bottom: 20px;",
      icon("lightbulb", style = "color: #457b9d;"),
      strong(" Note:"),
      " Your custom dataset should follow the format below for proper analysis."
    ),
    
    h5("Required Format:", style = "color: #1d3557; margin-top: 20px;"),
    
    p("Your dataset should be in CSV or XLSX format with the following structure:"),
    
    tags$ul(
      style = "line-height: 1.8;",
      tags$li(strong("Column 1:"), " Study names or IDs"),
      tags$li(strong("Columns 2-N:"), " Domain assessments (Low, High, Some concerns)"),
      tags$li(strong("Last Column:"), " Overall assessment")
    ),
    
    h5("Example:", style = "color: #1d3557; margin-top: 20px;"),
    
    # Example table
    div(
      style = "overflow-x: auto; margin-top: 10px;",
      tags$table(
        class = "table table-sm table-bordered",
        style = "font-size: 12px; background: white;",
        tags$thead(
          style = "background: #1d3557; color: white;",
          tags$tr(
            tags$th("Study"),
            tags$th("D1"),
            tags$th("D2"),
            tags$th("D3"),
            tags$th("Overall")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Study 1"),
            tags$td("Low"),
            tags$td("Low"),
            tags$td("Low"),
            tags$td("Low")
          ),
          tags$tr(
            tags$td("Study 2"),
            tags$td("Some concerns"),
            tags$td("Low"),
            tags$td("High"),
            tags$td("High")
          )
        )
      )
    ),
    
    div(
      style = "background: #FFF3CD; padding: 12px; border-radius: 6px; 
               border-left: 4px solid #F39C12; margin-top: 20px;",
      icon("triangle-exclamation", style = "color: #F39C12;"),
      strong(" Important:"),
      " Ensure there are no empty cells or missing values in your dataset."
    )
  )
}
