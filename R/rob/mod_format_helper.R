# =============================================================================
# Module: Format Helper Modal (Updated with Comprehensive Instructions)
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
        # Default comprehensive content
        rob_format_content()
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
            style = "font-size: 14px; line-height: 1.6; max-height: 70vh; overflow-y: auto;",
            content
          ),
          
          # Footer with close button
          footer = tagList(
            modalButton("Close", icon = icon("times"))
          ),
          
          size = "xl", # Extra large modal for comprehensive content
          easyClose = TRUE,
          fade = TRUE
        )
      )
    })
  })
}

#' Comprehensive ROB Dataset Format Content
#' @return HTML content for modal with all ROB tool specifications

default_format_content <- function() {
  tagList(
    # Introduction Banner
    div(
      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
               padding: 20px; border-radius: 8px; color: white; margin-bottom: 25px;
               box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
      h4(
        icon("file-import"), 
        " Risk of Bias Data Import Instructions",
        style = "margin: 0 0 10px 0; font-weight: 700;"
      ),
      p(
        "Welcome! Follow these instructions to ensure your Risk of Bias (RoB) dataset is correctly formatted for visualization.",
        style = "margin: 0; font-size: 15px; opacity: 0.95;"
      )
    ),
    
    # Supported Tools Info Box
    div(
      style = "background: #E8F4F8; padding: 18px; border-radius: 6px;
               border-left: 4px solid #457b9d; margin-bottom: 25px;",
      h5(
        icon("check-circle", style = "color: #457b9d;"),
        " Supported ROB Tools",
        style = "color: #1d3557; margin-top: 0; font-weight: 700;"
      ),
      tags$ul(
        style = "margin: 10px 0 0 0; padding-left: 25px; line-height: 1.8;",
        tags$li(strong("RoB 2.0"), " - Risk of Bias tool for Randomized Trials"),
        tags$li(strong("ROBINS-I"), " - Non-Randomized Studies of Interventions"),
        tags$li(strong("ROBINS-E"), " - Non-Randomized Studies of Exposures"),
        tags$li(strong("QUADAS-2"), " - Diagnostic Accuracy Studies"),
        tags$li(strong("QUIPS"), " - Prognosis Studies")
      )
    ),
    
    # Mandatory Rules Section
    h5(
      icon("exclamation-triangle", style = "color: #F39C12;"),
      " Mandatory Data Format Rules",
      style = "color: #1d3557; margin-top: 25px; font-weight: 700;"
    ),
    
    div(
      style = "background: white; padding: 18px; border-radius: 6px; 
               border: 1px solid #dee2e6; margin-bottom: 25px;",
      tags$ol(
        style = "margin: 0; padding-left: 25px; line-height: 2;",
        tags$li(
          strong("File Format:"), 
          " Dataset must be in ", 
          tags$code(".csv", style = "background: #f8f9fa; padding: 2px 6px; border-radius: 3px; color: #e83e8c;"), 
          ", ", 
          tags$code(".xlsx", style = "background: #f8f9fa; padding: 2px 6px; border-radius: 3px; color: #e83e8c;")
        ),
        tags$li(
          strong("Column Names:"), 
          " Headers must match the specified names exactly (case-sensitive: ",
          tags$code("Study", style = "background: #f8f9fa; padding: 2px 6px; border-radius: 3px; color: #e83e8c;"),
          " not ",
          tags$code("study", style = "background: #f8f9fa; padding: 2px 6px; border-radius: 3px; color: #dc3545;"),
          ")."
        ),
        tags$li(
          strong("Judgment Values:"), 
          " Domain cells must use recognized categorical judgments (see tool-specific tables below)."
        ),
        tags$li(
          strong("No Missing Data:"), 
          " Ensure all cells are filled with valid values."
        )
      )
    ),
    
    # Weight Column Note
    div(
      style = "background: #FFF3CD; padding: 15px; border-radius: 6px;
               border-left: 4px solid #F39C12; margin-bottom: 30px;",
      p(
        icon("info-circle", style = "color: #F39C12;"),
        strong(" Note on Weight Column:"),
        style = "margin: 0 0 8px 0; color: #856404;"
      ),
      p(
        "The ", 
        tags$code("Weight", style = "background: #fff; padding: 2px 6px; border-radius: 3px;"),
        " column is part of the dataset structure but is ",
        strong("currently NOT implemented"), 
        " in this version. Support for utilizing study weights will be added in future updates.",
        style = "margin: 0; font-size: 13px; color: #856404;"
      )
    ),
    
    # Tool-Specific Structures
    h4(
      icon("table"),
      " Tool-Specific Data Structures",
      style = "color: #1d3557; margin-top: 30px; margin-bottom: 20px; 
               font-weight: 700; border-bottom: 2px solid #1d3557; padding-bottom: 10px;"
    ),
    
    # RoB 2.0
    create_tool_section(
      tool_name = "1. RoB 2.0 (Randomized Trials)",
      columns = "8 Columns Required",
      color = "#667eea",
      table_data = list(
        list("Study", "Study identifier (e.g., Author 2020)", "Any text"),
        list("D1", "Bias arising from randomization process", "Low, Some Concerns, High, No information"),
        list("D2", "Bias due to deviations from intended interventions", "Low, Some Concerns, High, No information"),
        list("D3", "Bias due to missing outcome data", "Low, Some Concerns, High, No information"),
        list("D4", "Bias in measurement of the outcome", "Low, Some Concerns, High, No information"),
        list("D5", "Bias in selection of the reported results", "Low, Some Concerns, High, No information"),
        list("Overall", "Overall risk-of-bias judgment", "Low, Some Concerns, High, No information"),
        list("Weight", "Study weight/precision (e.g., sample size)", "Any number (ignored)")
      )
    ),
    
    # ROBINS-I
    create_tool_section(
      tool_name = "2. ROBINS-I (Non-Randomized Interventions)",
      columns = "10 Columns Required",
      color = "#764ba2",
      table_data = list(
        list("Study", "Study identifier", "Any text"),
        list("D1", "Bias due to confounding", "Low, Moderate, Serious, Critical, No information"),
        list("D2", "Bias due to selection of participants", "Low, Moderate, Serious, Critical, No information"),
        list("D3", "Bias in classification of interventions", "Low, Moderate, Serious, Critical, No information"),
        list("D4", "Bias due to deviations from intended interventions", "Low, Moderate, Serious, Critical, No information"),
        list("D5", "Bias due to missing data", "Low, Moderate, Serious, Critical, No information"),
        list("D6", "Bias in measurement of outcomes", "Low, Moderate, Serious, Critical, No information"),
        list("D7", "Bias in selection of the reported result", "Low, Moderate, Serious, Critical, No information"),
        list("Overall", "Overall risk-of-bias judgment", "Low, Moderate, Serious, Critical, No information"),
        list("Weight", "Study weight/precision", "Any number (ignored)")
      )
    ),
    
    # ROBINS-E
    create_tool_section(
      tool_name = "3. ROBINS-E (Non-Randomized Exposures)",
      columns = "10 Columns Required",
      color = "#dda15e",
      table_data = list(
        list("Study", "Study identifier", "Any text"),
        list("D1", "Bias due to confounding", "Low, Some Concerns, High, Very High, No information"),
        list("D2", "Bias arising from measurement of the exposure", "Low, Some Concerns, High, Very High, No information"),
        list("D3", "Bias in selection of participants", "Low, Some Concerns, High, Very High, No information"),
        list("D4", "Bias due to post-exposure interventions", "Low, Some Concerns, High, Very High, No information"),
        list("D5", "Bias due to missing data", "Low, Some Concerns, High, Very High, No information"),
        list("D6", "Bias arising from measurement of the outcome", "Low, Some Concerns, High, Very High, No information"),
        list("D7", "Bias in selection of the reported result", "Low, Some Concerns, High, Very High, No information"),
        list("Overall", "Overall risk-of-bias judgment", "Low, Some Concerns, High, Very High, No information"),
        list("Weight", "Study weight/precision", "Any number (ignored)")
      )
    ),
    
    # QUADAS-2
    create_tool_section(
      tool_name = "4. QUADAS-2 (Diagnostic Accuracy)",
      columns = "6 Columns Required",
      color = "#219ebc",
      table_data = list(
        list("Study", "Study identifier", "Any text"),
        list("D1", "Patient selection", "Low, High, Unclear"),
        list("D2", "Index test", "Low, High, Unclear"),
        list("D3", "Reference standard", "Low, High, Unclear"),
        list("D4", "Flow & timing", "Low, High, Unclear"),
        list("Overall", "Overall risk-of-bias judgment", "Low, High, Unclear")
      )
    ),
    
    # QUIPS
    create_tool_section(
      tool_name = "5. QUIPS (Prognosis Studies)",
      columns = "9 Columns Required",
      color = "#005f73",
      table_data = list(
        list("Study", "Study identifier", "Any text"),
        list("D1", "Bias due to participation", "Low, Moderate, High, No information"),
        list("D2", "Bias due to attrition", "Low, Moderate, High, No information"),
        list("D3", "Bias due to prognostic factor measurement", "Low, Moderate, High, No information"),
        list("D4", "Bias due to outcome measurement", "Low, Moderate, High, No information"),
        list("D5", "Bias due to confounding", "Low, Moderate, High, No information"),
        list("D6", "Bias in statistical analysis and reporting", "Low, Moderate, High, No information"),
        list("Overall", "Overall risk-of-bias judgment", "Low, Moderate, High, No information"),
        list("Weight", "Study weight/precision", "Any number (ignored)")
      )
    ),
    
    # Additional Resources
    div(
      style = "background: #f8f9fa; padding: 20px; border-radius: 8px; margin-top: 30px;
               border: 1px solid #dee2e6;",
      h5(
        icon("book"),
        " Additional Resources",
        style = "color: #1d3557; margin-top: 0; font-weight: 700;"
      ),
      p(
        "For more information about Risk of Bias assessment tools, visit:",
        style = "margin-bottom: 10px;"
      ),
      tags$a(
        href = "https://www.riskofbias.info/welcome",
        target = "_blank",
        icon("external-link-alt"),
        " Risk of Bias Info (ROBVIS Documentation)",
        style = "color: #667eea; font-weight: 600; text-decoration: none;"
      )
    )
  )
}

#' Helper function to create consistent tool section
#' @param tool_name Name of the ROB tool
#' @param columns Number of columns required
#' @param color Theme color for the section
#' @param table_data List of rows (each row is a list with 3 elements)

create_tool_section <- function(tool_name, columns, color, table_data) {
  tagList(
    # Tool Header
    div(
      style = paste0("background: ", color, "; padding: 12px 18px; border-radius: 6px;
                      margin-top: 20px; margin-bottom: 15px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);"),
      h5(
        tool_name,
        style = "color: white; margin: 0; font-weight: 700; font-size: 16px;"
      ),
      div(
        style = "color: rgba(255,255,255,0.9); font-size: 13px; margin-top: 5px;",
        icon("columns"),
        " ",
        columns
      )
    ),
    
    # Data Table
    div(
      style = "overflow-x: auto; margin-bottom: 25px;",
      tags$table(
        class = "table table-sm table-bordered table-hover",
        style = "font-size: 13px; background: white; margin-bottom: 0;",
        
        # Header
        tags$thead(
          style = "background: #1d3557; color: white;",
          tags$tr(
            tags$th("Column Name", style = "padding: 12px; font-weight: 600;"),
            tags$th("Description", style = "padding: 12px; font-weight: 600;"),
            tags$th("Mandatory Values", style = "padding: 12px; font-weight: 600;")
          )
        ),
        
        # Body
        tags$tbody(
          lapply(table_data, function(row) {
            tags$tr(
              tags$td(
                tags$code(
                  row[[1]], 
                  style = "background: #f8f9fa; padding: 4px 8px; border-radius: 3px; 
                           color: #e83e8c; font-weight: 600; font-size: 12px;"
                ),
                style = "padding: 10px; vertical-align: middle;"
              ),
              tags$td(row[[2]], style = "padding: 10px; vertical-align: middle;"),
              tags$td(
                tags$code(
                  row[[3]], 
                  style = "background: #e7f3ff; padding: 4px 8px; border-radius: 3px;
                           color: #0366d6; font-size: 11px;"
                ),
                style = "padding: 10px; vertical-align: middle;"
              )
            )
          })
        )
      )
    )
  )
}
