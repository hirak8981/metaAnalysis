# =============================================================================
# Network Meta-Analysis Format Helper Module
# =============================================================================

nmaFormatHelperUI <- function(id, button_style = "link") {
  ns <- NS(id)
  
  if (button_style == "link") {
    actionLink(
      ns("show_guide"),
      label = tagList(icon("circle-info"), "Data Format Guide"),
      style = "color: #1d3557; font-weight: 600; font-size: 13px; text-decoration: none;"
    )
  } else {
    actionButton(
      ns("show_guide"),
      "Data Format Guide",
      icon = icon("circle-info"),
      class = "btn-sm btn-outline-info",
      style = "padding: 5px 10px; font-size: 13px;"
    )
  }
}


nmaFormatHelperServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$show_guide, {
      showModal(
        modalDialog(
          title = tagList(
            icon("circle-info", class = "fa-lg", style = "color: #1d3557;"),
            " ",
            "Network Meta-Analysis Data Format Guide"
          ),
          size = "xl",
          easyClose = TRUE,
          fade = TRUE,
          
          div(
            style = "font-size: 14px; line-height: 1.6; max-height: 70vh; overflow-y: auto;",
            
            # Introduction Banner
            div(
              style = "background: linear-gradient(135deg, #BCA951 0%, #D79730 100%);
                       padding: 20px; border-radius: 8px; color: white; margin-bottom: 25px;
                       box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
              h4(
                icon("diagram-project"),
                " Network Meta-Analysis Data Import Instructions",
                style = "margin: 0 0 10px 0; font-weight: 700;"
              ),
              p(
                "Network meta-analysis requires arm-based data with treatment comparisons within studies.",
                style = "margin: 0; font-size: 15px; opacity: 0.95;"
              )
            ),
            
            # Supported Data Types
            div(
              style = "background: #E8F4F8; padding: 18px; border-radius: 6px;
                       border-left: 4px solid #457b9d; margin-bottom: 25px;",
              h5(
                icon("check-circle", style = "color: #457b9d;"),
                " Supported Data Types",
                style = "color: #1d3557; margin-top: 0; font-weight: 700;"
              ),
              tags$ul(
                style = "margin: 10px 0 0 0; padding-left: 25px; line-height: 1.8;",
                tags$li(strong("Continuous Outcomes"), " - Mean, SD, sample size per arm"),
                tags$li(strong("Dichotomous Outcomes"), " - Events and total per arm")
              )
            ),
            
            # Mandatory Rules
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
                  " or ",
                  tags$code(".xlsx", style = "background: #f8f9fa; padding: 2px 6px; border-radius: 3px; color: #e83e8c;")
                ),
                tags$li(
                  strong("Required Columns:"),
                  " Must include ",
                  tags$code("study", style = "background: #f8f9fa; padding: 2px 6px; border-radius: 3px; color: #28a745;"),
                  " and ",
                  tags$code("treatment", style = "background: #f8f9fa; padding: 2px 6px; border-radius: 3px; color: #28a745;")
                ),
                tags$li(
                  strong("Multi-arm Trials:"),
                  " Each row represents one treatment arm"
                ),
                tags$li(
                  strong("No Missing Data:"),
                  " All required cells must be filled"
                )
              )
            ),
            
            # Data Type Specifications
            h4(
              icon("table"),
              " Data Type Specifications",
              style = "color: #1d3557; margin-top: 30px; margin-bottom: 20px;
                       font-weight: 700; border-bottom: 2px solid #1d3557; padding-bottom: 10px;"
            ),
            
            # 1. Continuous NMA Data
            create_nma_format_section(
              tool_name = "1. Continuous Outcomes (Network Format)",
              columns = "5-6 Columns Required",
              color = "#667eea",
              use_case = "Multi-arm trials comparing means across treatments",
              measures = "MD, SMD, ROM",
              table_data = list(
                list("study", "Study identifier", "Any text (e.g., 'Study_1')"),
                list("treatment", "Treatment name", "Any text (e.g., 'Placebo', 'Drug A')"),
                list("mean", "Mean outcome", "Any number"),
                list("sd", "Standard deviation", "Positive number"),
                list("n", "Sample size", "Positive integer"),
                list("treatment_class", "Treatment class (OPTIONAL)", "Categorical text")
              ),
              example = 'study    treatment  mean   sd    n    treatment_class
Study_1  Placebo    9.43   6.42  69   Control
Study_1  Drug A     19.00  4.16  67   Active
Study_2  Placebo    19.30  4.98  78   Control
Study_2  Drug A     13.70  5.05  113  Active'
            ),
            
            # 2. Dichotomous NMA Data
            create_nma_format_section(
              tool_name = "2. Dichotomous Outcomes (Network Format)",
              columns = "4-5 Columns Required",
              color = "#764ba2",
              use_case = "Multi-arm trials with binary outcomes",
              measures = "OR, RR, RD",
              table_data = list(
                list("study", "Study identifier", "Any text"),
                list("treatment", "Treatment name", "Any text"),
                list("events", "Number of events", "Non-negative integer"),
                list("n", "Total sample size", "Positive integer"),
                list("treatment_class", "Treatment class (OPTIONAL)", "Categorical text")
              ),
              example = 'study    treatment  events  n     treatment_class
Study_1  Control    1003    2100  Control
Study_1  Trt X      945     2050  Active
Study_2  Control    385     1250  Control
Study_2  Trt X      765     1500  Active'
              
            ),
            
            # Tips
            div(
              style = "background: #FFF3CD; padding: 15px; border-radius: 6px;
                       border-left: 4px solid #F39C12; margin-top: 30px;",
              h6(
                icon("lightbulb", style = "color: #856404;"),
                " Tips for Network Meta-Analysis Data",
                style = "color: #856404; margin: 0 0 10px 0; font-weight: 700;"
              ),
              tags$ul(
                style = "color: #856404; margin: 0; padding-left: 20px;",
                tags$li("Each row = one treatment arm"),
                tags$li("Studies can have 2+ arms (multi-arm trials)"),
                tags$li("Treatment names must be consistent across studies"),
                tags$li("Optional: Add treatment_class for colored network plots")
              )
            )
          ),
          
          footer = modalButton("Close", icon = icon("times"))
        )
      )
    })
  })
}


# Helper function for creating format sections
create_nma_format_section <- function(tool_name, columns, color, use_case, measures, table_data, example) {
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
        icon("columns"), " ", columns
      )
    ),
    
    # Use Case
    div(
      style = "background: #f8f9fa; padding: 12px; border-radius: 4px; margin-bottom: 15px;",
      p(
        strong("Use for: "), use_case,
        style = "margin: 0 0 8px 0; color: #5a6169;"
      ),
      p(
        strong("Available Measures: "),
        tags$code(measures, style = "background: #e7f3ff; padding: 2px 6px; border-radius: 3px; color: #0366d6;"),
        style = "margin: 0; color: #5a6169;"
      )
    ),
    
    # Data Table
    div(
      style = "overflow-x: auto; margin-bottom: 15px;",
      tags$table(
        class = "table table-sm table-bordered table-hover",
        style = "font-size: 13px; background: white; margin-bottom: 0;",
        
        # Header
        tags$thead(
          style = "background: #1d3557; color: white;",
          tags$tr(
            tags$th("Column Name", style = "padding: 12px; font-weight: 600;"),
            tags$th("Description", style = "padding: 12px; font-weight: 600;"),
            tags$th("Valid Values", style = "padding: 12px; font-weight: 600;")
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
    ),
    
    # Example Data
    h6("Example Data:", style = "color: #457b9d; margin-top: 15px; font-weight: 600;"),
    div(
      style = "background: #f8f9fa; padding: 10px; border-radius: 4px; overflow-x: auto; margin-bottom: 25px;",
      tags$pre(
        style = "margin: 0; font-size: 12px; color: #1d3557;",
        example
      )
    )
  )
}
