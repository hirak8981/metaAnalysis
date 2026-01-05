# =============================================================================
# Meta-Analysis Format Helper Module
# =============================================================================

metaFormatHelperUI <- function(id, button_style = "link") {
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

metaFormatHelperServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$show_guide, {
      showModal(
        modalDialog(
          title = tagList(
            icon("circle-info", class = "fa-lg", style = "color: #1d3557;"),
            " ",
            "Meta-Analysis Data Format Guide"
          ),
          size = "xl",
          easyClose = TRUE,
          fade = TRUE,
          
          div(
            style = "font-size: 14px; line-height: 1.6; max-height: 70vh; overflow-y: auto;",
            
            # Introduction Banner with Gradient
            div(
              style = "background: linear-gradient(135deg, #20B2AA 0%, #29AB87 100%);
                       padding: 20px; border-radius: 8px; color: white; margin-bottom: 25px;
                       box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
              h4(
                icon("chart-line"),
                " Meta-Analysis Data Import Instructions",
                style = "margin: 0 0 10px 0; font-weight: 700;"
              ),
              p(
                "Welcome! Follow these instructions to ensure your meta-analysis dataset is correctly formatted.",
                style = "margin: 0; font-size: 15px; opacity: 0.95;"
              )
            ),
            
            # Supported Data Types Info Box
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
                tags$li(strong("Continuous Outcomes"), " - Two-group comparisons (means, SDs)"),
                tags$li(strong("Dichotomous Outcomes"), " - Binary events (2×2 tables)"),
                tags$li(strong("Correlation Coefficients"), " - Association measures")
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
                  " or ",
                  tags$code(".xlsx", style = "background: #f8f9fa; padding: 2px 6px; border-radius: 3px; color: #e83e8c;")
                ),
                tags$li(
                  strong("Column Names:"),
                  " Headers are case-insensitive (",
                  tags$code("study", style = "background: #f8f9fa; padding: 2px 6px; border-radius: 3px; color: #28a745;"),
                  " or ",
                  tags$code("Study", style = "background: #f8f9fa; padding: 2px 6px; border-radius: 3px; color: #28a745;"),
                  " both work)"
                ),
                tags$li(
                  strong("No Missing Data:"),
                  " All required cells must be filled with valid numeric values"
                ),
                tags$li(
                  strong("Study Column:"),
                  " Must contain unique study identifiers"
                )
              )
            ),
            
            # Data Type Sections with Color
            h4(
              icon("table"),
              " Data Type Specifications",
              style = "color: #1d3557; margin-top: 30px; margin-bottom: 20px;
                       font-weight: 700; border-bottom: 2px solid #1d3557; padding-bottom: 10px;"
            ),
            
            # 1. Continuous Data
            create_meta_tool_section(
              tool_name = "1. Continuous Outcomes (Two-Group Comparisons)",
              columns = "8 Columns Required",
              color = "#667eea",
              use_case = "Studies comparing means between two groups (e.g., weight loss, blood pressure)",
              measures = "MD, SMD, SMDH, SMD1, SMD1H, ROM",
              table_data = list(
                list("study", "Study identifier", "Any text (e.g., 'Smith 2020')"),
                list("n.e", "Sample size - experimental group", "Positive integer"),
                list("mean.e", "Mean - experimental group", "Any number"),
                list("sd.e", "Standard deviation - experimental group", "Positive number"),
                list("n.c", "Sample size - control group", "Positive integer"),
                list("mean.c", "Mean - control group", "Any number"),
                list("sd.c", "Standard deviation - control group", "Positive number"),
                list("group", "Subgroup variable (OPTIONAL)", "Categorical text")
              ),
              example = 'study       n.e  mean.e  sd.e  n.c  mean.c  sd.c  group
Study1       14    22.1   0.8   14    13.7  14.7   A
Study2       27    16.0   2.9   28     6.4   8.6   A
Study3       22     6.8   1.0   16     3.3   4.8   B'
            ),
            
            # 2. Dichotomous Data
            create_meta_tool_section(
              tool_name = "2. Dichotomous Outcomes (Binary Events)",
              columns = "6 Columns Required",
              color = "#764ba2",
              use_case = "Studies with binary outcomes (e.g., mortality, disease occurrence, treatment success)",
              measures = "OR, RR, RD, PETO, AS",
              table_data = list(
                list("study", "Study identifier", "Any text"),
                list("event.e", "Number of events - experimental group", "Non-negative integer"),
                list("n.e", "Total sample size - experimental group", "Positive integer"),
                list("event.c", "Number of events - control group", "Non-negative integer"),
                list("n.c", "Total sample size - control group", "Positive integer"),
                list("group", "Subgroup variable (OPTIONAL)", "Categorical text")
              ),
              example = 'study       event.e  n.e  event.c  n.c  group
study 1          16   35        5   32   B
study 2           5   52        1   50   B
study 3           5   50       17   45   A'
            ),
            
            # 3. Correlation Data
            create_meta_tool_section(
              tool_name = "3. Correlation Coefficients",
              columns = "4 Columns Required",
              color = "#dda15e",
              use_case = "Studies reporting correlation coefficients (e.g., test validity, associations)",
              measures = "ZCOR (recommended), COR",
              table_data = list(
                list("study", "Study identifier", "Any text"),
                list("r", "Correlation coefficient", "Number between -1 and 1"),
                list("n", "Sample size", "Positive integer (≥ 4)"),
                list("group", "Subgroup variable (OPTIONAL)", "Categorical text")
              ),
              example = 'study     r      n   group
S1      0.75    60    A
S2      0.65    75    A
S3      0.29    68    B'
            ),
            
            # Important Tips
            div(
              style = "background: #FFF3CD; padding: 15px; border-radius: 6px;
                       border-left: 4px solid #F39C12; margin-top: 30px;",
              h6(
                icon("lightbulb", style = "color: #856404;"),
                " Tips for Data Preparation",
                style = "color: #856404; margin: 0 0 10px 0; font-weight: 700;"
              ),
              tags$ul(
                style = "color: #856404; margin: 0; padding-left: 20px;",
                tags$li("Remove any empty rows or columns"),
                tags$li("Ensure numeric columns contain only numbers"),
                tags$li("Use consistent naming for subgroups"),
                tags$li("Check for typos in study names"),
                tags$li("Save Excel files as .xlsx (not .xls)")
              )
            ),
            
            # Additional Resources
            div(
              style = "background: #f8f9fa; padding: 20px; border-radius: 8px; margin-top: 25px;
                       border: 1px solid #dee2e6;",
              h5(
                icon("book"),
                " Additional Resources",
                style = "color: #1d3557; margin-top: 0; font-weight: 700;"
              ),
              p(
                "For more information about effect size calculation and meta-analysis methods:",
                style = "margin-bottom: 10px;"
              ),
              tags$a(
                href = "https://www.metafor-project.org/",
                target = "_blank",
                icon("external-link-alt"),
                " Metafor Package Documentation",
                style = "color: #667eea; font-weight: 600; text-decoration: none; display: block; margin-bottom: 5px;"
              )
            )
          ),
          
          footer = modalButton("Close", icon = icon("times"))
        )
      )
    })
  })
}

# Helper function to create consistent data type sections
create_meta_tool_section <- function(tool_name, columns, color, use_case, measures, table_data, example) {
  tagList(
    # Tool Header with Gradient
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
