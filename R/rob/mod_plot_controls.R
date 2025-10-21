# =============================================================================
# Module: Plot Controls (Fixed accordion)
# =============================================================================

library(colourpicker)

#' Plot Controls UI
plotControlsUI <- function(id) {
  ns <- NS(id)
  
  # Use bslib accordion for collapsible sections
  bslib::accordion(
    id = ns("customization_accordion"),
    open = FALSE,  # Collapsed by default
    multiple = FALSE,
    
    bslib::accordion_panel(
      value = "plot_custom",  # ADD THIS LINE - unique identifier
      title = tagList(icon("palette"), " Plot Customization"),
      
      # Auto-detect number of risk categories
      uiOutput(ns("color_controls")),
      
      hr(style = "border-color: #DEE2E6;"),
      
      # Traffic plot specific controls
      h6("Traffic Plot Settings", style = "color: #1d3557; font-weight: 600; font-size: 13px;"),
      
      sliderInput(
        ns("circle_size"),
        "Circle Size:",
        min = 6,
        max = 15,
        value = 9,
        step = 0.5
      ),
      
      sliderInput(
        ns("font_size_traffic"),
        "Font Size:",
        min = 4,
        max = 10,
        value = 6,
        step = 0.5
      ),
      
      checkboxInput(
        ns("show_overall_traffic"),
        "Show Overall Column",
        value = TRUE
      ),
      
      hr(style = "border-color: #DEE2E6;"),
      
      # Summary plot specific controls
      h6("Summary Plot Settings", style = "color: #1d3557; font-weight: 600; font-size: 13px;"),
      
      checkboxInput(
        ns("include_overall_summary"),
        "Include Overall in Summary",
        value = TRUE
      ),
      
      sliderInput(
        ns("font_size_summary"),
        "Font Size:",
        min = 10,
        max = 18,
        value = 13,
        step = 1
      ),
      
      hr(style = "border-color: #DEE2E6;"),
      
      # Plot dimensions
      h6("Plot Dimensions", style = "color: #1d3557; font-weight: 600; font-size: 13px;"),
      
      numericInput(
        ns("plot_width"),
        "Width (inches):",
        value = 10,
        min = 5,
        max = 20,
        step = 1
      ),
      
      numericInput(
        ns("plot_height"),
        "Height (inches):",
        value = 6,
        min = 4,
        max = 15,
        step = 1
      )
    )
  )
}

#' Plot Controls Server (unchanged)
plotControlsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Muted default colors
    default_colors_4 <- list(
      low = "#7cb5bd",
      some_concerns = "#f4e285",
      high = "#d97676",
      no_info = "#c8b8d4"
    )
    
    default_colors_5_robins_i <- list(
      low = "#7cb5bd",
      moderate = "#f4e285",
      serious = "#e8a87c",
      critical = "#d97676",
      no_info = "#c8b8d4"
    )
    
    default_colors_5_robins_e <- list(
      low = "#7cb5bd",
      some_concerns = "#f4e285",
      high = "#d97676",
      very_high = "#b85454",
      no_info = "#c8b8d4"
    )
    
    # Reactive category type
    risk_categories <- reactiveVal("4_category")
    
    # Dynamic color picker UI
    output$color_controls <- renderUI({
      ns <- session$ns
      category_type <- risk_categories()
      
      if (category_type == "5_robins_i") {
        tagList(
          h6("Risk Colors (ROBINS-I)", style = "color: #1d3557; font-weight: 600; font-size: 13px;"),
          colourpicker::colourInput(ns("color_low"), "Low Risk:", default_colors_5_robins_i$low),
          colourpicker::colourInput(ns("color_moderate"), "Moderate Risk:", default_colors_5_robins_i$moderate),
          colourpicker::colourInput(ns("color_serious"), "Serious Risk:", default_colors_5_robins_i$serious),
          colourpicker::colourInput(ns("color_critical"), "Critical Risk:", default_colors_5_robins_i$critical),
          colourpicker::colourInput(ns("color_no_info"), "No Information:", default_colors_5_robins_i$no_info)
        )
      } else if (category_type == "5_robins_e") {
        tagList(
          h6("Risk Colors (ROBINS-E)", style = "color: #1d3557; font-weight: 600; font-size: 13px;"),
          colourpicker::colourInput(ns("color_low"), "Low Risk:", default_colors_5_robins_e$low),
          colourpicker::colourInput(ns("color_some_concerns"), "Some Concerns:", default_colors_5_robins_e$some_concerns),
          colourpicker::colourInput(ns("color_high"), "High Risk:", default_colors_5_robins_e$high),
          colourpicker::colourInput(ns("color_very_high"), "Very High Risk:", default_colors_5_robins_e$very_high),
          colourpicker::colourInput(ns("color_no_info"), "No Information:", default_colors_5_robins_e$no_info)
        )
      } else {
        tagList(
          h6("Risk Colors", style = "color: #1d3557; font-weight: 600; font-size: 13px;"),
          colourpicker::colourInput(ns("color_low"), "Low Risk:", default_colors_4$low),
          colourpicker::colourInput(ns("color_some_concerns"), "Some Concerns:", default_colors_4$some_concerns),
          colourpicker::colourInput(ns("color_high"), "High Risk:", default_colors_4$high),
          colourpicker::colourInput(ns("color_no_info"), "No Information:", default_colors_4$no_info)
        )
      }
    })
    
    # Function to set category type
    set_category_type <- function(type) {
      risk_categories(type)
    }
    
    # Create a reactive trigger for external button
    generate_trigger <- reactiveVal(0)
    
    # Function to trigger generation from external button
    trigger_generation <- function() {
      generate_trigger(generate_trigger() + 1)
    }
    
    # Return reactive values
    return(reactive({
      list(
        colors = list(
          low = if (!is.null(input$color_low)) input$color_low else default_colors_4$low,
          some_concerns = if (!is.null(input$color_some_concerns)) input$color_some_concerns else default_colors_4$some_concerns,
          moderate = if (!is.null(input$color_moderate)) input$color_moderate else default_colors_5_robins_i$moderate,
          serious = if (!is.null(input$color_serious)) input$color_serious else default_colors_5_robins_i$serious,
          high = if (!is.null(input$color_high)) input$color_high else default_colors_4$high,
          critical = if (!is.null(input$color_critical)) input$color_critical else default_colors_5_robins_i$critical,
          very_high = if (!is.null(input$color_very_high)) input$color_very_high else default_colors_5_robins_e$very_high,
          no_info = if (!is.null(input$color_no_info)) input$color_no_info else default_colors_4$no_info
        ),
        circle_size = input$circle_size,
        font_size_traffic = input$font_size_traffic,
        font_size_summary = input$font_size_summary,
        show_overall_traffic = input$show_overall_traffic,
        include_overall_summary = input$include_overall_summary,
        width = input$plot_width,
        height = input$plot_height,
        trigger = generate_trigger(),
        category_type = risk_categories(),
        set_type = set_category_type,
        trigger_generation = trigger_generation
      )
    }))
  })
}
