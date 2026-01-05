# =============================================================================
# Risk of Bias Plot Controls Module (Modal - COMPLETE WITH ALL DATASETS)
# =============================================================================

library(colourpicker)

plotControlsUI <- function(id) {
  ns <- NS(id)
  tagList()
}

plotControlsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ==========================================================================
    # DEFAULT COLORS FOR ALL DATASET TYPES
    # ==========================================================================
    
    # QUADAS-2 (3 categories)
    default_colors_quadas <- list(
      low = "#7cb5bd",
      high = "#d97676",
      unclear = "#F39C12"
    )
    
    # ROB 2, QUIPS (4 categories)
    default_colors_4 <- list(
      low = "#7cb5bd",
      some_concerns = "#f4e285",
      moderate = "#f4e285",  # QUIPS uses Moderate instead of Some Concerns
      high = "#d97676",
      no_info = "#c8b8d4"
    )
    
    # ROBINS-I (5 categories)
    default_colors_5_robins_i <- list(
      low = "#7cb5bd",
      moderate = "#f4e285",
      serious = "#e8a87c",
      critical = "#d97676",
      no_info = "#c8b8d4"
    )
    
    # ROBINS-E (5 categories)
    default_colors_5_robins_e <- list(
      low = "#7cb5bd",
      some_concerns = "#f4e285",
      high = "#d97676",
      very_high = "#b85454",
      no_info = "#c8b8d4"
    )
    
    default_settings <- list(
      # Colors
      color_low = default_colors_4$low,
      color_some_concerns = default_colors_4$some_concerns,
      color_moderate = default_colors_4$moderate,
      color_high = default_colors_4$high,
      color_unclear = default_colors_quadas$unclear,
      color_serious = default_colors_5_robins_i$serious,
      color_critical = default_colors_5_robins_i$critical,
      color_very_high = default_colors_5_robins_e$very_high,
      color_no_info = default_colors_4$no_info,
      
      # Settings
      circle_size = 9,
      font_size_traffic = 6,
      show_overall_traffic = TRUE,
      include_overall_summary = TRUE,
      font_size_summary = 13,
      plot_width = 10,
      plot_height = 6
    )
    
    risk_categories <- reactiveVal("4_category")  # Default
    settings <- reactiveVal(default_settings)
    generate_trigger <- reactiveVal(0)
    
    # ==========================================================================
    # SHOW MODAL FUNCTION
    # ==========================================================================
    
    show_modal <- function() {
      
      category_type <- risk_categories()
      current_settings <- settings()
      
      showModal(
        modalDialog(
          title = tagList(icon("palette"), " Plot Customization"),
          size = "xl",
          easyClose = TRUE,
          footer = tagList(
            actionButton(session$ns("reset_defaults"), "Reset to Defaults",
                         class = "btn-warning", icon = icon("undo")),
            modalButton("Cancel"),
            actionButton(session$ns("apply_custom"), "Apply",
                         class = "btn-primary", icon = icon("check"))
          ),
          
          bslib::navset_tab(
            id = session$ns("custom_tabs"),
            
            # Risk Colors Tab
            bslib::nav_panel(
              title = "Risk Colors",
              icon = icon("palette"),
              div(style = "padding: 20px;",
                  uiOutput(session$ns("color_controls_modal")))
            ),
            
            # Traffic Plot Tab
            bslib::nav_panel(
              title = "Traffic Plot",
              icon = icon("traffic-light"),
              div(
                style = "padding: 20px;",
                h5("Circle Settings", style = "color: #1d3557; margin-bottom: 15px; font-weight: 700;"),
                sliderInput(session$ns("circle_size"), "Circle Size",
                            min = 6, max = 15, value = current_settings$circle_size, step = 0.5),
                hr(),
                h5("Text Settings", style = "color: #1d3557; margin-bottom: 15px; font-weight: 700;"),
                sliderInput(session$ns("font_size_traffic"), "Font Size",
                            min = 4, max = 10, value = current_settings$font_size_traffic, step = 0.5),
                hr(),
                h5("Display Options", style = "color: #1d3557; margin-bottom: 15px; font-weight: 700;"),
                checkboxInput(session$ns("show_overall_traffic"), "Show Overall Column",
                              value = current_settings$show_overall_traffic)
              )
            ),
            
            # Summary Plot Tab
            bslib::nav_panel(
              title = "Summary Plot",
              icon = icon("chart-bar"),
              div(
                style = "padding: 20px;",
                h5("Text Settings", style = "color: #1d3557; margin-bottom: 15px; font-weight: 700;"),
                sliderInput(session$ns("font_size_summary"), "Font Size",
                            min = 10, max = 18, value = current_settings$font_size_summary, step = 1),
                hr(),
                h5("Display Options", style = "color: #1d3557; margin-bottom: 15px; font-weight: 700;"),
                checkboxInput(session$ns("include_overall_summary"), "Include Overall in Summary",
                              value = current_settings$include_overall_summary)
              )
            ),
            
            # Plot Dimensions Tab
            bslib::nav_panel(
              title = "Plot Dimensions",
              icon = icon("expand"),
              div(
                style = "padding: 20px;",
                h5("Export Dimensions", style = "color: #1d3557; margin-bottom: 15px; font-weight: 700;"),
                p(icon("info-circle"), " These dimensions apply when downloading plots as PNG or PDF",
                  style = "color: #6c757d; font-size: 13px; margin-bottom: 20px;"),
                div(class = "row",
                    div(class = "col-md-6",
                        numericInput(session$ns("plot_width"), "Width (inches)",
                                     value = current_settings$plot_width, min = 5, max = 20, step = 1)),
                    div(class = "col-md-6",
                        numericInput(session$ns("plot_height"), "Height (inches)",
                                     value = current_settings$plot_height, min = 4, max = 15, step = 1))
                )
              )
            )
          )
        )
      )
    }
    
    # ==========================================================================
    # DYNAMIC COLOR CONTROLS - REACTIVE TO DATASET TYPE
    # ==========================================================================
    
    output$color_controls_modal <- renderUI({
      
      category_type <- risk_categories()
      current_settings <- settings()
      
      # QUADAS-2: 3 categories
      if (category_type == "3_quadas") {
        tagList(
          h5("Risk Colors (QUADAS-2)", style = "color: #1d3557; margin-bottom: 15px; font-weight: 700;"),
          p("3-category scale: Low, High, Unclear", style = "color: #6c757d; font-size: 13px; margin-bottom: 20px;"),
          div(class = "row",
              div(class = "col-md-4",
                  colourpicker::colourInput(session$ns("color_low"), "Low Risk", value = current_settings$color_low)),
              div(class = "col-md-4",
                  colourpicker::colourInput(session$ns("color_high"), "High Risk", value = current_settings$color_high)),
              div(class = "col-md-4",
                  colourpicker::colourInput(session$ns("color_unclear"), "Unclear", value = current_settings$color_unclear))
          )
        )
        
        # QUIPS: 4 categories with Moderate
      } else if (category_type == "4_quips") {
        tagList(
          h5("Risk Colors (QUIPS)", style = "color: #1d3557; margin-bottom: 15px; font-weight: 700;"),
          p("4-category scale: Low, Moderate, High, No information", style = "color: #6c757d; font-size: 13px; margin-bottom: 20px;"),
          div(class = "row",
              div(class = "col-md-6",
                  colourpicker::colourInput(session$ns("color_low"), "Low Risk", value = current_settings$color_low)),
              div(class = "col-md-6",
                  colourpicker::colourInput(session$ns("color_moderate"), "Moderate Risk", value = current_settings$color_moderate))
          ),
          div(class = "row",
              div(class = "col-md-6",
                  colourpicker::colourInput(session$ns("color_high"), "High Risk", value = current_settings$color_high)),
              div(class = "col-md-6",
                  colourpicker::colourInput(session$ns("color_no_info"), "No Information", value = current_settings$color_no_info))
          )
        )
        
        # ROBINS-I: 5 categories
      } else if (category_type == "5_robins_i") {
        tagList(
          h5("Risk Colors (ROBINS-I)", style = "color: #1d3557; margin-bottom: 15px; font-weight: 700;"),
          p("5-category scale: Low, Moderate, Serious, Critical, No information", style = "color: #6c757d; font-size: 13px; margin-bottom: 20px;"),
          div(class = "row",
              div(class = "col-md-6",
                  colourpicker::colourInput(session$ns("color_low"), "Low Risk", value = current_settings$color_low)),
              div(class = "col-md-6",
                  colourpicker::colourInput(session$ns("color_moderate"), "Moderate Risk", value = current_settings$color_moderate))
          ),
          div(class = "row",
              div(class = "col-md-6",
                  colourpicker::colourInput(session$ns("color_serious"), "Serious Risk", value = current_settings$color_serious)),
              div(class = "col-md-6",
                  colourpicker::colourInput(session$ns("color_critical"), "Critical Risk", value = current_settings$color_critical))
          ),
          div(class = "row",
              div(class = "col-md-6",
                  colourpicker::colourInput(session$ns("color_no_info"), "No Information", value = current_settings$color_no_info))
          )
        )
        
        # ROBINS-E: 5 categories
      } else if (category_type == "5_robins_e") {
        tagList(
          h5("Risk Colors (ROBINS-E)", style = "color: #1d3557; margin-bottom: 15px; font-weight: 700;"),
          p("5-category scale: Low, Some Concerns, High, Very High, No information", style = "color: #6c757d; font-size: 13px; margin-bottom: 20px;"),
          div(class = "row",
              div(class = "col-md-6",
                  colourpicker::colourInput(session$ns("color_low"), "Low Risk", value = current_settings$color_low)),
              div(class = "col-md-6",
                  colourpicker::colourInput(session$ns("color_some_concerns"), "Some Concerns", value = current_settings$color_some_concerns))
          ),
          div(class = "row",
              div(class = "col-md-6",
                  colourpicker::colourInput(session$ns("color_high"), "High Risk", value = current_settings$color_high)),
              div(class = "col-md-6",
                  colourpicker::colourInput(session$ns("color_very_high"), "Very High Risk", value = current_settings$color_very_high))
          ),
          div(class = "row",
              div(class = "col-md-6",
                  colourpicker::colourInput(session$ns("color_no_info"), "No Information", value = current_settings$color_no_info))
          )
        )
        
        # ROB 2: 4 categories (default)
      } else {
        tagList(
          h5("Risk Colors (ROB 2)", style = "color: #1d3557; margin-bottom: 15px; font-weight: 700;"),
          p("4-category scale: Low, Some Concerns, High, No information", style = "color: #6c757d; font-size: 13px; margin-bottom: 20px;"),
          div(class = "row",
              div(class = "col-md-6",
                  colourpicker::colourInput(session$ns("color_low"), "Low Risk", value = current_settings$color_low)),
              div(class = "col-md-6",
                  colourpicker::colourInput(session$ns("color_some_concerns"), "Some Concerns", value = current_settings$color_some_concerns))
          ),
          div(class = "row",
              div(class = "col-md-6",
                  colourpicker::colourInput(session$ns("color_high"), "High Risk", value = current_settings$color_high)),
              div(class = "col-md-6",
                  colourpicker::colourInput(session$ns("color_no_info"), "No Information", value = current_settings$color_no_info))
          )
        )
      }
    })
    
    # Apply & Reset observers (same as before)
    observeEvent(input$apply_custom, {
      new_settings <- list(
        color_low = if (!is.null(input$color_low)) input$color_low else default_settings$color_low,
        color_some_concerns = if (!is.null(input$color_some_concerns)) input$color_some_concerns else default_settings$color_some_concerns,
        color_moderate = if (!is.null(input$color_moderate)) input$color_moderate else default_settings$color_moderate,
        color_high = if (!is.null(input$color_high)) input$color_high else default_settings$color_high,
        color_unclear = if (!is.null(input$color_unclear)) input$color_unclear else default_settings$color_unclear,
        color_serious = if (!is.null(input$color_serious)) input$color_serious else default_settings$color_serious,
        color_critical = if (!is.null(input$color_critical)) input$color_critical else default_settings$color_critical,
        color_very_high = if (!is.null(input$color_very_high)) input$color_very_high else default_settings$color_very_high,
        color_no_info = if (!is.null(input$color_no_info)) input$color_no_info else default_settings$color_no_info,
        circle_size = input$circle_size,
        font_size_traffic = input$font_size_traffic,
        show_overall_traffic = input$show_overall_traffic,
        include_overall_summary = input$include_overall_summary,
        font_size_summary = input$font_size_summary,
        plot_width = input$plot_width,
        plot_height = input$plot_height
      )
      settings(new_settings)
      removeModal()
      showNotification("Plot customization applied!", type = "message", duration = 2)
    })
    
    observeEvent(input$reset_defaults, {
      settings(default_settings)
      colourpicker::updateColourInput(session, "color_low", value = default_settings$color_low)
      colourpicker::updateColourInput(session, "color_some_concerns", value = default_settings$color_some_concerns)
      colourpicker::updateColourInput(session, "color_moderate", value = default_settings$color_moderate)
      colourpicker::updateColourInput(session, "color_high", value = default_settings$color_high)
      colourpicker::updateColourInput(session, "color_unclear", value = default_settings$color_unclear)
      colourpicker::updateColourInput(session, "color_serious", value = default_settings$color_serious)
      colourpicker::updateColourInput(session, "color_critical", value = default_settings$color_critical)
      colourpicker::updateColourInput(session, "color_very_high", value = default_settings$color_very_high)
      colourpicker::updateColourInput(session, "color_no_info", value = default_settings$color_no_info)
      updateSliderInput(session, "circle_size", value = default_settings$circle_size)
      updateSliderInput(session, "font_size_traffic", value = default_settings$font_size_traffic)
      updateCheckboxInput(session, "show_overall_traffic", value = default_settings$show_overall_traffic)
      updateCheckboxInput(session, "include_overall_summary", value = default_settings$include_overall_summary)
      updateSliderInput(session, "font_size_summary", value = default_settings$font_size_summary)
      updateNumericInput(session, "plot_width", value = default_settings$plot_width)
      updateNumericInput(session, "plot_height", value = default_settings$plot_height)
      showNotification("Reset to default settings!", type = "warning", duration = 2)
    })
    
    set_category_type <- function(type) {
      risk_categories(type)
    }
    
    trigger_generation <- function() {
      generate_trigger(generate_trigger() + 1)
    }
    
    return(reactive({
      list(
        colors = list(
          low = settings()$color_low,
          some_concerns = settings()$color_some_concerns,
          moderate = settings()$color_moderate,
          high = settings()$color_high,
          unclear = settings()$color_unclear,
          serious = settings()$color_serious,
          critical = settings()$color_critical,
          very_high = settings()$color_very_high,
          no_info = settings()$color_no_info
        ),
        circle_size = settings()$circle_size,
        font_size_traffic = settings()$font_size_traffic,
        font_size_summary = settings()$font_size_summary,
        show_overall_traffic = settings()$show_overall_traffic,
        include_overall_summary = settings()$include_overall_summary,
        width = settings()$plot_width,
        height = settings()$plot_height,
        trigger = generate_trigger(),
        category_type = risk_categories(),
        set_type = set_category_type,
        trigger_generation = trigger_generation,
        show_modal = show_modal
      )
    }))
  })
}
