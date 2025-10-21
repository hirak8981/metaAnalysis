# =============================================================================
# ROB Main Module - Complete ROB Functionality Wrapper
# =============================================================================

robMainUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Inline CSS for tabs (same as before)
    tags$style(HTML("
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        background: #ffffff !important;
        background-color: #ffffff !important;
        color: #333333 !important;
        border: 1px solid #dddddd !important;
      }
      
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background: #f9f9f9 !important;
        background-color: #f9f9f9 !important;
        color: #333333 !important;
        border: 1px solid #999999 !important;
      }
      
      .dataTables_wrapper .dataTables_paginate .paginate_button.current,
      .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
        background: #f0f0f0 !important;
        background-color: #f0f0f0 !important;
        color: #333333 !important;
        border: 1px solid #dddddd !important;
      }
      
      table.dataTable thead th {
        background-color: #1d3557 !important;
        color: #ffffff !important;
        font-weight: 600;
        text-align: center;
      }
    ")),
    
    # Main Layout
    bslib::layout_sidebar(
      
      sidebar = bslib::sidebar(
        width = 320,
        open = TRUE,
        bg = "#FAFBFC",
        
        dataLoaderUI(ns("data_loader")),
        tags$hr(style = "border-color: #DEE2E6; margin: 20px 0;"),
        
        fileUploadUI(ns("file_upload")),
        tags$hr(style = "border-color: #DEE2E6; margin: 20px 0;"),
        
        div(
          style = "margin-bottom: 20px;",
          actionButton(
            ns("generate_plots_main"),
            "Generate Plots",
            icon = icon("chart-bar"),
            class = "btn-primary btn-block",
            style = "font-weight: 600; font-size: 16px; padding: 12px;"
          )
        ),
        
        tags$hr(style = "border-color: #DEE2E6; margin: 20px 0;"),
        
        plotControlsUI(ns("plot_controls"))
      ),
      
      bslib::navset_card_tab(
        id = ns("rob_tabs"),
        
        bslib::nav_panel(
          title = "Data Preview",
          icon = icon("table"),
          value = "data_tab",
          
          div(
            style = "padding: 25px;",
            div(
              style = "margin-bottom: 25px;",
              h3("Dataset Preview", style = "color: #1d3557; margin-bottom: 10px;"),
              p("Displaying the first 5 rows", style = "color: #7F8C8D; margin: 0;")
            ),
            div(
              style = "background: white; padding: 20px; border-radius: 8px;",
              DTOutput(ns("data_preview"))
            )
          )
        ),
        
        bslib::nav_panel(
          title = "Visualizations",
          icon = icon("chart-bar"),
          value = "viz_tab",
          
          div(
            style = "padding: 25px;",
            div(
              style = "margin-bottom: 30px;",
              h3("Risk of Bias Visualizations", style = "color: #1d3557;")
            ),
            
            div(
              style = "margin-bottom: 40px;",
              plotDisplayUI(ns("plot1"), "Summary Plot")
            ),
            
            div(
              style = "margin-bottom: 40px;",
              plotDisplayUI(ns("plot2"), "Traffic Light Plot")
            )
          )
        )
      )
    )
  )
}

robMainServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Initialize sub-modules
    loaded_data <- dataLoaderServer("data_loader", rob_folder = "data/rob")
    uploaded_file <- fileUploadServer("file_upload")
    plot_controls <- plotControlsServer("plot_controls")
    
    # Connect main generate button to plot controls
    observeEvent(input$generate_plots_main, {
      bslib::nav_select("rob_tabs", selected = "viz_tab")
      plot_controls()$trigger_generation()
    })
    
    # Current data management
    current_data <- reactive({
      if (!is.null(uploaded_file()$data)) {
        uploaded_file()$data
      } else {
        loaded_data()$current_data
      }
    })
    
    # Get current tool name
    current_tool <- reactive({
      if (!is.null(uploaded_file()$data)) {
        return(NULL)
      } else {
        req(input$`data_loader-selected_dataset`)
        return(input$`data_loader-selected_dataset`)
      }
    })
    
    # Get domain names
    domain_names <- reactive({
      tool <- current_tool()
      if (is.null(tool)) {
        return(NULL)
      }
      get_domain_names(tool)
    })
    
    # Auto-detect tool type
    observe({
      req(current_data())
      df <- current_data()
      
      all_values <- df %>%
        select(-Study) %>%
        unlist() %>%
        unique() %>%
        na.omit() %>%
        tolower() %>%
        as.character()
      
      tool_type <- if ("moderate" %in% all_values && "serious" %in% all_values && "critical" %in% all_values) {
        "5_robins_i"
      } else if ("very high" %in% all_values || "very_high" %in% all_values) {
        "5_robins_e"
      } else {
        "4_category"
      }
      
      plot_controls()$set_type(tool_type)
    })
    
    # Data preview
    output$data_preview <- renderDT({
      req(current_data())
      
      datatable(
        head(current_data(), 5),
        options = list(
          pageLength = 5,
          lengthChange = FALSE,
          searching = TRUE,
          info = TRUE,
          scrollX = TRUE,
          paging = TRUE,
          ordering = TRUE,
          dom = 'ftip',
          pagingType = 'simple_numbers'
        ),
        filter = 'none',
        rownames = FALSE,
        class = "display compact stripe"
      )
    })
    
    # Plot data
    plot_data <- eventReactive(plot_controls()$trigger, {
      req(current_data())
      current_data()
    })
    
    plot_settings <- reactive({
      req(plot_controls())
      plot_controls()
    })
    
    # ROB configuration
    rob_config <- reactive({
      req(plot_data())
      req(plot_settings())
      
      df <- plot_data()
      settings <- plot_settings()
      category_type <- settings$category_type
      
      all_values <- df %>%
        select(-Study) %>%
        unlist() %>%
        unique() %>%
        na.omit() %>%
        as.character()
      
      if (category_type == "5_robins_i") {
        risk_labels <- c("Low", "Moderate", "Serious", "Critical", "No information")
        color_map <- c(
          "Low" = settings$colors$low,
          "Moderate" = settings$colors$moderate,
          "Serious" = settings$colors$serious,
          "Critical" = settings$colors$critical,
          "No information" = settings$colors$no_info
        )
        color_map <- color_map[!sapply(color_map, is.null)]
        symbol_map <- c("Low" = "+", "Moderate" = "~", "Serious" = "−", 
                        "Critical" = "×", "No information" = "?")
      } else if (category_type == "5_robins_e") {
        risk_labels <- c("Low", "Some concerns", "High", "Very high", "No information")
        color_map <- c(
          "Low" = settings$colors$low,
          "Some concerns" = settings$colors$some_concerns,
          "High" = settings$colors$high,
          "Very high" = settings$colors$very_high,
          "No information" = settings$colors$no_info
        )
        color_map <- color_map[!sapply(color_map, is.null)]
        symbol_map <- c("Low" = "+", "Some concerns" = "−", "High" = "×", 
                        "Very high" = "××", "No information" = "?")
      } else {
        risk_labels <- c("Low", "Some concerns", "High", "No information")
        color_map <- c(
          "Low" = settings$colors$low,
          "Some concerns" = settings$colors$some_concerns,
          "High" = settings$colors$high,
          "No information" = settings$colors$no_info
        )
        color_map <- color_map[!sapply(color_map, is.null)]
        symbol_map <- c("Low" = "+", "Some concerns" = "−", "High" = "×", "No information" = "?")
      }
      
      risk_labels <- intersect(risk_labels, all_values)
      
      if (length(color_map) == 0) {
        showNotification("No colors available. Please check color picker inputs.", type = "error")
        return(NULL)
      }
      
      list(risk_labels = risk_labels, color_map = color_map, symbol_map = symbol_map)
    })
    
    # Plot functions
    plot1_function <- function(data, settings) {
      req(rob_config())
      config <- rob_config()
      
      rob_summary_plot(
        df = data,
        color_map = config$color_map,
        risk_labels = config$risk_labels,
        domain_names = domain_names(),
        include_overall = settings$include_overall_summary,
        base_size = settings$font_size_summary
      )
    }
    
    plot2_function <- function(data, settings) {
      req(rob_config())
      config <- rob_config()
      
      rob_traffic_plot(
        df = data,
        color_map = config$color_map,
        symbol_map = config$symbol_map,
        domain_names = domain_names(),
        psize = settings$circle_size,
        show_overall = settings$show_overall_traffic,
        font_size = settings$font_size_traffic,
        legend_title = "Risk of Bias"
      )
    }
    
    # Initialize plot display modules
    plotDisplayServer("plot1", plot1_function, plot_data, plot_settings)
    plotDisplayServer("plot2", plot2_function, plot_data, plot_settings)
  })
}
