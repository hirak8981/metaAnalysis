# =============================================================================
# ROB Main Module - Complete ROB Functionality Wrapper (UPDATED)
# =============================================================================

robMainUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Inline CSS for tabs
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
      
      .bslib-sidebar-layout > .sidebar {
        background: linear-gradient(180deg, 
          #F8F9FA 0%,           /* Light gray (top - clean) */
          #DDE1E6 60%,          /* Your existing mid-gray */
          #DDE7ED 85%,          /* Barely-there blue tint */
          #D8E3EA 100%          /* Very subtle brand blue hint */
        ) !important;
      }
      
      /* All sidebar .btn-block have same width & height */
      .bslib-sidebar-layout > .sidebar .btn-block {
        width: 100% !important;
        min-height: 50px !important;
        padding: 10px 18px !important;
        font-size: 14px !important;
        font-weight: 600 !important;
        display: inline-flex !important;
        align-items: center !important;
        justify-content: center !important;
        box-sizing: border-box !important;
      }
    
      /* Icon–text spacing for all sidebar buttons */
      .bslib-sidebar-layout > .sidebar .btn i {
        margin-right: 8px !important;
      }"
      )),
    
    # Main Layout
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 330,
        open = TRUE,
        
        div(
          style = "margin-bottom: 20px; padding-bottom: 15px; border-bottom: 2px solid #DEE2E6;",
          actionButton(
            ns("reset_rob"),
            "Reset Module",
            icon = icon("rotate-right"),
            class = "btn-outline-danger btn-block",
            style = "font-weight: 600; font-size: 14px; padding: 10px;"
          ),
          tags$small(
            style = "color: #6c757d; display: block; margin-top: 8px; text-align: center;",
            "Clear all data and return to initial state"
          )
        ),
        
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
            class = "btn-primary btn-block"
          )
        ),
        
        
        tags$hr(style = "border-color: #DEE2E6; margin: 20px 0;"),
        
        # Plot Customization Button
        div(
          style = "margin-bottom: 20px;",  
          actionButton(
            ns("customize_plots"),
            "Plot Customization",
            icon = icon("palette"),
            class = "btn-outline-primary btn-block"
          )
        ),
        
        plotControlsUI(ns("plot_controls"))
      ),
      
      # Main content with tabs
      bslib::navset_card_tab(
        id = ns("rob_tabs"),
        
        # Tab 1: Data Table 
        bslib::nav_panel(
          title = "Data Table",
          icon = icon("table"),
          value = "data_tab",
          div(
            style = "padding: 25px;",
            div(
              style = "margin-bottom: 25px; display: flex; justify-content: space-between; align-items: center;",
              div(
                h3("Dataset Table", style = "color: #1d3557; margin-bottom: 10px;"),
                p("View and edit your Risk of Bias data", style = "color: #7F8C8D; margin: 0;")
              ),
              div(
                actionButton(
                  ns("toggle_edit"),
                  "Enable Editing",
                  icon = icon("edit"),
                  class = "btn-sm",
                  style = "margin-right: 10px; background: #667eea; color: white; border: none;"
                ),
                downloadButton(
                  ns("download_csv"),
                  "CSV",
                  class = "btn-sm",
                  style = "margin-right: 5px; font-size: 12px;"
                ),
                downloadButton(
                  ns("download_excel"),
                  "Excel",
                  class = "btn-sm",
                  style = "font-size: 12px;"
                )
              )
            ),
            div(
              style = "background: white; padding: 20px; border-radius: 8px;",
              DTOutput(ns("data_table"))
            )
          )
        ),
        
        # Tab 2: Visualizations
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
            ),
            div(
              style = "margin-bottom: 40px;",
              plotDisplayUI(ns("plot3"), "Heatmap Plot")  # NEW
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
    edited_data <- reactiveVal(NULL)
    edit_mode <- reactiveVal(FALSE)
    validation_shown <- reactiveVal(FALSE)
    reset_file_trigger <- reactiveVal(0)  # NEW: Counter for file reset
    plots_ready <- reactiveVal(FALSE) 
    
    # Initialize sub-modules AFTER reactive values
    loaded_data <- dataLoaderServer("data_loader", rob_folder = "data/rob")
    uploaded_file <- fileUploadServer("file_upload", reset_trigger = reactive(reset_file_trigger()))
    plot_controls <- plotControlsServer("plotcontrols")
    

    
    # Connect main generate button to plot controls
    observeEvent(input$generate_plots_main, {
      bslib::nav_select("rob_tabs", selected = "viz_tab")
      plots_ready(TRUE)                                # NEW
      plot_controls()$trigger_generation()
    })

    
    # Reset function for invalid data
    reset_rob_state <- function() {
      edited_data(NULL)
      edit_mode(FALSE)
      plots_ready(FALSE)                               # NEW
      updateActionButton(session, "toggle_edit",
                         label = "Enable Editing",
                         icon = icon("edit"))
      reset_file_trigger(reset_file_trigger() + 1)
    }
    
    
    # =========================================================================
    # RESET BUTTON HANDLER (SINGLE VERSION - NO DUPLICATES)
    # =========================================================================
    
    observeEvent(input$reset_rob, {
      showModal(
        modalDialog(
          title = tagList(
            icon("exclamation-triangle", style = "color: #dc3545;"),
            " Reset ROB Module?"
          ),
          
          div(
            style = "padding: 10px;",
            p(
              "This will clear all your work and return the module to its initial state:",
              style = "margin-bottom: 15px; font-size: 15px;"
            ),
            tags$ul(
              style = "color: #5a6169; margin-bottom: 15px;",
              tags$li("Remove uploaded files"),
              tags$li("Clear all data edits"),
              tags$li("Clear all visualizations"),
              tags$li("Deselect current dataset")
            ),
            p(
              strong("This action cannot be undone."),
              style = "color: #dc3545; margin: 0;"
            )
          ),
          
          footer = tagList(
            modalButton("Cancel", icon = icon("times")),
            actionButton(
              session$ns("confirm_reset"),
              "Yes, Reset Module",
              icon = icon("rotate-right"),
              class = "btn-danger"
            )
          ),
          
          size = "m",
          easyClose = TRUE,
          fade = TRUE
        )
      )
    })
    
    # SINGLE confirm reset handler
    # Confirm reset
    observeEvent(input$confirm_reset, {
      removeModal()
      
      # Reset all reactive values
      edited_data(NULL)
      validation_shown(FALSE)
      edit_mode(FALSE)
      plots_ready(FALSE) 
      
      # Trigger file upload reset (CHANGED)
      reset_file_trigger(reset_file_trigger() + 1)
      
      # Update UI elements
      updateActionButton(session, "toggle_edit",
                         label = "Enable Editing",
                         icon = icon("edit"))
      
      # Clear dataset selection
      updateSelectInput(session, "data_loader-selected_dataset", selected = character(0))
      
      # Switch to data tab
      bslib::nav_select("rob_tabs", selected = "data_tab")
      
      # Show notification
      showNotification(
        "ROB module reset complete. Please select a dataset or upload a file.",
        type = "message",
        duration = 3,
        closeButton = TRUE
      )
    })
    
    
    # =========================================================================
    # CURRENT DATA WITH VALIDATION
    # =========================================================================
    
    current_data <- reactive({
      raw_data <- if (!is.null(uploaded_file()$data)) {
        uploaded_file()$data
      } else {
        loaded_data()$current_data
      }
      
      if (is.null(raw_data)) {
        return(NULL)
      }
      
      if (nrow(raw_data) == 0) {
        return(raw_data)
      }
      
      validation_result <- validate_rob_data(raw_data)
      
      if (!validation_result$valid) {
        if (!isTRUE(isolate(validation_shown()))) {
          show_validation_error(validation_result$errors)
          validation_shown(TRUE)
        }
        reset_rob_state()
        return(NULL)
      } else {
        validation_shown(FALSE)
      }
      
      return(raw_data)
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
    # Replace the auto-detect observer with this COMPLETE version:
    
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
      
      # Detect tool type based on unique values
      tool_type <- if ("unclear" %in% all_values && !("moderate" %in% all_values) && !("some concerns" %in% all_values)) {
        "3_quadas"  # QUADAS-2: Low, High, Unclear
      } else if ("moderate" %in% all_values && "serious" %in% all_values && "critical" %in% all_values) {
        "5_robins_i"  # ROBINS-I: Low, Moderate, Serious, Critical, No information
      } else if ("very high" %in% all_values || "very_high" %in% all_values) {
        "5_robins_e"  # ROBINS-E: Low, Some Concerns, High, Very High, No information
      } else if ("moderate" %in% all_values && !("serious" %in% all_values)) {
        "4_quips"  # QUIPS: Low, Moderate, High, No information
      } else {
        "4_category"  # ROB 2: Low, Some Concerns, High, No information (default)
      }
      
      plot_controls()$set_type(tool_type)
    })
    
    
    # ADD THIS OBSERVER:
    observeEvent(input$customize_plots, {
      plot_controls()$show_modal()
    })
    # =========================================================================
    # EDITABLE DATA TABLE
    # =========================================================================
    
    # Toggle edit mode
    observeEvent(input$toggle_edit, {
      edit_mode(!edit_mode())
      
      if (edit_mode()) {
        updateActionButton(session, "toggle_edit",
                           label = "Disable Editing",
                           icon = icon("save"))
        showNotification("Edit mode enabled. Click cells to edit.",
                         type = "message", duration = 3)
      } else {
        updateActionButton(session, "toggle_edit",
                           label = "Enable Editing",
                           icon = icon("edit"))
        showNotification("Changes saved.", type = "message", duration = 2)
      }
    })
    
    # Update data table with full dataset and edit capability
    output$data_table <- renderDT({
      data_to_show <- if (!is.null(edited_data())) {
        edited_data()
      } else {
        req(current_data())
        current_data()
      }
      
      datatable(
        data_to_show,
        editable = if(edit_mode()) list(target = "cell", disable = list(columns = 0)) else FALSE,
        options = list(
          pageLength = 15,
          lengthMenu = c(10, 15, 25, 50, 100),
          searching = TRUE,
          info = TRUE,
          scrollX = TRUE,
          paging = TRUE,
          ordering = TRUE,
          dom = 'frtip',
          buttons = c('copy', 'print'),
          pagingType = 'full_numbers',
          columnDefs = list(
            list(className = 'dt-center', targets = 1:(ncol(data_to_show) - 1))
          )
        ),
        filter = 'none',
        rownames = FALSE,
        class = "display compact stripe hover",
        style = "bootstrap5"
      )
    })
    
    # Handle cell edits
    observeEvent(input$data_table_cell_edit, {
      info <- input$data_table_cell_edit
      data_copy <- if (!is.null(edited_data())) edited_data() else current_data()
      data_copy[info$row, info$col] <- info$value
      edited_data(data_copy)
    })
    
    
    # CSV Download
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("rob_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        data_to_export <- if (!is.null(edited_data())) edited_data() else current_data()
        write.csv(data_to_export, file, row.names = FALSE)
      }
    )
    
    # Excel Download
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("rob_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        data_to_export <- if (!is.null(edited_data())) edited_data() else current_data()
        writexl::write_xlsx(data_to_export, file)
      }
    )
    
    # =========================================================================
    # PLOT DATA AND SETTINGS
    # =========================================================================
    
    plot_data <- reactive({
      req(plots_ready())          # Only allow if Generate Plots was clicked
      if (!is.null(edited_data())) {
        edited_data()
      } else {
        req(current_data())
        current_data()
      }
    })
    
    observeEvent(input$rob_tabs, {
      if (identical(input$rob_tabs, "viz_tab") && !isTRUE(plots_ready())) {
        showNotification(
          HTML("<strong>Plots not generated yet.</strong><br>
           Please click the <em>Generate Plots</em> button in the sidebar to create visualizations."),
          type = "warning",
          duration = 5,
          closeButton = TRUE
        )
      }
    })
    
    plot_settings <- reactive({
      req(plot_controls())
      plot_controls()
    })
    
    # ROB configuration (UPDATED - Add QUADAS and QUIPS)
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
        tolower() %>%
        as.character()
      
      # QUADAS-2: 3 categories (Low, High, Unclear)
      if ("unclear" %in% all_values && !("moderate" %in% all_values) && !("some concerns" %in% all_values)) {
        risk_labels <- c("Low", "High", "Unclear")
        color_map <- c(
          "Low" = settings$colors$low,
          "High" = settings$colors$high,
          "Unclear" = "#F39C12"
        )
        color_map <- color_map[!sapply(color_map, is.null)]
        symbol_map <- c("Low" = "+", "High" = "×", "Unclear" = "?")
        
      } else if ("moderate" %in% all_values && !("serious" %in% all_values) && !("critical" %in% all_values)) {
        risk_labels <- c("Low", "Moderate", "High", "No information")
        color_map <- c(
          "Low" = settings$colors$low,
          "Moderate" = settings$colors$moderate,
          "High" = settings$colors$high,
          "No information" = settings$colors$no_info
        )
        color_map <- color_map[!sapply(color_map, is.null)]
        symbol_map <- c("Low" = "+", "Moderate" = "~", "High" = "×", "No information" = "?")
        
      } else if (category_type == "5_robins_i" || ("moderate" %in% all_values && "serious" %in% all_values && "critical" %in% all_values)) {
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
        
      } else if (category_type == "5_robins_e" || ("very high" %in% all_values || "very_high" %in% all_values)) {
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
      
      risk_labels <- intersect(risk_labels, unique(df %>% select(-Study) %>% unlist() %>% na.omit()))
      
      if (length(color_map) == 0) {
        showNotification("No colors available. Please check color picker inputs.", type = "error")
        return(NULL)
      }
      
      list(risk_labels = risk_labels, color_map = color_map, symbol_map = symbol_map)
    })
    
    # =========================================================================
    # PLOT FUNCTIONS
    # =========================================================================
    
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
    
    plot3_function <- function(data, settings) {
      req(rob_config())
      config <- rob_config()
      
      rob_heatmap_plot(
        df = data,
        color_map = config$color_map,
        risk_labels = config$risk_labels,
        show_row_avg = TRUE,
        show_col_avg = TRUE,
        font_size = settings$font_size_summary,
        show_overall = settings$include_overall_summary
      )
    }
    
    plotDisplayServer("plot1", plot1_function, plot_data, plot_settings)
    plotDisplayServer("plot2", plot2_function, plot_data, plot_settings)
    plotDisplayServer("plot3", plot3_function, plot_data, plot_settings)
  })
}

