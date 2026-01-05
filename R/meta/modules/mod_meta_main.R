# =============================================================================
# Meta-Analysis Main Module - Matching ROB Structure
# =============================================================================

metaMainUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Custom CSS for styling
    tags$style(HTML("
      .dataTable_wrapper .dataTable_paginate .paginate_button {
        background: #ffffff !important;
        background-color: #ffffff !important;
        color: #333333 !important;
        border: 1px solid #dddddd !important;
      }
      .dataTable_wrapper .dataTable_paginate .paginate_button:hover {
        background: #f9f9f9 !important;
        background-color: #f9f9f9 !important;
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
    
    # Sidebar Layout (like ROB)
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 330,
        open = TRUE,
        
        # Reset Button
        div(
          style = "margin-bottom: 20px; padding-bottom: 15px; border-bottom: 2px solid #DEE2E6;",
          actionButton(
            ns("reset_meta"),
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
        
        # Data Loader
        metaDataLoaderUI(ns("data_loader")),
        
        tags$hr(style = "border-color: #DEE2E6; margin: 20px 0;"),
        
        # File Upload
        metaFileUploadUI(ns("file_upload")),
        
        tags$hr(style = "border-color: #DEE2E6; margin: 20px 0;"),
        
        # Model Settings Button
        div(
          style = "margin-bottom: 15px;",
          actionButton(
            ns("open_model_settings"),
            "Model Settings",
            icon = icon("sliders"),
            class = "btn-outline-primary btn-block"
          )
        ),
        
        tags$hr(style = "border-color: #DEE2E6; margin: 20px 0;"),
        # Run Analysis Button
        div(
          style = "margin-bottom: 20px;",
          actionButton(
            ns("run_analysis"),
            "Run Analysis",
            icon = icon("play"),
            class = "btn-primary btn-block"
          )
        ),
        
        tags$hr(style = "border-color: #DEE2E6; margin: 20px 0;"),
        
        # Plot Customization Button
        div(
          style = "margin-bottom: 20px;",
          actionButton(
            ns("open_plot_customization"),
            "Plot Customization",
            icon = icon("palette"),
            class = "btn-outline-primary btn-block"
          )
        ),
        
      ),
      
      # Main Content with Tabs
      bslib::navset_card_tab(
        id = ns("meta_tabs"),
        
        # Tab 1: Data Table
        bslib::nav_panel(
          title = "Data Table",
          icon = icon("table"),
          value = "data_tab",
          div(
            style = "padding: 25px;",
            
            # Header with actions
            div(
              style = "margin-bottom: 25px; display: flex; justify-content: space-between; align-items: center;",
              div(
                h3("Dataset Table", style = "color: #1d3557; margin-bottom: 10px;"),
                p("View and edit your meta-analysis data", style = "color: #7F8C8D; margin: 0;")
              ),
              div(
                actionButton(
                  ns("toggle_edit"),
                  "Enable Editing",
                  icon = icon("edit"),
                  class = "btn-sm",
                  style = "margin-right: 10px; background: #667eea; color: white; border: none;"
                ),
                downloadButton(ns("download_csv"), "CSV", class = "btn-sm",
                               style = "margin-right: 5px; font-size: 12px;"),
                downloadButton(ns("download_excel"), "Excel", class = "btn-sm",
                               style = "font-size: 12px;")
              )
            ),
            
            # Data Table
            div(
              style = "background: white; padding: 20px; border-radius: 8px;",
              DTOutput(ns("data_table"))
            )
          )
        ),
        
        # Tab 2: Results
        bslib::nav_panel(
          title = "Results",
          icon = icon("chart-bar"),
          value = "results_tab",
          div(
            style = "padding: 25px;",
            metaResultsDisplayUI(ns("results_display"))
          )
        ),
        
        # Tab 3: Visualizations (Effect Plots)
        bslib::nav_panel(
          title = "Effect Plots",
          icon = icon("chart-line"),
          value = "viz_tab",
          div(
            style = "padding: 25px;",
            metaEffectPlotsUI(ns("effect_plots"))
          )
        ),
        
        # Tab 4: Diagnostics (Influence Plots)
        bslib::nav_panel(
          title = "Diagnostics",
          icon = icon("magnifying-glass-chart"),
          value = "influence_tab",
          div(
            style = "padding: 25px;",
            metaInfluencePlotsUI(ns("influence_plots"))
          )
        )
      )
    )
  )
}

# =============================================================================
# SERVER
# =============================================================================

metaMainServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ========================================================================
    # 1. REACTIVE VALUES
    # ========================================================================
    
    edited_data <- reactiveVal(NULL)
    edit_mode <- reactiveVal(FALSE)
    validation_shown <- reactiveVal(FALSE)
    reset_file_trigger <- reactiveVal(0)
    analysis_results <- reactiveVal(NULL)
    influence_results <- reactiveVal(NULL)
    previous_data_type <- reactiveVal(NULL)  # Track previous data type
    data_type_changed <- reactiveVal(FALSE)  # Flag for data type change
    
    # ========================================================================
    # 2. INITIALIZE SUB-MODULES
    # ========================================================================
    
    loaded_data <- metaDataLoaderServer("data_loader", meta_folder = "data/meta")
    uploaded_file <- metaFileUploadServer("file_upload", reset_trigger = reactive(reset_file_trigger()))
    
    model_settings_module <- metaModelSettingsServer(
      "model_settings",
      current_data = reactive({
        if (!is.null(uploaded_file()$data)) {
          uploaded_file()$data
        } else {
          loaded_data()$current_data
        }
      }),
      data_type = reactive({
        if (!is.null(uploaded_file()$data)) {
          validation <- validate_meta_data(uploaded_file()$data)
          validation$data_type
        } else {
          loaded_data()$data_type
        }
      })
    )
    
    # INITIALIZE PLOT CUSTOMIZATION MODULE (NEW)
    plot_custom_module <- metaPlotCustomizationServer("plot_customization")
    
    # ========================================================================
    # 3. RESET FUNCTION
    # ========================================================================
    
    reset_meta_state <- function() {
      edited_data(NULL)
      edit_mode(FALSE)
      analysis_results(NULL)
      influence_results(NULL)
      previous_data_type(NULL)  # Reset data type tracking
      data_type_changed(FALSE)  # Reset change flag
      
      updateActionButton(session, "toggle_edit", label = "Enable Editing", icon = icon("edit"))
      reset_file_trigger(reset_file_trigger() + 1)
    }
    
    # ========================================================================
    # 4. RESET BUTTON
    # ========================================================================
    
    observeEvent(input$reset_meta, {
      showModal(
        modalDialog(
          title = tagList(
            icon("exclamation-triangle", style = "color: #dc3545;"),
            " Reset Meta-Analysis Module?"
          ),
          div(
            style = "padding: 10px;",
            p("This will clear all your work and return the module to its initial state.",
              style = "margin-bottom: 15px; font-size: 15px;"),
            tags$ul(
              style = "color: #5a6169; margin-bottom: 15px;",
              tags$li("Remove uploaded files"),
              tags$li("Clear all data edits"),
              tags$li("Clear all analysis results"),
              tags$li("Reset to default dataset (Continuous)")  # ✅ UPDATED TEXT
            ),
            p(strong("This action cannot be undone."), style = "color: #dc3545; margin: 0;")
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
    
    observeEvent(input$confirm_reset, {
      removeModal()
      
      # Call reset function (clears all reactive values)
      reset_meta_state()
      
      
      # trigger a fresh load by briefly clearing and letting default take over
      updateSelectInput(session, "data_loader-selected_dataset", selected = "continuous")
      
      # Small delay to ensure data loads
      Sys.sleep(0.2)
      
      # Switch to data tab
      bslib::nav_select("meta_tabs", selected = "data_tab")
      
      # UPDATED MESSAGE
      showNotification(
        "Meta-analysis module reset. Default dataset (Continuous) loaded.",
        type = "message",
        duration = 3,
        closeButton = TRUE
      )
    })
    
    
    # ========================================================================
    # 5. CURRENT DATA
    # ========================================================================
    
    current_data <- reactive({
      raw_data <- if (!is.null(uploaded_file()$data)) {
        uploaded_file()$data
      } else {
        loaded_data()$current_data
      }
      
      if (is.null(raw_data)) return(NULL)
      if (nrow(raw_data) == 0) return(raw_data)
      
      validation_result <- validate_meta_data(raw_data)
      
      if (!validation_result$valid) {
        if (!isTRUE(isolate(validation_shown()))) {
          show_meta_validation_error(validation_result$errors)
          validation_shown(TRUE)
        }
        reset_meta_state()
        return(NULL)
      } else {
        validation_shown(FALSE)
      }
      
      return(raw_data)
    })
    
    current_data_type <- reactive({
      if (!is.null(uploaded_file()$data)) {
        validation <- validate_meta_data(uploaded_file()$data)
        return(validation$data_type)
      } else {
        return(loaded_data()$data_type)
      }
    })
    
    # ========================================================================
    # DETECT DATA TYPE CHANGES AND PROMPT USER
    # ========================================================================
    observe({
      current_dtype <- current_data_type()
      prev_dtype <- previous_data_type()
      
      # Skip if no data loaded yet
      if (is.null(current_dtype)) {
        return()
      }
      
      # Check if data type has changed
      if (!is.null(prev_dtype) && prev_dtype != current_dtype) {
        # Data type changed!
        data_type_changed(TRUE)
        
        # Show informative modal
        showModal(
          modalDialog(
            title = tagList(
              icon("exclamation-triangle", style = "color: #f39c12;"),
              " Data Type Changed"
            ),
            size = "m",
            easyClose = FALSE,
            fade = TRUE,
            div(
              style = "padding: 15px;",
              
              # Change summary
              div(
                style = "background: #fff3cd; border-left: 4px solid #f39c12; 
                     padding: 15px; border-radius: 4px; margin-bottom: 20px;",
                h5(
                  icon("info-circle"),
                  " Dataset Type Changed",
                  style = "color: #856404; margin: 0 0 10px 0;"
                ),
                p(
                  style = "margin: 0; color: #856404;",
                  strong("Previous:"), " ", format_data_type_name(prev_dtype), br(),
                  strong("Current:"), " ", format_data_type_name(current_dtype)
                )
              ),
              
              # Warning message
              div(
                style = "margin-bottom: 15px;",
                p(
                  style = "font-size: 15px; line-height: 1.6;",
                  "Your current model settings may not be compatible with the new data type.",
                  br(), br(),
                  "What would you like to do?"
                )
              ),
              
              # Action buttons explanation
              div(
                style = "background: #e8f4f8; padding: 12px; border-radius: 4px;",
                tags$ul(
                  style = "margin: 0; padding-left: 20px; color: #1d3557;",
                  tags$li(
                    strong("Review Settings:"), 
                    " Opens model settings to configure for new data type"
                  ),
                  tags$li(
                    strong("Reset & Configure:"), 
                    " Clears previous analysis and opens settings"
                  ),
                  tags$li(
                    strong("Continue:"), 
                    " Keep current settings (may cause errors)"
                  )
                )
              )
            ),
            
            footer = tagList(
              actionButton(
                session$ns("review_settings"),
                "Review Settings",
                icon = icon("sliders"),
                class = "btn-warning",
                style = "font-weight: 600;"
              ),
              actionButton(
                session$ns("reset_and_configure"),
                "Reset & Configure",
                icon = icon("rotate-right"),
                class = "btn-primary",
                style = "font-weight: 600;"
              ),
              modalButton(
                "Continue Anyway",
                icon = icon("forward")
              )
            )
          )
        )
      }
      
      # Update previous data type
      previous_data_type(current_dtype)
    })
    
    # Handle "Review Settings" button
    observeEvent(input$review_settings, {
      removeModal()
      
      # Clear flag immediately when user chooses to review
      data_type_changed(FALSE)
      
      showNotification(
        HTML("<strong>Please configure your model settings</strong><br>
         Adjust the effect measure and other options for your new data type."),
        type = "warning",
        duration = 6,
        closeButton = TRUE
      )
      
      # Open model settings after a brief delay
      Sys.sleep(0.3)  # Use Sys.sleep instead of shinyjs::delay
      model_settings_module$show_modal()
    })
    
    
    # Handle "Reset & Configure" button
# Handle "Reset & Configure" button
observeEvent(input$reset_and_configure, {
  removeModal()
  
  # Reset analysis results
  analysis_results(NULL)
  influence_results(NULL)
  
  # ✅ Clear flag immediately
  data_type_changed(FALSE)
  
  showNotification(
    HTML("<strong>Analysis cleared</strong><br>
         Please configure settings for your new data type."),
    type = "message",
    duration = 4,
    closeButton = TRUE
  )
  
  # Open model settings
  Sys.sleep(0.3)  # Use Sys.sleep instead of shinyjs::delay
  model_settings_module$show_modal()
  
  # Switch to data tab
  bslib::nav_select("meta_tabs", selected = "data_tab")
})

    
    # ========================================================================
    # 6. EDIT MODE TOGGLE
    # ========================================================================
    
    observeEvent(input$toggle_edit, {
      edit_mode(!edit_mode())
      if (edit_mode()) {
        updateActionButton(session, "toggle_edit", label = "Disable Editing", icon = icon("save"))
        showNotification("Edit mode enabled. Click cells to edit.", type = "message", duration = 3)
      } else {
        updateActionButton(session, "toggle_edit", label = "Enable Editing", icon = icon("edit"))
        showNotification("Changes saved.", type = "message", duration = 2)
      }
    })
    
    # ========================================================================
    # 7. DATA TABLE
    # ========================================================================
    
    output$data_table <- renderDT({
      data_to_show <- if (!is.null(edited_data())) {
        edited_data()
      } else {
        req(current_data())
        current_data()
      }
      
      datatable(
        data_to_show,
        editable = if (edit_mode()) {
          list(target = "cell", disable = list(columns = 0))
        } else {
          FALSE
        },
        options = list(
          pageLength = 15,
          lengthMenu = c(10, 15, 25, 50, 100),
          searching = TRUE,
          scrollX = TRUE,
          paging = TRUE,
          ordering = TRUE,
          dom = 'frtip',
          pagingType = "full_numbers",
          columnDefs = list(
            list(className = "dt-center", targets = 1:(ncol(data_to_show) - 1))
          )
        ),
        filter = "none",
        rownames = FALSE,
        class = "display compact stripe hover",
        style = "bootstrap5"
      )
    })
    
    observeEvent(input$data_table_cell_edit, {
      info <- input$data_table_cell_edit
      data_copy <- if (!is.null(edited_data())) {
        edited_data()
      } else {
        current_data()
      }
      data_copy[info$row, info$col] <- info$value
      edited_data(data_copy)
    })
    
    # ========================================================================
    # 8. MODEL SETTINGS
    # ========================================================================
    
    observeEvent(input$open_model_settings, {
      if (is.null(current_data())) {
        showNotification(
          "Please load or upload data first!",
          type = "warning",
          duration = 3
        )
        return()
      }
      model_settings_module$show_modal()
      
    })
    
    # ========================================================================
    # 8B. PLOT CUSTOMIZATION (NEW)
    # ========================================================================
    
    observeEvent(input$open_plot_customization, {
      plot_custom_module$show_modal()
    })
    
    
    # ========================================================================
    # 8C. HANDLE FORCE RUN AND GOTO SETTINGS (MISSING HANDLERS)
    # ========================================================================
    # Handle "Force Run" button
    observeEvent(input$force_run, {
      removeModal()
      data_type_changed(FALSE)  # Clear flag
      # Trigger the analysis by calling the rest of the run_analysis logic
    })
    
    # Handle "Goto Settings" button  
    observeEvent(input$goto_settings, {
      removeModal()
      model_settings_module$show_modal()
    })
    
    # ========================================================================
    # 9. RUN ANALYSIS (REFACTORED WITH SHARED LOGIC)
    # ========================================================================
    
    # Shared function for running analysis
    run_analysis_logic <- function() {
      req(current_data())
      
      # Validate data
      validation <- validate_meta_data(current_data())
      if (!validation$valid) {
        show_meta_validation_error(validation$errors)
        return()
      }
      
      # Get settings
      settings <- model_settings_module$settings()
      
      # Validate effect measure is selected (LAYER 2 - HARD STOP)
      if (is.null(settings$measure) || settings$measure == "") {
        showNotification(
          "Please select an effect size measure in Model Settings",
          type = "warning",
          duration = 5
        )
        return()
      }
      
      # Show progress notification
      showNotification(
        "Running meta-analysis...",
        id = "analysis_progress",
        duration = NULL,
        type = "message"
      )
      
      # Run the analysis
      results <- tryCatch({
        run_meta_analysis(
          data = current_data(),
          data_type = current_data_type(),
          measure = settings$measure,
          method = settings$method,
          transform = settings$transform,
          subgroup = settings$subgroup,
          confidence_level = settings$confidence_level
        )
      }, error = function(e) {
        removeNotification(id = "analysis_progress")
        showNotification(
          paste("Analysis error:", e$message),
          type = "error",
          duration = 8
        )
        return(NULL)
      })
      
      # Process results
      if (!is.null(results)) {
        analysis_results(results)
        
        # clear the data type changed flag after successful analysis
        data_type_changed(FALSE)
        
        # Run influence analysis for ALL models
        infl_results <- tryCatch({
          perform_influence_analysis(results$model)
        }, error = function(e) {
          showNotification(
            paste("Influence analysis warning:", e$message),
            type = "warning",
            duration = 5
          )
          return(NULL)
        })
        
        # re-compute Leave-One-Out analysis
        if (!is.null(infl_results)) {
          message("🔄 Pre-computing leave-one-out analysis...")
          
          loo_precomputed <- tryCatch({
            metafor::leave1out(results$model)
          }, error = function(e) {
            message("⚠ Leave-one-out analysis failed: ", e$message)
            NULL
          })
          
          # Add to influence results
          infl_results$loo_precomputed <- loo_precomputed
          
          if (!is.null(loo_precomputed)) {
            message("✓ Leave-one-out analysis pre-computed successfully")
          }
        }
        
        influence_results(infl_results) # ALWAYS set results (even if NULL on error)
        
        removeNotification(id = "analysis_progress")
        showNotification(
          "Analysis completed successfully!",
          type = "message",
          duration = 3
        )
        
        bslib::nav_select("meta_tabs", selected = "results_tab")
      }
    }
    
    # Main run analysis button observer
    observeEvent(input$run_analysis, {
      req(current_data())
      
      # check if data type changed but user didn't review settings
      if (isTRUE(data_type_changed())) {
        showModal(
          modalDialog(
            title = tagList(
              icon("exclamation-triangle", style = "color: #dc3545;"),
              " Settings Not Updated"
            ),
            size = "m",
            easyClose = TRUE,
            p(
              style = "font-size: 15px;",
              "Your data type has changed, but model settings haven't been reviewed.",
              br(), br(),
              strong("We strongly recommend reviewing your settings before running the analysis.")
            ),
            footer = tagList(
              actionButton(
                session$ns("force_run"),
                "Run Anyway",
                icon = icon("triangle-exclamation"),
                class = "btn-danger"
              ),
              actionButton(
                session$ns("goto_settings"),
                "Review Settings First",
                icon = icon("sliders"),
                class = "btn-primary"
              )
            )
          )
        )
        return()
      }
      
      # If flag not set, run analysis normally
      run_analysis_logic()
    })
    
    # Handle "Force Run" button - runs analysis even with data type change
    observeEvent(input$force_run, {
      removeModal()
      data_type_changed(FALSE)  # Clear the flag
      run_analysis_logic()  # Actually run the analysis
    })
    
    # Handle "Review Settings First" button - opens model settings
    observeEvent(input$goto_settings, {
      removeModal()
      model_settings_module$show_modal()
    })
    
    
    # ========================================================================
    # 10. DOWNLOAD HANDLERS
    # ========================================================================
    
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("meta_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        data_to_export <- if (!is.null(edited_data())) {
          edited_data()
        } else {
          current_data()
        }
        write.csv(data_to_export, file, row.names = FALSE)
      }
    )
    
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("meta_data_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        data_to_export <- if (!is.null(edited_data())) {
          edited_data()
        } else {
          current_data()
        }
        writexl::write_xlsx(data_to_export, file)
      }
    )
    
    # ========================================================================
    # 11. INITIALIZE DISPLAY MODULES (UPDATED)
    # ========================================================================
    
    
    metaResultsDisplayServer("results_display", analysis_results = analysis_results)
    
    metaEffectPlotsServer(
      "effect_plots",
      analysis_results = analysis_results,
      current_data = current_data,
      model_settings = reactive(model_settings_module$settings()),
      plot_settings = reactive(plot_custom_module$settings())   
    )
    
    metaInfluencePlotsServer(
      "influence_plots",
      influence_results = influence_results,
      current_data = current_data,
      model_settings = reactive(model_settings_module$settings()),
      plot_settings = reactive(plot_custom_module$settings())  
    )
  })
}
