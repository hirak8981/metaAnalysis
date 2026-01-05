#===============================================================================
# MODULE: Network Meta-Analysis Main Module
# FINAL FIXED VERSION with all corrections
#===============================================================================

networkMainUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Custom CSS for styling
    tags$style(HTML("
      .dataTable_wrapper .dataTables_paginate .paginate_button {
        background: #ffffff !important;
        background-color: #ffffff !important;
        color: #333333 !important;
        border: 1px solid #dddddd !important;
      }
      .dataTable_wrapper .dataTables_paginate .paginate_button:hover {
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
            
      /* Standardize all sidebar action buttons */
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
    
    # Sidebar Layout
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 330,
        open = TRUE,
        
        # Reset Button
        div(
          style = "margin-bottom: 20px; padding-bottom: 15px; border-bottom: 2px solid #DEE2E6;",
          actionButton(
            ns("reset_nma"),
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
        nmaDataLoaderUI(ns("data_loader")),
        
        tags$hr(style = "border-color: #DEE2E6; margin: 20px 0;"),
        
        # File Upload
        nmaFileUploadUI(ns("file_upload")),
        
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
        )
      ),
      
      # Main Content with Tabs
      bslib::navset_card_tab(
        id = ns("nma_tabs"),
        
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
                p("View and edit your network meta-analysis data", style = "color: #7F8C8D; margin: 0;")
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
            nmaResultsDisplayUI(ns("results_display"))
          )
        ),
        
        # Tab 3: Network & Forest
        bslib::nav_panel(
          title = "Network & Forest",
          icon = icon("diagram-project"),
          value = "network_tab",
          div(
            style = "padding: 25px;",
            nmaNetworkPlotsUI(ns("network_plots"))
          )
        ),
        
        # Tab 4: Diagnostics
        bslib::nav_panel(
          title = "Diagnostics",
          icon = icon("magnifying-glass-chart"),
          value = "diagnostics_tab",
          div(
            style = "padding: 25px;",
            nmaRankingInconsistencyUI(ns("ranking_inconsistency"))
          )
        )
      )
    )
  )
}

#===============================================================================
# SERVER
#===============================================================================

networkMainServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================================
    # REACTIVE VALUES
    # =========================================================================
    
    edited_data <- reactiveVal(NULL)
    edit_mode <- reactiveVal(FALSE)
    validation_shown <- reactiveVal(FALSE)
    reset_file_trigger <- reactiveVal(0)
    analysis_results <- reactiveVal(NULL)
    previous_data_type <- reactiveVal(NULL)  # ✅ FIX: Consistent naming
    data_type_changed <- reactiveVal(FALSE)  # ✅ FIX: Consistent naming
    
    # =========================================================================
    # INITIALIZE SUB-MODULES
    # =========================================================================
    
    loaded_data <- nmaDataLoaderServer("data_loader", nma_folder = "data/network", 
                                       reset_trigger = reactive(reset_file_trigger()))
    
    uploaded_file <- nmaFileUploadServer("file_upload", 
                                         reset_trigger = reactive(reset_file_trigger()))
    
    model_settings_module <- nmaModelSettingsServer(
      "model_settings",
      current_data = reactive({
        if (!is.null(uploaded_file()) && !is.null(uploaded_file()$data)) {
          uploaded_file()$data
        } else if (!is.null(loaded_data$data)) {
          loaded_data$data
        } else {
          NULL
        }
      }),
      data_type = reactive({
        if (!is.null(uploaded_file()) && !is.null(uploaded_file()$data_type)) {
          uploaded_file()$data_type  # ✅ Use stored value
        } else if (!is.null(loaded_data$data_type)) {
          loaded_data$data_type
        } else {
          NULL
        }
      })
    )
    
    plot_customization <- nmaPlotCustomizationServer(
      "plot_customization",
      treatment_classes = reactive({
        if (!is.null(analysis_results())) {
          analysis_results()$treatment_classes
        } else {
          NULL
        }
      })
    )
    
    
    # =========================================================================
    # RESET FUNCTION
    # =========================================================================
    
    reset_nma_state <- function() {
      edited_data(NULL)
      edit_mode(FALSE)
      analysis_results(NULL)
      previous_data_type(NULL)  # ✅ FIX: Reset tracking
      data_type_changed(FALSE)  # ✅ FIX: Clear flag
      updateActionButton(session, "toggle_edit", label = "Enable Editing", icon = icon("edit"))
      reset_file_trigger(reset_file_trigger() + 1)
    }
    
    # =========================================================================
    # CURRENT DATA
    # =========================================================================
    
    current_data <- reactive({
      raw_data <- if (!is.null(uploaded_file()) && !is.null(uploaded_file()$data)) {
        uploaded_file()$data
      } else if (!is.null(loaded_data$data)) {
        loaded_data$data
      } else {
        NULL
      }
      
      if (is.null(raw_data)) return(NULL)
      if (nrow(raw_data) == 0) return(raw_data)
      
      validation_result <- validate_nma_data(raw_data)
      
      if (!validation_result$valid) {
        if (!isTRUE(isolate(validation_shown()))) {
          show_nma_validation_error(validation_result$errors)
          validation_shown(TRUE)
          reset_nma_state()
        }
        return(NULL)
      } else {
        validation_shown(FALSE)
      }
      
      return(raw_data)
    })
    
    current_data_type <- reactive({
      if (!is.null(uploaded_file()) && !is.null(uploaded_file()$data_type)) {
        return(uploaded_file()$data_type)
      } else if (!is.null(loaded_data$data_type)) {
        return(loaded_data$data_type)
      } else {
        return(NULL)
      }
    })
    
    # =========================================================================
    # ✅ FIXED: DATA TYPE CHANGE DETECTION
    # =========================================================================
    
    # =========================================================================
    # ✅ FIXED: DATA TYPE CHANGE DETECTION - NO INFINITE LOOP
    # =========================================================================
    
    observeEvent(current_data_type(), {
      
      current_dtype <- current_data_type()
      prev_dtype <- isolate(previous_data_type())  # ✅ ISOLATE!
      
      # Only proceed if current_dtype exists
      if (is.null(current_dtype)) {
        return()
      }
      
      # ✅ Check for data type change
      if (!is.null(prev_dtype) && prev_dtype != current_dtype) {
        
        isolate(data_type_changed(TRUE))  # ✅ ISOLATE!
        
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
              
              # Change summary box
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
                  strong("Previous: "), format_nma_data_type_name(prev_dtype), br(),
                  strong("Current: "), format_nma_data_type_name(current_dtype)
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
              
              # Options explanation
              div(
                style = "background: #e8f4f8; padding: 12px; border-radius: 4px;",
                tags$ul(
                  style = "margin: 0; padding-left: 20px; color: #1d3557;",
                  tags$li(strong("Review Settings:"), " Opens model settings to configure for new data type"),
                  tags$li(strong("Reset & Configure:"), " Clears previous analysis and opens settings"),
                  tags$li(strong("Continue:"), " Keep current settings (may cause errors)")
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
      
      # ✅ ALWAYS update previous_data_type (ISOLATED to prevent loop)
      isolate(previous_data_type(current_dtype))
      
    }, ignoreNULL = TRUE, ignoreInit = TRUE)  # ✅ Critical!
    
    
    # Handle "Review Settings" button
    observeEvent(input$review_settings, {
      removeModal()
      data_type_changed(FALSE)
      
      showNotification(
        HTML("<strong>Please configure your model settings</strong><br>
              Adjust the effect measure and reference treatment for your new data type."),
        type = "warning",
        duration = 6,
        closeButton = TRUE
      )
      
      Sys.sleep(0.3)
      model_settings_module$show_modal()
    })
    
    # Handle "Reset & Configure" button
    observeEvent(input$reset_and_configure, {
      removeModal()
      analysis_results(NULL)
      data_type_changed(FALSE)
      
      showNotification(
        HTML("<strong>Analysis cleared</strong><br>
              Please configure settings for your new data type."),
        type = "message",
        duration = 4,
        closeButton = TRUE
      )
      
      Sys.sleep(0.3)
      model_settings_module$show_modal()
      bslib::nav_select("nma_tabs", selected = "data_tab")
    })
    
    # =========================================================================
    # RESET BUTTON
    # =========================================================================
    
    
    observeEvent(input$reset_nma, {  # ✅ FIXED: With underscore to match UI
      showModal(
        modalDialog(
          title = tagList(
            icon("exclamation-triangle", style = "color: #dc3545;"),
            " Reset Network Meta-Analysis Module?"
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
              tags$li("Reset to default dataset (Continuous)")
            ),
            p(strong("This action cannot be undone."), style = "color: #dc3545; margin: 0;")
          ),
          footer = tagList(
            modalButton("Cancel", icon = icon("times")),
            actionButton(
              session$ns("confirm_reset"),  # ✅ FIXED: With underscore
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
    
    observeEvent(input$confirm_reset, {  # ✅ FIXED: With underscore
      removeModal()
      
      # Call reset function (clears all reactive values)
      reset_nma_state()
      
      # ✅ FIXED: Correct module and input IDs with underscores
      updateSelectInput(session, "data_loader-selected_dataset", selected = character(0))
      
      # ✅ Small delay to let the auto-load complete
      Sys.sleep(0.2)
      
      # ✅ FIXED: Correct tab ID with underscore
      bslib::nav_select("nma_tabs", selected = "data_tab")
      
      showNotification(
        "Network meta-analysis module reset. Default dataset (Continuous) loaded.",
        type = "message",
        duration = 3,
        closeButton = TRUE
      )
    })
    
    
    
    # =========================================================================
    # EDIT MODE TOGGLE
    # =========================================================================
    
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
    
    # =========================================================================
    # DATA TABLE
    # =========================================================================
    
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
    
    # =========================================================================
    # MODEL SETTINGS
    # =========================================================================
    
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
    
    # =========================================================================
    # PLOT CUSTOMIZATION
    # =========================================================================
    
    observeEvent(input$open_plot_customization, {
      plot_customization$show_modal()
    })
    
    # =========================================================================
    # RUN ANALYSIS
    # =========================================================================
    
    run_analysis_logic <- function() {
      cat("\n=== [1] RUN ANALYSIS START ===\n")
      
      req(current_data())
      cat("=== [2] Data validated ===\n")
      
      # Validate data
      validation <- validate_nma_data(current_data())
      cat("=== [3] Validation completed ===\n")
      
      if (!validation$valid) {
        show_nma_validation_error(validation$errors)
        return()
      }
      
      # Get settings
      settings <- isolate(model_settings_module$settings())
      cat("=== [4] Settings retrieved ===\n")
      
      # Validate reference group is selected
      if (is.null(settings$reference_group) || settings$reference_group == "") {
        showNotification(
          "Please select a reference treatment in Model Settings",
          type = "warning",
          duration = 5
        )
        return()
      }
      cat("=== [5] Reference group validated ===\n")
      
      # Show informative progress notification for NMA
      showNotification(
        HTML('
      <div style="font-size: 14px;">
        <strong>Running Network Meta-Analysis...</strong><br>
        <span style="color: #6c757d; font-size: 12px;">
          This may take a few moments depending on network complexity.
        </span>
      </div>
    '),
        id = "analysis_progress",
        duration = NULL,
        type = "message",
        closeButton = FALSE
      )
      cat("=== [6] Progress notification shown ===\n")
      
      # ✅ Initialize results variable OUTSIDE tryCatch
      results <- NULL
      error_occurred <- FALSE
      
      cat("=== [7] Starting run_nma() ===\n")
      
      # ✅ Run NMA
      nma_result <- tryCatch({
        run_nma(
          data = isolate(current_data()),
          data_type = isolate(current_data_type()),
          effect_measure = settings$effect_measure,
          reference_group = settings$reference_group,
          model_type = settings$model_type,
          method_tau = settings$method_tau
        )
      }, error = function(e) {
        cat("=== [ERROR in run_nma]:", e$message, "===\n")
        error_occurred <<- TRUE
        removeNotification(id = "analysis_progress")
        showNotification(
          paste("Analysis error:", e$message),
          type = "error",
          duration = 8
        )
        return(NULL)
      })
      
      cat("=== [8] run_nma() COMPLETED ===\n")
      
      if (error_occurred || is.null(nma_result)) {
        cat("=== [ERROR] run_nma failed ===\n")
        return()
      }
      
      # ✅ Extract results
      cat("=== [9] Starting extract_all_nma_results ===\n")
      extracted <- tryCatch({
        extract_all_nma_results(
          nma_result$nma,
          run_netsplit = TRUE,
          model = settings$model_type,
          remove_na = TRUE
        )
      }, error = function(e) {
        cat("=== [ERROR in extract_all_nma_results]:", e$message, "===\n")
        error_occurred <<- TRUE
        removeNotification(id = "analysis_progress")
        showNotification(
          paste("Extraction error:", e$message),
          type = "error",
          duration = 8
        )
        return(NULL)
      })
      
      cat("=== [10] extract_all_nma_results() COMPLETED ===\n")
      
      if (error_occurred || is.null(extracted)) {
        cat("=== [ERROR] extract_all_nma_results failed ===\n")
        return()
      }
      
      # ✅ Extract league table
      cat("=== [11] Starting extract_league_table ===\n")
      league <- tryCatch({
        extract_league_table(
          nma_result$nma,
          model = settings$model_type,
          digits = 3,
          add_significance = TRUE
        )
      }, error = function(e) {
        cat("=== [ERROR in extract_league_table]:", e$message, "===\n")
        error_occurred <<- TRUE
        removeNotification(id = "analysis_progress")
        showNotification(
          paste("League table error:", e$message),
          type = "error",
          duration = 8
        )
        return(NULL)
      })
      
      cat("=== [12] extract_league_table() COMPLETED ===\n")
      
      if (error_occurred || is.null(league)) {
        cat("=== [ERROR] extract_league_table failed ===\n")
        return()
      }
      
      # ✅ Build result list
      cat("=== [13] Building result list ===\n")
      results <- list(
        nma = nma_result$nma,
        treatments = nma_result$treatments,
        nstudies = nma_result$nstudies,
        ncomparisons = nma_result$ncomparisons,
        results = extracted,
        league = league,
        settings = settings,
        treatment_classes = nma_result$treatment_classes,
        
        # ================================================================
        # PRE-COMPUTE EXPENSIVE OPERATIONS ONCE
        # ================================================================
        
        # 1. Rankogram data (for radial & stacked plots) - THIS IS CORRECT TYPE
        rankogram_data = tryCatch({
          message("🔄 Pre-computing rankogram data...")
          set.seed(123)
          netmeta::rankogram(nma_result$nma, nsim = 10000, random = TRUE)
        }, error = function(e) {
          message("⚠ Could not pre-compute rankogram data: ", e$message)
          NULL
        }),
        
        # 2. Ranking metrics data (for beading plot) - DIFFERENT TYPE
        ranking_metrics = tryCatch({
          message("🔄 Pre-computing ranking metrics (SUCRA)...")
          
          prefer_direction <- if (settings$effect_measure %in% c("OR", "RR", "HR")) {
            "small"
          } else {
            "small"
          }
          
          model_type <- if (settings$model_type == "random") "random" else "common"
          
          prepare_ranking_data(
            nma_obj = nma_result$nma,
            outcome_name = settings$effect_measure,
            prefer = prefer_direction,
            metrics_type = "SUCRA",
            model = model_type,
            nsim = 10000
          )
        }, error = function(e) {
          message("⚠ Could not pre-compute ranking metrics: ", e$message)
          NULL
        }),
        
        # 3. Design decomposition (for netheat plots)
        design_decomp = tryCatch({
          message("🔄 Pre-computing design decomposition...")
          netmeta::decomp.design(nma_result$nma)
        }, error = function(e) {
          message("⚠ Could not pre-compute design decomposition: ", e$message)
          NULL
        })
      )
      
      
      cat("=== [14] Result list built ===\n")
      cat("=== [15] Setting analysis_results ===\n")
      
      # ✅ Set results with isolation
      isolate({
        analysis_results(results)
        data_type_changed(FALSE)
      })
      
      cat("=== [16] Results set ===\n")
      
      # ✅ Clean up and notify
      removeNotification(id = "analysis_progress")
      cat("=== [17] Notification removed ===\n")
      
      showNotification(
        "Network meta-analysis completed successfully!",
        type = "message",
        duration = 3
      )
      cat("=== [18] Success notification shown ===\n")
      
      bslib::nav_select("nma_tabs", selected = "results_tab")
      cat("=== [19] Tab switched ===\n")
      
      cat("=== [20] RUN ANALYSIS COMPLETE ===\n\n")
    }
    
    
    observeEvent(input$run_analysis, {
      req(current_data())
      
      # ✅ Check if data type changed but user didn't review settings
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
      
      run_analysis_logic()
    })
    
    observeEvent(input$force_run, {
      removeModal()
      data_type_changed(FALSE)
      run_analysis_logic()
    })
    
    observeEvent(input$goto_settings, {
      removeModal()
      model_settings_module$show_modal()
    })
    
    # =========================================================================
    # DOWNLOAD HANDLERS
    # =========================================================================
    
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("nma_data_", Sys.Date(), ".csv")
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
        paste0("nma_data_", Sys.Date(), ".xlsx")
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
    
    # =========================================================================
    # INITIALIZE PLOT MODULES - ✅ FIXED PARAMETER NAMES
    # =========================================================================
    
    nmaNetworkPlotsServer(
      "network_plots",
      analysisresults = analysis_results,  # ✅ No underscore
      plotsettings = plot_customization$settings  # ✅ No underscore
    )
    
    nmaRankingInconsistencyServer(
      "ranking_inconsistency",
      analysisresults = analysis_results,  # ✅ No underscore
      plotsettings = plot_customization$settings  # ✅ No underscore
    )
    
    nmaResultsDisplayServer(
      "results_display",
      analysis_results = analysis_results  # ✅ No underscore
    )
    
  })
}