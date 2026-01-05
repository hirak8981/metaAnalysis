# =============================================================================
# Network Meta-Analysis Results Display Module (UPDATED)
# =============================================================================

nmaResultsDisplayUI <- function(id) {
  ns <- NS(id)
  div(
    style = "padding: 20px;",
    # Download All Results Button
    div(
      style = "margin-bottom: 20px; text-align: right;",
      downloadButton(
        ns("download_results_word"),
        "Download All Tables (Word)",
        icon = icon("file-word"),
        class = "btn-primary",
        style = "font-weight: 600;"
      )
    ),
    uiOutput(ns("results_content"))
  )
}

nmaResultsDisplayServer <- function(id, analysis_results) {
  moduleServer(id, function(input, output, session) {
    
    # ========================================================================
    # REACTIVE: League Table Format Toggle (NEW)
    # ========================================================================
    
    league_format <- reactiveVal("matrix") # Default: matrix
    
    # ========================================================================
    # RENDER RESULTS UI WITH FULLSCREEN CARDS
    # ========================================================================
    
    output$results_content <- renderUI({
      if (is.null(analysis_results())) {
        return(
          div(
            style = "padding: 40px; text-align: center;",
            icon("info-circle", style = "font-size: 48px; color: #457b9d; margin-bottom: 20px;"),
            h4("Configure model settings and run analysis to see results",
               style = "color: #5a6169; margin: 0;")
          )
        )
      }
      
      tagList(
        # Network Summary Card
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(class = "bg-primary", "Network Summary"),
          bslib::card_body(
            padding = "15px",
            DTOutput(session$ns("network_summary_table"))
          )
        ),
        
        # Treatment Effects Card
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(class = "bg-primary", "Treatment Effects vs Reference"),
          bslib::card_body(
            padding = "15px",
            uiOutput(session$ns("reference_info")),
            DTOutput(session$ns("treatment_effects_table"))
          )
        ),
        
        # Heterogeneity Statistics Card
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(class = "bg-primary", "Heterogeneity Statistics"),
          bslib::card_body(
            padding = "15px",
            DTOutput(session$ns("heterogeneity_table"))
          )
        ),
        
        # Model Tests Card
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(class = "bg-primary", "Model Tests (Q-Statistics)"),
          bslib::card_body(
            padding = "15px",
            DTOutput(session$ns("model_tests_table"))
          )
        ),
        
        # league table toggle card
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "bg-primary",
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              "League Table",
              # Toggle buttons for format
              div(
                style = "display: flex; gap: 8px;",
                actionButton(
                  session$ns("league_format_matrix"),
                  "Matrix",
                  class = "btn-sm",
                  style = "background: #457b9d; color: white; border: none; font-weight: 600;"
                ),
                actionButton(
                  session$ns("league_format_long"),
                  "Long Format",
                  class = "btn-sm",
                  style = "background: #B0B0B0; color: white; border: none; font-weight: 600;"
                )
              )
            )
          ),
          bslib::card_body(
            padding = "15px",
            div(
              style = "margin-bottom: 8px; padding: 6px 10px; background: #E8F4F8; border-radius: 4px;",
              tags$small(
                icon("info-circle", style = "color: #457b9d;"),
                " To download the league table, please switch the format to ",
                tags$strong("Long Format"),
                " using the toggle above.",
                style = "color: #1d3557;"
              )
            ),
            DTOutput(session$ns("league_table"))
          )
        ),
        
        # Netsplit Card
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(class = "bg-primary", "Direct vs Indirect Evidence (Netsplit)"),
          bslib::card_body(
            padding = "15px",
            div(
              style = "margin-bottom: 15px; padding: 10px; background: #E8F4F8; border-radius: 6px;",
              tags$small(
                icon("info-circle", style = "color: #457b9d;"),
                " Comparison of direct and indirect evidence. Significant p-values indicate inconsistency.",
                style = "color: #1d3557;"
              )
            ),
            DTOutput(session$ns("netsplit_table"))
          )
        )
      )
    })
    
    # ========================================================================
    # LEAGUE TABLE FORMAT TOGGLE OBSERVERS (NEW)
    # ========================================================================
    
    observeEvent(input$league_format_matrix, {
      league_format("matrix")
      # Update button styles
      shinyjs::runjs("
        $('#" %+% session$ns("league_format_matrix") %+% "').css({
          'background': '#457b9d',
          'color': 'white'
        });
        $('#" %+% session$ns("league_format_long") %+% "').css({
          'background': '#B0B0B0',
          'color': 'white'
        });
      ")
    })
    
    observeEvent(input$league_format_long, {
      league_format("long")
      # Update button styles
      shinyjs::runjs("
        $('#" %+% session$ns("league_format_long") %+% "').css({
          'background': '#457b9d',
          'color': 'white'
        });
        $('#" %+% session$ns("league_format_matrix") %+% "').css({
          'background': '#B0B0B0',
          'color': 'white'
        });
      ")
    })
    
    # ========================================================================
    # RENDER DATA TABLES
    # ========================================================================
    
    # Network Summary Table
    output$network_summary_table <- renderDT({
      results <- req(analysis_results())
      datatable(
        results$results$network_info,
        options = list(
          dom = 't', ordering = FALSE, scrollX = TRUE,
          paging = FALSE, info = FALSE, searching = FALSE,
          columnDefs = list(list(className = "dt-center", targets = "_all")),
          fnInitComplete = htmlwidgets::JS(
            "function() { $(this.api().table().body()).css({'font-size': '12px'}); }"
          )
        ),
        rownames = FALSE,
        class = "display compact stripe hover",
        style = "bootstrap5",
        escape = FALSE
      )
    })
    
    # Reference treatment info
    output$reference_info <- renderUI({
      results <- req(analysis_results())
      ref <- results$results$reference_group
      div(
        style = "background: #E8F4F8; padding: 12px; border-radius: 6px; border-left: 4px solid #457b9d; margin-bottom: 15px;",
        tags$small(
          icon("info-circle", style = "color: #457b9d;"),
          " Reference treatment: ", strong(ref),
          style = "color: #1d3557;"
        )
      )
    })
    
    # Treatment Effects Table
    output$treatment_effects_table <- renderDT({
      results <- req(analysis_results())
      effects_df <- results$results$treatment_estimates
      
      effects_display <- data.frame(
        Treatment = effects_df$treatment,
        Estimate = sprintf("%.3f", effects_df$estimate),
        `95% CI` = sprintf("[%.3f, %.3f]", effects_df$lower_ci, effects_df$upper_ci),
        `Z-value` = sprintf("%.3f", effects_df$z_value),
        `P-value` = ifelse(
          effects_df$p_value < 0.001,
          "< 0.001",
          sprintf("%.4f", effects_df$p_value)
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
      datatable(
        effects_display,
        options = list(
          dom = 't', ordering = FALSE, scrollX = TRUE,
          paging = FALSE, info = FALSE, searching = FALSE,
          columnDefs = list(list(className = "dt-center", targets = "_all")),
          fnInitComplete = htmlwidgets::JS(
            "function() { $(this.api().table().body()).css({'font-size': '11px'}); }"
          )
        ),
        rownames = FALSE,
        class = "display compact stripe hover",
        style = "bootstrap5",
        escape = FALSE
      )
    })
    
    # Heterogeneity Table
    output$heterogeneity_table <- renderDT({
      results <- req(analysis_results())
      het_df <- results$results$heterogeneity
      
      het_display <- data.frame(
        Statistic = het_df$statistic,
        Value = sprintf("%.4f", het_df$value),
        stringsAsFactors = FALSE
      )
      
      datatable(
        het_display,
        options = list(
          dom = 't', ordering = FALSE, scrollX = TRUE,
          paging = FALSE, info = FALSE, searching = FALSE,
          columnDefs = list(list(className = "dt-center", targets = "_all")),
          fnInitComplete = htmlwidgets::JS(
            "function() { $(this.api().table().body()).css({'font-size': '11px'}); }"
          )
        ),
        rownames = FALSE,
        class = "display compact stripe hover",
        style = "bootstrap5",
        escape = FALSE
      )
    })
    
    # Model Tests Table
    output$model_tests_table <- renderDT({
      results <- req(analysis_results())
      tests_df <- results$results$model_tests
      
      tests_display <- data.frame(
        Test = tests_df$test,
        `Q Statistic` = sprintf("%.2f", tests_df$q_statistic),
        df = as.integer(tests_df$df),
        `P-value` = ifelse(
          tests_df$p_value < 0.001,
          "< 0.001",
          sprintf("%.4f", tests_df$p_value)
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
      datatable(
        tests_display,
        options = list(
          dom = 't', ordering = FALSE, scrollX = TRUE,
          paging = FALSE, info = FALSE, searching = FALSE,
          columnDefs = list(list(className = "dt-center", targets = "_all")),
          fnInitComplete = htmlwidgets::JS(
            "function() { $(this.api().table().body()).css({'font-size': '11px'}); }"
          )
        ),
        rownames = FALSE,
        class = "display compact stripe hover",
        style = "bootstrap5",
        escape = FALSE
      )
    })
    
    # League Table Toggle
    output$league_table <- renderDT({
      results <- req(analysis_results())
      format_selected <- league_format() # ← Use reactive value
      
      if (format_selected == "matrix") {
        # Show matrix format
        league_data <- results$league$matrix
      } else {
        # Show long format
        league_data <- results$league$long
      }
      
      datatable(
        league_data,
        options = list(
          dom = 'frtip',
          ordering = TRUE,
          scrollX = TRUE,
          paging = TRUE,
          pageLength = 10,
          info = TRUE,
          searching = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
          ),
          # smaller font
          fnInitComplete = htmlwidgets::JS(
            "function() { $(this.api().table().body()).css({'font-size': '11px'}); }"
          )
        ),
        rownames = FALSE,
        class = "display compact stripe hover",
        style = "bootstrap5",
        escape = FALSE
      )
      
    })
    
    # Netsplit Table
    output$netsplit_table <- renderDT({
      results <- req(analysis_results())
      netsplit_df <- results$results$netsplit
      
      netsplit_display <- data.frame(
        Comparison = netsplit_df$comparison,
        k = netsplit_df$k,
        `Prop. Direct` = sprintf("%.3f", netsplit_df$prop_direct),
        NMA = sprintf("%.3f", netsplit_df$nma),
        Direct = ifelse(is.na(netsplit_df$direct), "—", sprintf("%.3f", netsplit_df$direct)),
        Indirect = ifelse(is.na(netsplit_df$indirect), "—", sprintf("%.3f", netsplit_df$indirect)),
        Diff = ifelse(is.na(netsplit_df$diff), "—", sprintf("%.3f", netsplit_df$diff)),
        z = ifelse(is.na(netsplit_df$z), "—", sprintf("%.3f", netsplit_df$z)),
        `P-value` = ifelse(
          is.na(netsplit_df$p_value), "—",
          ifelse(netsplit_df$p_value < 0.001, "< 0.001", sprintf("%.4f", netsplit_df$p_value))
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
      datatable(
        netsplit_display,
        options = list(
          dom = 'frtip', ordering = TRUE, scrollX = TRUE,
          paging = TRUE, pageLength = 10, info = TRUE, searching = TRUE,
          columnDefs = list(list(className = "dt-center", targets = "_all")),
          fnInitComplete = htmlwidgets::JS(
            "function() { $(this.api().table().body()).css({'font-size': '11px'}); }"
          )
        ),
        rownames = FALSE,
        class = "display compact stripe hover",
        style = "bootstrap5",
        escape = FALSE
      )
      
      
    })
    
    # ========================================================================
    # DOWNLOAD HANDLER - Export All Tables to Word Document (UPDATED)
    # ========================================================================
    
    output$download_results_word <- downloadHandler(
      filename = function() {
        paste0("nma_results_", Sys.Date(), ".docx")
      },
      content = function(file) {
        results <- req(analysis_results())
        
        # Create a new Word document
        doc <- read_docx()
        
        # Add title and metadata
        doc <- doc %>%
          body_add_par("Network Meta-Analysis Results", style = "heading 1") %>%
          body_add_par(paste("Generated on:", Sys.Date()), style = "Normal") %>%
          body_add_par("", style = "Normal")
        
        # ============================================================
        # Network Summary (Portrait)
        # ============================================================
        
        doc <- doc %>%
          body_add_par("Network Summary", style = "heading 2")
        
        ft_network <- flextable(results$results$network_info) %>%
          theme_vanilla() %>%
          autofit() %>%
          align(align = "center", part = "all") %>%
          fontsize(size = 10, part = "all")
        
        doc <- doc %>%
          body_add_flextable(ft_network) %>%
          body_add_par("", style = "Normal")
        
        # ============================================================
        # Treatment Effects (Portrait)
        # ============================================================
        
        doc <- doc %>%
          body_add_par("Treatment Effects vs Reference", style = "heading 2")
        
        effects_df <- results$results$treatment_estimates
        effects_export <- data.frame(
          Treatment = effects_df$treatment,
          Estimate = sprintf("%.3f", effects_df$estimate),
          `95% CI` = sprintf("[%.3f, %.3f]", effects_df$lower_ci, effects_df$upper_ci),
          `Z-value` = sprintf("%.3f", effects_df$z_value),
          `P-value` = ifelse(
            effects_df$p_value < 0.001,
            "< 0.001",
            sprintf("%.4f", effects_df$p_value)
          ),
          check.names = FALSE
        )
        
        ft_effects <- flextable(effects_export) %>%
          theme_vanilla() %>%
          autofit() %>%
          align(align = "center", part = "all") %>%
          fontsize(size = 10, part = "all")
        
        doc <- doc %>%
          body_add_flextable(ft_effects) %>%
          body_add_par("", style = "Normal")
        
        # ============================================================
        # Heterogeneity (Portrait)
        # ============================================================
        
        doc <- doc %>%
          body_add_par("Heterogeneity Statistics", style = "heading 2")
        
        het_df <- results$results$heterogeneity
        het_export <- data.frame(
          Statistic = het_df$statistic,
          Value = sprintf("%.4f", het_df$value)
        )
        
        ft_het <- flextable(het_export) %>%
          theme_vanilla() %>%
          autofit() %>%
          align(align = "center", part = "all") %>%
          fontsize(size = 10, part = "all")
        
        doc <- doc %>%
          body_add_flextable(ft_het) %>%
          body_add_par("", style = "Normal")
        
        # ============================================================
        # Model Tests (Portrait)
        # ============================================================
        
        doc <- doc %>%
          body_add_par("Model Tests", style = "heading 2")
        
        tests_df <- results$results$model_tests
        tests_export <- data.frame(
          Test = tests_df$test,
          `Q Statistic` = sprintf("%.2f", tests_df$q_statistic),
          df = as.integer(tests_df$df),
          `P-value` = ifelse(
            tests_df$p_value < 0.001,
            "< 0.001",
            sprintf("%.4f", tests_df$p_value)
          ),
          check.names = FALSE
        )
        
        ft_tests <- flextable(tests_export) %>%
          theme_vanilla() %>%
          autofit() %>%
          align(align = "center", part = "all") %>%
          fontsize(size = 10, part = "all")
        
        doc <- doc %>%
          body_add_flextable(ft_tests) %>%
          body_add_par("", style = "Normal")
        
        # ============================================================
        # LANDSCAPE SECTION FOR LEAGUE TABLE
        # ============================================================
        
        # End portrait section and start landscape
        doc <- doc %>%
          body_end_section_portrait()  # ← End current portrait section
        
        # Add page break with landscape orientation
        doc <- doc %>%
          body_add_par("League Table", style = "heading 2")
        
        # Get the format that's currently selected in the app
        # For word document, use the current reactive value
        selected_format <- league_format()
        
        if (selected_format == "matrix") {
          league_data <- results$league$matrix
        } else {
          league_data <- results$league$long
        }
        
        ft_league <- flextable(league_data) %>%
          theme_vanilla() %>%
          autofit() %>%
          align(align = "center", part = "all") %>%
          fontsize(size = 9, part = "all")
        
        doc <- doc %>%
          body_add_flextable(ft_league) %>%
          body_end_section_landscape()  # ← End this section in landscape
        
        # ============================================================
        # Netsplit (Back to Portrait)
        # ============================================================
        
        doc <- doc %>%
          body_add_par("Direct vs Indirect Evidence (Netsplit)", style = "heading 2")
        
        netsplit_df <- results$results$netsplit
        netsplit_export <- data.frame(
          Comparison = netsplit_df$comparison,
          k = netsplit_df$k,
          `Prop. Direct` = sprintf("%.3f", netsplit_df$prop_direct),
          NMA = sprintf("%.3f", netsplit_df$nma),
          Direct = ifelse(is.na(netsplit_df$direct), "—", sprintf("%.3f", netsplit_df$direct)),
          Indirect = ifelse(is.na(netsplit_df$indirect), "—", sprintf("%.3f", netsplit_df$indirect)),
          Diff = ifelse(is.na(netsplit_df$diff), "—", sprintf("%.3f", netsplit_df$diff)),
          z = ifelse(is.na(netsplit_df$z), "—", sprintf("%.3f", netsplit_df$z)),
          `P-value` = ifelse(
            is.na(netsplit_df$p_value), "—",
            ifelse(netsplit_df$p_value < 0.001, "< 0.001", sprintf("%.4f", netsplit_df$p_value))
          ),
          check.names = FALSE
        )
        
        ft_netsplit <- flextable(netsplit_export) %>%
          theme_vanilla() %>%
          autofit() %>%
          align(align = "center", part = "all") %>%
          fontsize(size = 9, part = "all")
        
        doc <- doc %>%
          body_add_flextable(ft_netsplit)
        
        # Save the document
        print(doc, target = file)
        
        showNotification(
          "Results exported successfully!",
          type = "message",
          duration = 3
        )
      }
    )
  })
}
