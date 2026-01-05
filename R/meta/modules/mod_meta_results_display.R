# =============================================================================
# Meta-Analysis Results Display Module
# =============================================================================



metaResultsDisplayUI <- function(id) {
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

metaResultsDisplayServer <- function(id, analysis_results) {
  moduleServer(id, function(input, output, session) {
    
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
        # Summary Statistics
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "bg-primary",
            "Summary Statistics"
          ),
          bslib::card_body(
            padding = "15px",  
            DTOutput(session$ns("summary_table"))
          )
        ),
        
        # Heterogeneity Statistics
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "bg-primary",
            "Heterogeneity Statistics"
          ),
          bslib::card_body(
            padding = "15px",  
            DTOutput(session$ns("heterogeneity_table"))
          )
        ),
        
        # Publication Bias Tests
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "bg-primary",
            "Publication Bias Tests"
          ),
          bslib::card_body(
            padding = "15px",  
            div(
              style = "font-size: 13px; overflow-x: auto;",
              DTOutput(session$ns("pubias_table"))
            )
          )
        )
      )
    })
    
    # ========================================================================
    # RENDER DATA TABLES
    # ========================================================================
    
    # Summary Table
    output$summary_table <- renderDT({
      req(analysis_results())
      datatable(
        analysis_results()$summary,
        options = list(
          dom = 't',
          ordering = FALSE,
          scrollX = TRUE,
          paging = FALSE,
          info = FALSE,
          searching = FALSE,
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
          ),
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
    
    # Heterogeneity Table
    output$heterogeneity_table <- renderDT({
      req(analysis_results())
      datatable(
        analysis_results()$heterogeneity,
        options = list(
          dom = 't',
          ordering = FALSE,
          scrollX = TRUE,
          paging = FALSE,
          info = FALSE,
          searching = FALSE,
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
          ),
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
    
    # Publication Bias Table
    output$pubias_table <- renderDT({
      req(analysis_results())
      
      
      is_subgroup <- !is.null(analysis_results()$model$mods) && 
        ncol(analysis_results()$model$mods) > 0
      
      # Generate publication bias summary
      pubias_summary <- tryCatch({
        result <- tidy_pubias_summary(analysis_results()$model)
        
        # ✅ If empty dataframe (common in subgroup analysis), return custom message
        if (is.null(result) || nrow(result) == 0) {
          if (is_subgroup) {
            data.frame(
              MESSAGE = "Publication bias tests are currently not available for subgroup/meta-regression analyses.",
              REASON = "These tests require individual study-level effect sizes without moderator variables."
            )
          } else {
            data.frame(
              MESSAGE = "Publication bias tests could not be computed.",
              REASON = "Insufficient data or model type not supported for these tests."
            )
          }
        } else {
          result
        }
      }, error = function(e) {
        if (is_subgroup) {
          data.frame(
            MESSAGE = "Publication bias tests are currently not available for subgroup/meta-regression analyses.",
            REASON = "These tests are designed for standard meta-analyses without moderator variables."
          )
        } else {
          data.frame(
            MESSAGE = "Publication bias tests could not be computed.",
            REASON = paste("Note:", e$message)
          )
        }
      })
      
      
      is_message_df <- "MESSAGE" %in% names(pubias_summary)
      
      
      column_defs <- if (is_message_df) {
        list(
          list(className = "dt-left", targets = "_all"),  
          list(width = "40%", targets = 0),  
          list(width = "60%", targets = 1)   
        )
      } else {
        list(
          list(className = "dt-center", targets = "_all"),
          list(className = "dt-left", targets = 6), 
          list(width = "120px", targets = 0),  # Test
          list(width = "90px", targets = 1),   # Statistic
          list(width = "80px", targets = 2),   # P-value
          list(width = "140px", targets = 3),  # Estimate
          list(width = "120px", targets = 4),  # Result
          list(visible = FALSE, targets = 5),  # Hide Significant column
          list(width = "280px", targets = 6)   # Conclusion
        )
      }
      
      datatable(
        pubias_summary,
        options = list(
          dom = 't',
          ordering = FALSE,
          scrollX = TRUE,
          paging = FALSE,
          info = FALSE, 
          searching = FALSE,
          autoWidth = FALSE,
          columnDefs = column_defs
        ),
        rownames = FALSE,
        class = if (is_message_df) {
          "display compact"  # Remove hover/stripe for message rows
        } else {
          "display compact stripe hover"
        },
        style = "bootstrap5",
        escape = FALSE  
      ) %>% 
        
        {
          if (is_message_df) {
            formatStyle(
              .,
              columns = names(pubias_summary),
              backgroundColor = '#FFF3CD',  # Light yellow background
              color = '#856404',  # Dark yellow/brown text
              fontStyle = 'italic',
              fontSize = '14px',
              padding = '15px'
            )
          } else {
            .  # Return unchanged if not a message
          }
        }
    })
    
    # ========================================================================
    # DOWNLOAD HANDLER - Export All Tables to Word Document
    # ========================================================================
    output$download_results_word <- downloadHandler(
      filename = function() {
        paste0("meta_analysis_results_", Sys.Date(), ".docx")
      },
      content = function(file) {
        req(analysis_results())
        
        # Create a new Word document
        doc <- read_docx()
        
        # Add title and metadata (Portrait)
        doc <- doc %>%
          body_add_par("Meta-Analysis Results", style = "heading 1") %>%
          body_add_par(paste("Generated on:", Sys.Date()), style = "Normal") %>%
          body_add_par("", style = "Normal")
        
        # ============================================================
        # SUMMARY STATISTICS (Portrait)
        # ============================================================
        doc <- doc %>%
          body_add_par("Summary Statistics", style = "heading 2")
        
        ft_summary <- flextable(analysis_results()$summary) %>%
          theme_vanilla() %>%
          autofit() %>%
          align(align = "center", part = "all") %>%
          fontsize(size = 10, part = "all")
        
        doc <- doc %>% 
          body_add_flextable(ft_summary) %>%
          body_add_par("", style = "Normal")
        
        # ============================================================
        # HETEROGENEITY STATISTICS (Portrait)
        # ============================================================
        doc <- doc %>%
          body_add_par("Heterogeneity Statistics", style = "heading 2")
        
        ft_het <- flextable(analysis_results()$heterogeneity) %>%
          theme_vanilla() %>%
          autofit() %>%
          align(align = "center", part = "all") %>%
          fontsize(size = 10, part = "all")
        
        doc <- doc %>% 
          body_add_flextable(ft_het) %>%
          body_add_par("", style = "Normal")
        
        # ============================================================
        # LANDSCAPE SECTION FOR PUBLICATION BIAS
        # ============================================================
        doc <- doc %>%
          body_end_section_portrait() %>%  # End current portrait section
          body_add_par("Publication Bias Tests", style = "heading 2")
        
        pubias_summary <- tryCatch({
          pb_data <- tidy_pubias_summary(analysis_results()$model)
          
          # Strip HTML <br> tags and replace with line breaks
          if ("Conclusion" %in% names(pb_data)) {
            pb_data$Conclusion <- gsub("<br>", "\n", pb_data$Conclusion)
          }
          
          # Remove "Significant" column if present
          if ("Significant" %in% names(pb_data)) {
            pb_data$Significant <- NULL
          }
          
          pb_data
        }, error = function(e) {
          data.frame(
            Message = "Publication bias tests could not be computed",
            Reason = e$message
          )
        })
        
        ft_pubias <- flextable(pubias_summary) %>%
          theme_vanilla() %>%
          align(align = "center", part = "all") %>%
          align(j = "Conclusion", align = "left", part = "body") %>%
          fontsize(size = 8, part = "all") %>%
          width(j = "Test", width = 2.0) %>%
          width(j = "Statistic", width = 1.0) %>%
          width(j = "P-value", width = 0.9) %>%
          width(j = "Estimate [95% CI]", width = 1.6) %>%
          width(j = "Result", width = 1.4) %>%
          width(j = "Conclusion", width = 4.0) %>%
          valign(valign = "top", part = "all") %>%
          padding(padding = 4, part = "all")
        
        doc <- doc %>% 
          body_add_flextable(ft_pubias) %>%
          body_end_section_landscape()  
        
        # Save the document
        print(doc, target = file)
        
        # Show notification
        showNotification(
          "Results exported successfully!",
          type = "message",
          duration = 3
        )
      }
    )
    
    
  })
}
