# Helper function to sanitize class names for input IDs
sanitize_class_name <- function(class_name) {
  class_id <- tolower(class_name)
  class_id <- gsub("[^a-z0-9]+", "_", class_id)  
  class_id <- gsub("^_+|_+$", "", class_id)      
  return(class_id)
}

#===============================================================================
# MODULE: NMA Network Plots - WITH CLASS PALETTE SUPPORT
#===============================================================================

nmaNetworkPlotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "margin-bottom: 30px;",
        h3("Network & Forest Visualizations", style = "color: #1d3557;")),
    
    div(style = "margin-bottom: 40px;",
        plotDisplayUI(ns("dualnetwork"), "Dual Network Graph")),
    
    div(style = "margin-bottom: 40px;",
        plotDisplayUI(ns("forest"), "Forest Plot")),
    
    div(style = "margin-bottom: 40px;",
        plotDisplayUI(ns("directevidence"), "Direct Evidence"))
  )
}


nmaNetworkPlotsServer <- function(id, analysisresults, plotsettings = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================================
    # Dual Network Plot - WITH CLASS PALETTE
    # =========================================================================
    
    dual_network_function <- function(data, settings) {
      
      # Use settings if available, otherwise use defaults
      custom <- if (!is.null(settings$custom)) settings$custom else list(
        network_color_palette = "default",
        network_edge_color = "grey60",
        network_node_border_color = "#333333",
        network_node_size_min = 6,
        network_node_size_max = 14,
        network_edge_width_min = 0.5,
        network_edge_width_max = 3,
        network_label_size = 4,
        network_title = "Treatment Network Graph"
      )
      
      
      class_palette <- NULL
      
      if (!is.null(data$treatment_classes) && nrow(data$treatment_classes) > 0) {
        
        unique_classes <- unique(data$treatment_classes$treatment_class)
        class_palette <- character(length(unique_classes))
        names(class_palette) <- sort(unique_classes)
        
        # Map class colors from customization settings
        for (cls in names(class_palette)) {
          # Sanitize class name for input ID
          class_id <- tolower(cls)
          class_id <- gsub("[^a-z0-9]+", "_", class_id)
          class_id <- gsub("^_+|_+$", "", class_id)
          input_id <- paste0("network_class_", class_id, "_color")
          
          cat("\n[DEBUG] Class:", cls, "→ ID:", input_id, "\n")
          cat("Available keys:", paste(names(custom)[grepl("class", names(custom))], collapse = ", "), "\n")
          
          # Check if color exists in custom settings
          if (!is.null(custom[[input_id]])) {
            class_palette[cls] <- custom[[input_id]]
            cat("✓ Found:", custom[[input_id]], "\n")
          } else {
            # Fallback colors
            if (tolower(cls) %in% c("control", "placebo")) {
              class_palette[cls] <- "#E8A87C"
            } else if (tolower(cls) == "active") {
              class_palette[cls] <- "#7CB5BD"
            } else {
              class_palette[cls] <- "#999999"
            }
            cat("✗ Not found, using fallback:", class_palette[cls], "\n")
          }
        }
        
        cat("\nFinal class_palette:\n")
        print(class_palette)
        cat("\n")
      }
      
      # Build the nma result structure
      nma_result_for_plot <- list(
        nma = data$results$nma,
        treatment_class_map = data$treatment_classes  
      )
      
      # Call plot_dual_networks with proper structure
      plot_dual_networks(
        nma_result_for_plot,  
        treatment_colors = NULL,
        color_palette = custom$network_color_palette,
        class_palette = class_palette,  
        edge_color = custom$network_edge_color,
        node_border_color = custom$network_node_border_color,
        node_size_range = c(custom$network_node_size_min, custom$network_node_size_max),
        edge_width_range = c(custom$network_edge_width_min, custom$network_edge_width_max),
        label_size = custom$network_label_size,
        title_class = custom$network_title,
        title_treatment = "Network by Treatment"
      )
    }
    
    dual_network_data <- reactive({
      req(analysisresults())
      list(
        results = analysisresults(),  # Full analysis results
        treatment_classes = analysisresults()$treatment_classes  # Extract treatment classes
      )
    }) %>% bindCache(
      analysisresults()$nma$call,  # Cache key: NMA model specification
      nrow(analysisresults()$treatment_classes)  # Number of treatment classes
    )
    
    
    dual_network_settings <- reactive({
      req(analysisresults())
      
      # Get settings from plotsettings reactive
      custom_settings <- if (!is.null(plotsettings)) {
        plotsettings()
      } else {
        NULL
      }
      
      list(
        width = 12,
        height = 8,
        custom = custom_settings
      )
    })
    
    plotDisplayServer("dualnetwork", dual_network_function, dual_network_data, dual_network_settings)
    
    
    # =========================================================================
    # Forest Plot
    # =========================================================================
    
    forest_function <- function(data, settings) {
      forest_netmeta(
        data$nma,
        model = "random",
        ref = data$reference,
        sm = data$effect_measure,
        title = paste("Forest Plot -", data$effect_measure),
        point_color = "#7CB5BD",
        ci_color = "#1D3557",
        point_size = 5,
        ci_lwd = 0.6,
        digits = 2,
        auto_detect_sm = TRUE
      )
    }
    
    forest_data <- reactive({
      req(analysisresults())
      list(
        nma = analysisresults()$nma,
        reference = analysisresults()$results$reference_group,
        effect_measure = analysisresults()$settings$effect_measure
      )
    }) %>% bindCache(
      analysisresults()$nma$call,  
      analysisresults()$results$reference_group,  
      analysisresults()$settings$effect_measure  
    )
    
    forest_settings <- reactive({
      req(analysisresults())
      list(
        width = 10,
        height = max(8, length(analysisresults()$treatments) * 0.5)
      )
    })
    
    plotDisplayServer("forest", forest_function, forest_data, forest_settings)
    
    
    # =========================================================================
    # Direct Evidence Plot
    # =========================================================================
    
    direct_evidence_function <- function(data, settings) {
      
      # Use settings if available
      custom <- if (!is.null(settings$custom)) settings$custom else list(
        direct_direct_color = "#1D3557",
        direct_indirect_color = "#E8A87C",
        direct_bar_color = "#7CB5BD",
        direct_cutoff_color = "#590d22"
      )
      
      plot_network_evidence(
        data$nma,
        direct_col = custom$direct_direct_color,
        indirect_col = custom$direct_indirect_color,
        bar_col = custom$direct_bar_color,
        cutoff_col = custom$direct_cutoff_color
      )
    }
    
    direct_evidence_data <- reactive({
      req(analysisresults())
      list(nma = analysisresults()$nma)
    }) %>% bindCache(
      analysisresults()$nma$call  
    )
    
    direct_evidence_settings <- reactive({
      req(analysisresults())
      
      # Get settings from plotsettings reactive
      custom_settings <- if (!is.null(plotsettings)) {
        plotsettings()
      } else {
        NULL
      }
      
      list(
        width = 12,
        height = 10,
        custom = custom_settings
      )
    })
    
    plotDisplayServer("directevidence", direct_evidence_function, direct_evidence_data, direct_evidence_settings)
    
  })
}
