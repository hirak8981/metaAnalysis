#===============================================================================
# MODULE: NMA Ranking & Inconsistency - FIXED TO USE SETTINGS
#===============================================================================

nmaRankingInconsistencyUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "margin-bottom: 30px;",
        h3("Ranking & Inconsistency Analysis", style = "color: #1d3557;")),
    
    div(style = "margin-bottom: 40px;",
        plotDisplayUI(ns("inconsistency"), "Design Inconsistency Analysis")),
    
    div(style = "margin-bottom: 40px;",
        plotDisplayUI(ns("radialranking"), "Radial Rank Heat Plot")),
    
    div(style = "margin-bottom: 40px;",
        plotDisplayUI(ns("beading"), "SUCRA Beading Plot")),
    
    div(style = "margin-bottom: 40px;",
        plotDisplayUI(ns("stacked"), "Ranking Probabilities"))
  )
}

nmaRankingInconsistencyServer <- function(id, analysisresults, 
                                          plotsettings = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # ===================================================================
    # SHARED DATA (computed once, cached, reused everywhere)
    # ===================================================================
    
    # Rankogram data (for radial & stacked plots)
    shared_rankogram_data <- reactive({
      req(analysisresults())
      
      # Use pre-computed rankogram
      if (!is.null(analysisresults()$rankogram_data)) {
        message("✓ Using pre-computed rankogram data")
        return(analysisresults()$rankogram_data)
      }
      
      # Fallback
      message("⚠ Rankogram data not pre-computed, computing now...")
      set.seed(123)
      netmeta::rankogram(analysisresults()$nma, nsim = 10000, random = TRUE)
    }) %>% bindCache(
      analysisresults()$nma
    )
    
    # Ranking metrics (for beading plot)
    shared_ranking_metrics <- reactive({
      req(analysisresults())
      
      # Use pre-computed ranking metrics
      if (!is.null(analysisresults()$ranking_metrics)) {
        message("✓ Using pre-computed ranking metrics")
        return(analysisresults()$ranking_metrics)
      }
      
      # Fallback
      message("⚠ Ranking metrics not pre-computed, computing now...")
      effect_measure <- analysisresults()$settings$effect_measure
      prefer <- if (effect_measure %in% c("OR", "RR", "HR")) "small" else "small"
      
      prepare_ranking_data(
        nma_obj = analysisresults()$nma,
        outcome_name = effect_measure,
        prefer = prefer,
        metrics_type = "SUCRA",
        model = analysisresults()$settings$model_type,
        nsim = 10000
      )
    }) %>% bindCache(
      analysisresults()$nma,
      analysisresults()$settings$effect_measure,
      analysisresults()$settings$model_type
    )
    
    # Design decomposition (for netheat plots)
    shared_decomp_data <- reactive({
      req(analysisresults())
      
      # Use pre-computed from analysis
      if (!is.null(analysisresults()$design_decomp)) {
        message("✓ Using pre-computed design decomposition")
        return(analysisresults()$design_decomp)
      }
      
      # Fallback
      message("⚠ Design decomposition not pre-computed, computing now...")
      netmeta::decomp.design(analysisresults()$nma)
    }) %>% bindCache(
      analysisresults()$nma
    )
    
    # =========================================================================
    # Inconsistency Plot (unchanged)
    # =========================================================================
    inconsistency_function <- function(data, settings) {
      custom <- if (!is.null(settings$custom)) settings$custom else list(
        netheat_title = "Net Heat Matrix - Design Inconsistency",
        netheat_gradient_low = "#CAF0F8",
        netheat_gradient_mid = "#7493A8",
        netheat_gradient_high = "#1D3357",
        netheat_bar_sig_color = "#E8A87C",
        netheat_bar_nonsig_color = "#7CB5BD"
      )
      
      p_netheat <- plot_netheat_matrix(
        data$nma,
        precomputed_decomp = data$decomp,
        gradient_colors = c(custom$netheat_gradient_low, 
                            custom$netheat_gradient_mid,
                            custom$netheat_gradient_high),
        title = custom$netheat_title
      )
      
      p_qbar <- plot_design_q_barplot(
        data$nma,
        precomputed_decomp = data$decomp,
        sig_color = custom$netheat_bar_sig_color,
        nonsig_color = custom$netheat_bar_nonsig_color
      )
      
      p_combined <- p_netheat + p_qbar + patchwork::plot_layout(widths = c(2, 2))
      return(p_combined)
    }
    
    inconsistency_data <- reactive({
      req(shared_decomp_data())
      list(
        nma = analysisresults()$nma,
        decomp = shared_decomp_data()
      )
    })
    
    inconsistency_settings <- reactive({
      req(analysisresults())
      
      custom_settings <- if (!is.null(plotsettings)) {
        plotsettings()
      } else {
        NULL
      }
      
      list(
        width = 20,
        height = 12,
        custom = custom_settings
      )
    })
    
    plotDisplayServer("inconsistency", inconsistency_function, 
                      inconsistency_data, inconsistency_settings)
    
    # =========================================================================
    # Radial Rank Plot - FIXED TO USE RANKOGRAM
    # =========================================================================
    radial_function <- function(data, settings) {
      custom <- if (!is.null(settings$custom)) settings$custom else list(
        radial_gradient_low = "#CAF0F8",
        radial_gradient_mid = "#7493A8",
        radial_gradient_high = "#1D3357",
        radial_gradient_midpoint = 50
      )
      
      p_radial_main <- plot_radial_ranks(
        data$nma,
        precomputed_ranking = data$rankogram,  
        legend_position = "none",
        gradient_colors = c(custom$radial_gradient_low,
                            custom$radial_gradient_mid,
                            custom$radial_gradient_high),
        gradient_midpoint = custom$radial_gradient_midpoint
      )
      
      p_radial_legend <- create_rank_legend_panel(
        data$nma,
        gradient_colors = c(custom$radial_gradient_low,
                            custom$radial_gradient_mid,
                            custom$radial_gradient_high),
        section_spacing = 2.5
      )
      
      p_combined <- p_radial_main + p_radial_legend + 
        patchwork::plot_layout(widths = c(2.5, 1))
      return(p_combined)
    }
    
    radial_data <- reactive({
      req(shared_rankogram_data())  
      list(
        nma = analysisresults()$nma,
        rankogram = shared_rankogram_data()  
      )
    })
    
    radial_settings <- reactive({
      req(analysisresults())
      
      custom_settings <- if (!is.null(plotsettings)) {
        plotsettings()
      } else {
        NULL
      }
      
      list(
        width = 14,
        height = 10,
        custom = custom_settings
      )
    })
    
    plotDisplayServer("radialranking", radial_function, radial_data, radial_settings)
    
    # =========================================================================
    # Beading Plot - USES RANKING METRICS (rankinma object)
    # =========================================================================
    beading_function <- function(data, settings) {
      custom <- if (!is.null(settings$custom)) {
        settings$custom
      } else {
        list(
          beading_color_palette = "default",
          beading_layout = "vertical"
        )
      }
      
      p_beading <- plot_beading(
        data$ranking_metrics,  
        nma_obj = data$nma,
        color_palette = custom$beading_color_palette,
        layout = custom$beading_layout,
        show_labels = TRUE
      )
      
      return(p_beading)
    }
    
    beading_data <- reactive({
      req(shared_ranking_metrics())  
      list(
        nma = analysisresults()$nma,
        ranking_metrics = shared_ranking_metrics()  
      )
    })
    
    beading_settings <- reactive({
      req(analysisresults())
      
      custom_settings <- if (!is.null(plotsettings)) {
        plotsettings()
      } else {
        NULL
      }
      
      list(
        width = 10,
        height = max(8, length(analysisresults()$treatments) * 0.6),
        custom = custom_settings
      )
    })
    
    plotDisplayServer("beading", beading_function, beading_data, beading_settings)
    
    # =========================================================================
    # Stacked Bar Plot - FIXED TO USE RANKOGRAM
    # =========================================================================
    stacked_function <- function(data, settings) {
      custom <- if (!is.null(settings$custom)) {
        settings$custom
      } else {
        list(
          stacked_title = "Ranking Probabilities",
          stacked_layout = "stacked",
          stacked_rank_palette = "colorblind",
          stacked_treatment_palette = "default"
        )
      }
      
      p_stacked <- plot_stacked_bars(
        data$nma,
        data$ranking_metrics,  
        precomputed_ranking = data$rankogram,  
        layout = custom$stacked_layout,
        title = custom$stacked_title,
        rank_palette = custom$stacked_rank_palette,
        treatment_palette = custom$stacked_treatment_palette,
        legend_position = "right"
      )
      
      return(p_stacked)
    }
    
    stacked_data <- reactive({
      req(shared_rankogram_data())  
      req(shared_ranking_metrics())
      list(
        nma = analysisresults()$nma,
        rankogram = shared_rankogram_data(),  
        ranking_metrics = shared_ranking_metrics()  
      )
    })
    
    stacked_settings <- reactive({
      req(analysisresults())
      
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
    
    plotDisplayServer("stacked", stacked_function, stacked_data, stacked_settings)
  })
}




