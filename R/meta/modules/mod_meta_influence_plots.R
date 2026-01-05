# =============================================================================
# Meta-Analysis Influence Plots Module (Cook's, Studentized, LOO)
# =============================================================================

metaInfluencePlotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 30px;",
      h3("Influence & Sensitivity Analysis", style = "color: #1d3557;")
    ),
    
    # Cook's Distance - ALWAYS SHOW
    div(
      style = "margin-bottom: 40px;",
      plotDisplayUI(ns("cook_plot"), "Cook's Distance")
    ),
    
    # Studentized Residuals - ALWAYS SHOW
    div(
      style = "margin-bottom: 40px;",
      plotDisplayUI(ns("stud_plot"), "Studentized Residuals")
    ),
    
    # Leave-One-Out - CONDITIONAL
    uiOutput(ns("loo_content"))
  )
}

metaInfluencePlotsServer <- function(id, influence_results, current_data,
                                     model_settings, plot_settings) {
  moduleServer(id, function(input, output, session) {
    
    # ========================================================================
    # CONDITIONAL LOO CONTENT (shows warning or plot)
    # ========================================================================
    output$loo_content <- renderUI({
      req(current_data())
      settings <- model_settings()
      
      # Check if subgroup analysis
      if (!is.null(settings$subgroup) && settings$subgroup != "") {
        # Show warning message for subgroup
        return(
          div(
            style = "margin-bottom: 40px;",
            div(
              style = "background: #fff3cd; padding: 20px; border-radius: 8px; border-left: 4px solid #F39C12;",
              div(
                style = "display: flex; align-items: center;",
                icon("info-circle", style = "font-size: 24px; color: #F39C12; margin-right: 15px;"),
                div(
                  h5("Leave-One-Out Analysis Unavailable", style = "color: #856404; margin: 0 0 5px 0;"),
                  p("Leave-One-Out sensitivity analysis is not supported for subgroup/moderator analyses.",
                    style = "color: #856404; font-size: 14px; margin: 0;")
                )
              )
            )
          )
        )
      }
      
      # No subgroup - show LOO plot
      req(influence_results())
      div(
        style = "margin-bottom: 40px;",
        plotDisplayUI(session$ns("loo_plot"), "Leave-One-Out Sensitivity Analysis")
      )
    })
    
    # ========================================================================
    # COOK'S DISTANCE PLOT (Always renders)
    # ========================================================================
    cooks_function <- function(data, settings) {
      plot_cooks_distance(  
        influence_results = data$infl,
        color_below = settings$cook_color_below,
        color_above = settings$cook_color_above,
        threshold_line_color = settings$cook_threshold_line_color,  
        subtitle_color = settings$cook_subtitle_color,              
        custom_theme = theme_minimal(),
        title = settings$cook_title
      )
    }
    
    cooks_data <- reactive({
      req(influence_results())
      list(infl = influence_results())
    }) %>% bindCache(
      influence_results()$cook_threshold,     
      nrow(influence_results()$influence_stats) 
    )
    
    
    cooks_settings <- reactive({
      req(plot_settings())
      custom <- plot_settings()
      list(
        cook_color_below = custom$cook_color_below,
        cook_color_above = custom$cook_color_above,
        cook_threshold_line_color = custom$cook_threshold_line_color,  
        cook_subtitle_color = custom$cook_subtitle_color,              
        cook_title = custom$cook_title,
        width = 10,
        height = 8
      )
    })
    
    plotDisplayServer("cook_plot", cooks_function, cooks_data, cooks_settings)
    
    # ========================================================================
    # STUDENTIZED RESIDUALS PLOT (Always renders)
    # ========================================================================
    studentized_function <- function(data, settings) {
      plot_studentized_residuals(
        influence_results = data$infl,
        color_below = settings$stud_color_below,
        color_above = settings$stud_color_above,
        threshold_line_color = settings$stud_threshold_line_color,  
        subtitle_color = settings$stud_subtitle_color,              
        custom_theme = theme_minimal(),
        title = settings$stud_title
      )
    }
    
    studentized_data <- reactive({
      req(influence_results())
      list(infl = influence_results())
    }) %>% bindCache(
      influence_results()$studentized_threshold, 
      nrow(influence_results()$influence_stats)  
    )
    
    
    studentized_settings <- reactive({
      req(plot_settings())
      custom <- plot_settings()
      list(
        stud_color_below = custom$stud_color_below,
        stud_color_above = custom$stud_color_above,
        stud_threshold_line_color = custom$stud_threshold_line_color,  
        stud_subtitle_color = custom$stud_subtitle_color,              
        stud_title = custom$stud_title,
        width = 10,
        height = 8
      )
    })
    
    plotDisplayServer("stud_plot", studentized_function, studentized_data, studentized_settings)
    
    # ========================================================================
    # LEAVE-ONE-OUT PLOT (Only when no subgroup)
    # ========================================================================
    loo_function <- function(data, settings) {
      plot_leave_one_out(
        influence_results = data$infl,
        custom_theme = theme_minimal(),
        point_color = settings$loo_point_color,
        point_size = settings$loo_point_size,
        title = settings$loo_title
      )
    }
    
    loo_data <- reactive({
      req(influence_results())
      
      # Check if subgroup - return NULL to skip rendering
      settings <- model_settings()
      if (!is.null(settings$subgroup) && settings$subgroup != "") {
        return(NULL)
      }
      
      # OPTIMIZED: Include pre-computed LOO data
      list(
        infl = influence_results(),
        loo_precomputed = influence_results()$loo_precomputed
      )
    }) %>% bindCache(
      !is.null(influence_results()$loo_precomputed),  
      model_settings()$subgroup,                       
      nrow(influence_results()$influence_stats)        
    )
    
    
    loo_settings <- reactive({
      req(plot_settings())
      custom <- plot_settings()
      list(
        loo_point_color = custom$loo_point_color,
        loo_point_size = custom$loo_point_size,
        loo_ci_color = custom$loo_ci_color,
        loo_refline_color = custom$loo_refline_color,
        loo_title = custom$loo_title,
        width = 10,
        height = 8
      )
    })
    
    plotDisplayServer("loo_plot", loo_function, loo_data, loo_settings)
  })
}
