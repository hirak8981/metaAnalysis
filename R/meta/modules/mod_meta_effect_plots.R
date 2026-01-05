
# =============================================================================
# Meta-Analysis Effect Plots Module (Forest, Funnel, Sunset)
# =============================================================================

metaEffectPlotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-bottom: 30px;",
      h3("Effect Size Visualizations", style = "color: #1d3557;")
    ),
    
    # Forest Plot
    div(
      style = "margin-bottom: 40px;",
      plotDisplayUI(ns("forest_plot"), "Forest Plot")
    ),
    
    # Funnel Plot
    div(
      style = "margin-bottom: 40px;",
      plotDisplayUI(ns("funnel_plot"), "Funnel Plot")
    ),
    
    # Sunset Plot
    div(
      style = "margin-bottom: 40px;",
      plotDisplayUI(ns("sunset_plot"), "Sunset Plot (Power-Enhanced Funnel)")
    )
  )
}

metaEffectPlotsServer <- function(id, analysis_results, current_data, model_settings, plot_settings) {
  moduleServer(id, function(input, output, session) {
    
    # ========================================================================
    # FOREST PLOT
    # ========================================================================
    forest_function <- function(data, settings) {
      forest_custom(
        x = data$model,
        study_labels = "study",
        data = data$data_df,
        point_color = settings$forest_point_color,
        point_size = settings$forest_point_size,
        summary_color = settings$forest_summary_color,
        ci_color = settings$forest_ci_color,
        text_size = settings$forest_text_size,
        x_trans_function = settings$x_trans_function,  
        annotate_CI = TRUE,
        theme_fun = theme_minimal(),
        plot_title = settings$forest_title,
        show_heterogeneity = TRUE,
        het_position = "subtitle",
        het_digits = 2
      )
    }
    
    forest_data <- reactive({
      req(analysis_results())
      req(current_data())
      list(
        model = analysis_results()$model,
        data_df = current_data()
      )
    }) %>% bindCache(
      analysis_results()$model$call, 
      nrow(current_data())             
    )
    
    forest_settings <- reactive({
      req(plot_settings())
      req(model_settings())
      list(
        forest_point_color = plot_settings()$forest_point_color,
        forest_point_size = plot_settings()$forest_point_size,
        forest_summary_color = plot_settings()$forest_summary_color,
        forest_ci_color = plot_settings()$forest_ci_color,
        forest_text_size = plot_settings()$forest_text_size,
        forest_title = plot_settings()$forest_title,
        x_trans_function = model_settings()$transform,  
        width = 12,
        height = max(8, nrow(current_data()) * 0.4)
      )
    })
    
    plotDisplayServer("forest_plot", forest_function, forest_data, forest_settings)
    
    # ========================================================================
    # FUNNEL PLOT
    # ========================================================================
    funnel_function <- function(data, settings) {
      # Check if model has subgroup/moderator
      has_subgroup <- !is.null(data$model$mods) || length(data$model$b) > 1
      
      if (has_subgroup) {
        # Subgroup mode: use group legend and colors
        funnel_custom(
          x = data$model,
          y_axis = "se",
          contours = TRUE,
          contours_col = settings$funnel_contours_col,
          sig_contours = TRUE,
          addev_contours = FALSE,
          egger = TRUE,
          trim_and_fill = FALSE,
          point_size = settings$funnel_point_size,
          plot_title = settings$funnel_title,
          group_legend = TRUE,
          group_colors = settings$funnel_group_colors,
          x_trans_function = settings$x_trans_function  
        )
      } else {
        # No subgroup: standard funnel plot
        funnel_custom(
          x = data$model,
          y_axis = "se",
          contours = TRUE,
          contours_col = settings$funnel_contours_col,
          sig_contours = TRUE,
          addev_contours = FALSE,
          egger = TRUE,
          trim_and_fill = TRUE,
          trim_and_fill_side = "left",
          point_color = settings$funnel_point_color,
          point_size = settings$funnel_point_size,
          plot_title = settings$funnel_title,
          x_trans_function = settings$x_trans_function  
        )
      }
    }
    
    funnel_data <- reactive({
      req(analysis_results())
      list(model = analysis_results()$model)
    }) %>% bindCache(
      analysis_results()$model$call  # Cache key: model specification
    )
    
    
    funnel_settings <- reactive({
      req(plot_settings())
      req(model_settings())
      list(
        funnel_point_color = plot_settings()$funnel_point_color,
        funnel_point_size = plot_settings()$funnel_point_size,
        funnel_contours_col = plot_settings()$funnel_contours_col,
        funnel_group_colors = plot_settings()$funnel_group_colors,
        funnel_title = plot_settings()$funnel_title,
        x_trans_function = model_settings()$transform,  
        height = 8
      )
    })
    
    plotDisplayServer("funnel_plot", funnel_function, funnel_data, funnel_settings)
    
    # ========================================================================
    # SUNSET PLOT
    # ========================================================================
    sunset_function <- function(data, settings) {
      sunset_custom(
        x = data$model,
        true_effect = settings$true_effect,
        sig_level = settings$sig_level,
        power_contours = "continuous",
        power_col = settings$sunset_power_col,
        contour_col = settings$sunset_contour_col,
        point_color = settings$sunset_point_color,
        point_size = settings$sunset_point_size,
        plot_title = settings$sunset_title,
        x_trans_function = settings$x_trans_function  
      )
    }
    
    sunset_data <- reactive({
      req(analysis_results())
      list(model = analysis_results()$model)
    }) %>% bindCache(
      analysis_results()$model$call  
    )
    
    
    sunset_settings <- reactive({
      req(plot_settings())
      req(model_settings())
      list(
        true_effect = plot_settings()$sunset_true_effect,
        sig_level = plot_settings()$sunset_sig_level,
        sunset_point_color = plot_settings()$sunset_point_color,
        sunset_point_size = plot_settings()$sunset_point_size,
        sunset_power_col = plot_settings()$sunset_power_col,
        sunset_contour_col = plot_settings()$sunset_contour_col,
        sunset_title = plot_settings()$sunset_title,
        x_trans_function = model_settings()$transform,
        width = 10,
        height = 8
      )
    })
    
    plotDisplayServer("sunset_plot", sunset_function, sunset_data, sunset_settings)
  })
}
