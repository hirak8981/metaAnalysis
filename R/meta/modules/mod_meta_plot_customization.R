# =============================================================================
# Meta-Analysis Plot Customization Module (COMPLETE)
# =============================================================================

metaPlotCustomizationUI <- function(id) {
  ns <- NS(id)
  # Modal is created in server
  tagList()
}

metaPlotCustomizationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Default settings
    # Default settings
    default_settings <- list(
      # Forest Plot
      forest_point_size = 4,
      forest_point_color = "#7CB5BD",
      forest_summary_color = "#E8A87C",
      forest_ci_color = "#1D3557",
      forest_text_size = 5,
      forest_title = "Forest Plot",
      
      # Funnel Plot
      funnel_point_color = "#1d3557",
      funnel_point_size = 5,
      funnel_contours_col = "#7CB5BD",
      funnel_group_colors = c("#1d3557", "#E8A87C"),
      funnel_title = "Funnel Plot",
      
      # Sunset Plot - FIXED: Add these two parameters
      sunset_true_effect = NULL,      # NULL means use summary effect
      sunset_sig_level = 0.05,
      sunset_point_color = "#FFA000",
      sunset_point_size = 5,
      sunset_power_col = c("#1d3557", "#334C6B", "#48647F", "#5E7B93", "#7493A8",
                           "#89AABC", "#9FC1D0", "#B4D9E4", "#CAF0F8"),
      sunset_contour_col = "#B0C4B1",
      sunset_title = "Sunset Plot (Power-Enhanced Funnel)",
      
      # Cook's Distance
      cook_color_below = "#335c67",
      cook_color_above = "#C83737",
      cook_title = "Cook's Distance",
      cook_threshold_line_color = "#1D3557",  
      cook_subtitle_color = "#1D3557",        
      
      # Studentized Residuals
      stud_color_below = "#335c67",
      stud_color_above = "#C83737",
      stud_title = "Studentized Residuals",
      stud_threshold_line_color = "#1D3557",  
      stud_subtitle_color = "#1D3557",        
      
      
      # Leave-One-Out
      loo_point_color = "#86608E",
      loo_point_size = 5,
      loo_ci_color = "#34495E",
      loo_refline_color = "#E74C3C",
      loo_title = "Leave-One-Out Sensitivity Analysis"
    )
    
    
    # Reactive settings
    settings <- reactiveVal(default_settings)
    
    # Show modal function
    show_modal <- function() {
      showModal(
        modalDialog(
          title = tagList(
            icon("palette"), 
            "Plot Customization"
          ),
          size = "xl",
          easyClose = TRUE,
          footer = tagList(
            actionButton(session$ns("reset_defaults"), "Reset to Defaults", 
                         class = "btn-warning", icon = icon("undo")),
            modalButton("Cancel"),
            actionButton(session$ns("apply_custom"), "Apply", 
                         class = "btn-primary", icon = icon("check"))
          ),
          
          # Tabbed customization
          bslib::navset_tab(
            id = session$ns("custom_tabs"),
            
            # ================================================================
            # FOREST PLOT TAB
            # ================================================================
            bslib::nav_panel(
              title = "Forest Plot",
              icon = icon("chart-bar"),
              div(
                style = "padding: 20px;",
                
                h5("Title", style = "color: #1d3557; margin-bottom: 15px;"),
                textInput(
                  session$ns("forest_title"),
                  NULL,
                  value = settings()$forest_title,
                  placeholder = "Enter plot title"
                ),
                
                hr(),
                h5("Point Settings", style = "color: #1d3557; margin-bottom: 15px;"),
                div(
                  class = "row",
                  div(
                    class = "col-md-6",
                    sliderInput(
                      session$ns("forest_point_size"),
                      "Point Size",
                      min = 1, max = 10, value = settings()$forest_point_size, step = 0.5
                    )
                  ),
                  div(
                    class = "col-md-6",
                    colourpicker::colourInput(
                      session$ns("forest_point_color"),
                      "Point Color",
                      value = settings()$forest_point_color
                    )
                  )
                ),
                
                hr(),
                h5("Summary & CI Settings", style = "color: #1d3557; margin-bottom: 15px;"),
                div(
                  class = "row",
                  div(
                    class = "col-md-4",
                    colourpicker::colourInput(
                      session$ns("forest_summary_color"),
                      "Summary Color",
                      value = settings()$forest_summary_color
                    )
                  ),
                  div(
                    class = "col-md-4",
                    colourpicker::colourInput(
                      session$ns("forest_ci_color"),
                      "CI Color",
                      value = settings()$forest_ci_color
                    )
                  ),
                  div(
                    class = "col-md-4",
                    sliderInput(
                      session$ns("forest_text_size"),
                      "Text Size",
                      min = 2, max = 8, value = settings()$forest_text_size, step = 0.5
                    )
                  )
                )
              )
            ),
            
            # ================================================================
            # FUNNEL PLOT TAB
            # ================================================================
            bslib::nav_panel(
              title = "Funnel Plot",
              icon = icon("filter"),
              div(
                style = "padding: 20px;",
                
                h5("Title", style = "color: #1d3557; margin-bottom: 15px;"),
                textInput(
                  session$ns("funnel_title"),
                  NULL,
                  value = settings()$funnel_title,
                  placeholder = "Enter plot title"
                ),
                
                hr(),
                h5("Point Settings", style = "color: #1d3557; margin-bottom: 15px;"),
                div(
                  class = "row",
                  div(
                    class = "col-md-6",
                    sliderInput(
                      session$ns("funnel_point_size"),
                      "Point Size",
                      min = 1, max = 10, value = settings()$funnel_point_size, step = 0.5
                    )
                  ),
                  div(
                    class = "col-md-6",
                    colourpicker::colourInput(
                      session$ns("funnel_point_color"),
                      "Point Color (No Subgroup)",
                      value = settings()$funnel_point_color
                    )
                  )
                ),
                
                hr(),
                h5("Contour Settings", style = "color: #1d3557; margin-bottom: 15px;"),
                colourpicker::colourInput(
                  session$ns("funnel_contours_col"),
                  "Contour Color",
                  value = settings()$funnel_contours_col
                ),
                
                hr(),
                h5("Subgroup Colors", style = "color: #1d3557; margin-bottom: 15px;"),
                p("Colors used when subgroup/moderator analysis is active", 
                  style = "color: #6c757d; font-size: 13px;"),
                div(
                  class = "row",
                  div(
                    class = "col-md-6",
                    colourpicker::colourInput(
                      session$ns("funnel_group_color1"),
                      "Group 1 Color",
                      value = settings()$funnel_group_colors[1]
                    )
                  ),
                  div(
                    class = "col-md-6",
                    colourpicker::colourInput(
                      session$ns("funnel_group_color2"),
                      "Group 2 Color",
                      value = settings()$funnel_group_colors[2]
                    )
                  )
                )
              )
            ),
            
            # ================================================================
            # SUNSET PLOT TAB
            # ================================================================
            bslib::nav_panel(
              title = "Sunset Plot",
              icon = icon("sun"),
              div(
                style = "padding: 20px;",
                
                h5("Title", style = "color: #1d3557; margin-bottom: 15px;"),
                textInput(
                  session$ns("sunset_title"),
                  NULL,
                  value = settings()$sunset_title,
                  placeholder = "Enter plot title"
                ),
                
                hr(),
                
                # ADD THESE NEW INPUTS FOR TRUE EFFECT AND SIG LEVEL
                h5("Power Analysis Settings", style = "color: #1d3557; margin-bottom: 15px;"),
                
                div(
                  class = "row",
                  div(
                    class = "col-md-6",
                    numericInput(
                      session$ns("sunset_true_effect"),
                      "True Effect (δ) - leave empty for summary effect",
                      value = settings()$sunset_true_effect,
                      step = 0.1
                    )
                  ),
                  div(
                    class = "col-md-6",
                    numericInput(
                      session$ns("sunset_sig_level"),
                      "Significance Level (α)",
                      value = settings()$sunset_sig_level,
                      min = 0.01,
                      max = 0.10,
                      step = 0.01
                    )
                  )
                ),
                
                hr(),
                
                h5("Point Settings", style = "color: #1d3557; margin-bottom: 15px;"),
                div(
                  class = "row",
                  div(
                    class = "col-md-6",
                    sliderInput(
                      session$ns("sunset_point_size"),
                      "Point Size",
                      min = 1, max = 10, value = settings()$sunset_point_size, step = 0.5
                    )
                  ),
                  div(
                    class = "col-md-6",
                    colourpicker::colourInput(
                      session$ns("sunset_point_color"),
                      "Point Color",
                      value = settings()$sunset_point_color
                    )
                  )
                ),
                
                hr(),
                h5("Contour Settings", style = "color: #1d3557; margin-bottom: 15px;"),
                colourpicker::colourInput(
                  session$ns("sunset_contour_col"),
                  "Contour Line Color",
                  value = settings()$sunset_contour_col
                ),
                
                hr(),
                h5("Power Gradient", style = "color: #1d3557; margin-bottom: 15px;"),
                p(icon("info-circle"), 
                  "Customize the 9-color gradient for power contours (dark to light):",
                  style = "color: #6c757d; font-size: 13px; margin-bottom: 15px;"),
                
                div(
                  class = "row",
                  div(class = "col-md-4",
                      colourpicker::colourInput(session$ns("sunset_power1"), "Color 1 (Darkest)", 
                                                value = settings()$sunset_power_col[1])),
                  div(class = "col-md-4",
                      colourpicker::colourInput(session$ns("sunset_power2"), "Color 2", 
                                                value = settings()$sunset_power_col[2])),
                  div(class = "col-md-4",
                      colourpicker::colourInput(session$ns("sunset_power3"), "Color 3", 
                                                value = settings()$sunset_power_col[3]))
                ),
                div(
                  class = "row",
                  div(class = "col-md-4",
                      colourpicker::colourInput(session$ns("sunset_power4"), "Color 4", 
                                                value = settings()$sunset_power_col[4])),
                  div(class = "col-md-4",
                      colourpicker::colourInput(session$ns("sunset_power5"), "Color 5", 
                                                value = settings()$sunset_power_col[5])),
                  div(class = "col-md-4",
                      colourpicker::colourInput(session$ns("sunset_power6"), "Color 6", 
                                                value = settings()$sunset_power_col[6]))
                ),
                div(
                  class = "row",
                  div(class = "col-md-4",
                      colourpicker::colourInput(session$ns("sunset_power7"), "Color 7", 
                                                value = settings()$sunset_power_col[7])),
                  div(class = "col-md-4",
                      colourpicker::colourInput(session$ns("sunset_power8"), "Color 8", 
                                                value = settings()$sunset_power_col[8])),
                  div(class = "col-md-4",
                      colourpicker::colourInput(session$ns("sunset_power9"), "Color 9 (Lightest)", 
                                                value = settings()$sunset_power_col[9]))
                )
              )
            ),
            
            # ================================================================
            # COOK'S DISTANCE TAB
            # ================================================================
            bslib::nav_panel(
              title = "Cook's Distance",
              icon = icon("circle-dot"),
              div(
                style = "padding: 20px;",
                
                h5("Title", style = "color: #1d3557; margin-bottom: 15px;"),
                textInput(
                  session$ns("cook_title"),
                  NULL,
                  value = settings()$cook_title,
                  placeholder = "Enter plot title"
                ),
                
                hr(),
                
                h5("Point Color Settings", style = "color: #1d3557; margin-bottom: 15px;"),
                div(
                  class = "row",
                  div(
                    class = "col-md-6",
                    colourpicker::colourInput(
                      session$ns("cook_color_below"),
                      "Point Color (Below Threshold)",
                      value = settings()$cook_color_below
                    )
                  ),
                  div(
                    class = "col-md-6",
                    colourpicker::colourInput(
                      session$ns("cook_color_above"),
                      "Point Color (Above Threshold)",
                      value = settings()$cook_color_above
                    )
                  )
                ),
                
                hr(),
                
                h5("Threshold Line & Subtitle", style = "color: #1d3557; margin-bottom: 15px;"),
                div(
                  class = "row",
                  div(
                    class = "col-md-6",
                    colourpicker::colourInput(
                      session$ns("cook_threshold_line_color"),
                      "Threshold Line Color",
                      value = settings()$cook_threshold_line_color
                    )
                  ),
                  div(
                    class = "col-md-6",
                    colourpicker::colourInput(
                      session$ns("cook_subtitle_color"),
                      "Subtitle Color",
                      value = settings()$cook_subtitle_color
                    )
                  )
                )
              )
            ),
            
            # ================================================================
            # STUDENTIZED RESIDUALS TAB
            # ================================================================
            bslib::nav_panel(
              title = "Studentized Residuals",
              icon = icon("chart-line"),
              div(
                style = "padding: 20px;",
                
                h5("Title", style = "color: #1d3557; margin-bottom: 15px;"),
                textInput(
                  session$ns("stud_title"),
                  NULL,
                  value = settings()$stud_title,
                  placeholder = "Enter plot title"
                ),
                
                hr(),
                
                h5("Point Color Settings", style = "color: #1d3557; margin-bottom: 15px;"),
                div(
                  class = "row",
                  div(
                    class = "col-md-6",
                    colourpicker::colourInput(
                      session$ns("stud_color_below"),
                      "Point Color (Within Range)",
                      value = settings()$stud_color_below
                    )
                  ),
                  div(
                    class = "col-md-6",
                    colourpicker::colourInput(
                      session$ns("stud_color_above"),
                      "Point Color (Outlier)",
                      value = settings()$stud_color_above
                    )
                  )
                ),
                
                hr(),
                
                h5("Threshold Line & Subtitle", style = "color: #1d3557; margin-bottom: 15px;"),
                div(
                  class = "row",
                  div(
                    class = "col-md-6",
                    colourpicker::colourInput(
                      session$ns("stud_threshold_line_color"),
                      "Threshold Line Color",
                      value = settings()$stud_threshold_line_color
                    )
                  ),
                  div(
                    class = "col-md-6",
                    colourpicker::colourInput(
                      session$ns("stud_subtitle_color"),
                      "Subtitle Color",
                      value = settings()$stud_subtitle_color
                    )
                  )
                )
              )
            ),
            
            
            # ================================================================
            # LEAVE-ONE-OUT TAB
            # ================================================================
            bslib::nav_panel(
              title = "Leave-One-Out",
              icon = icon("arrows-left-right"),
              div(
                style = "padding: 20px;",
                
                h5("Title", style = "color: #1d3557; margin-bottom: 15px;"),
                textInput(
                  session$ns("loo_title"),
                  NULL,
                  value = settings()$loo_title,
                  placeholder = "Enter plot title"
                ),
                
                hr(),
                h5("Point & CI Settings", style = "color: #1d3557; margin-bottom: 15px;"),
                div(
                  class = "row",
                  div(
                    class = "col-md-6",
                    sliderInput(
                      session$ns("loo_point_size"),
                      "Point Size",
                      min = 1, max = 8, value = settings()$loo_point_size, step = 0.5
                    )
                  ),
                  div(
                    class = "col-md-6",
                    colourpicker::colourInput(
                      session$ns("loo_point_color"),
                      "Point Color",
                      value = settings()$loo_point_color
                    )
                  )
                ),
                
                hr(),
                h5("CI & Reference Line", style = "color: #1d3557; margin-bottom: 15px;"),
                div(
                  class = "row",
                  div(
                    class = "col-md-6",
                    colourpicker::colourInput(
                      session$ns("loo_ci_color"),
                      "CI Color",
                      value = settings()$loo_ci_color
                    )
                  ),
                  div(
                    class = "col-md-6",
                    colourpicker::colourInput(
                      session$ns("loo_refline_color"),
                      "Reference Line Color",
                      value = settings()$loo_refline_color
                    )
                  )
                )
              )
            )
          )
        )
      )
    }
    
    # Apply customization
    observeEvent(input$apply_custom, {
      new_settings <- list(
        # Forest
        forest_point_size = input$forest_point_size,
        forest_point_color = input$forest_point_color,
        forest_summary_color = input$forest_summary_color,
        forest_ci_color = input$forest_ci_color,
        forest_text_size = input$forest_text_size,
        forest_title = input$forest_title,
        
        # Funnel
        funnel_point_color = input$funnel_point_color,
        funnel_point_size = input$funnel_point_size,
        funnel_contours_col = input$funnel_contours_col,
        funnel_group_colors = c(input$funnel_group_color1, input$funnel_group_color2),
        funnel_title = input$funnel_title,
        
        # Sunset - ADD THESE TWO LINES
        sunset_true_effect = if(is.na(input$sunset_true_effect) || input$sunset_true_effect == "") NULL else input$sunset_true_effect,
        sunset_sig_level = input$sunset_sig_level,
        sunset_point_color = input$sunset_point_color,
        sunset_point_size = input$sunset_point_size,
        sunset_power_col = c(input$sunset_power1, input$sunset_power2, input$sunset_power3,
                             input$sunset_power4, input$sunset_power5, input$sunset_power6,
                             input$sunset_power7, input$sunset_power8, input$sunset_power9),
        sunset_contour_col = input$sunset_contour_col,
        sunset_title = input$sunset_title,
        
        # Cook's
        cook_color_below = input$cook_color_below,
        cook_color_above = input$cook_color_above,
        cook_title = input$cook_title,
        cook_threshold_line_color = input$cook_threshold_line_color,  
        cook_subtitle_color = input$cook_subtitle_color,              
        
        # Studentized
        stud_color_below = input$stud_color_below,
        stud_color_above = input$stud_color_above,
        stud_title = input$stud_title,
        stud_threshold_line_color = input$stud_threshold_line_color,  
        stud_subtitle_color = input$stud_subtitle_color,              
        
        
        # LOO
        loo_point_color = input$loo_point_color,
        loo_point_size = input$loo_point_size,
        loo_ci_color = input$loo_ci_color,
        loo_refline_color = input$loo_refline_color,
        loo_title = input$loo_title
      )
      
      settings(new_settings)
      removeModal()
      showNotification("Plot customization applied!", type = "message", duration = 2)
    })
    
    # Reset to defaults
    observeEvent(input$reset_defaults, {
      settings(default_settings)
      
      # Update all forest inputs
      updateTextInput(session, "forest_title", value = default_settings$forest_title)
      updateSliderInput(session, "forest_point_size", value = default_settings$forest_point_size)
      colourpicker::updateColourInput(session, "forest_point_color", value = default_settings$forest_point_color)
      colourpicker::updateColourInput(session, "forest_summary_color", value = default_settings$forest_summary_color)
      colourpicker::updateColourInput(session, "forest_ci_color", value = default_settings$forest_ci_color)
      updateSliderInput(session, "forest_text_size", value = default_settings$forest_text_size)
      
      # Update funnel inputs
      updateTextInput(session, "funnel_title", value = default_settings$funnel_title)
      updateSliderInput(session, "funnel_point_size", value = default_settings$funnel_point_size)
      colourpicker::updateColourInput(session, "funnel_point_color", value = default_settings$funnel_point_color)
      colourpicker::updateColourInput(session, "funnel_contours_col", value = default_settings$funnel_contours_col)
      colourpicker::updateColourInput(session, "funnel_group_color1", value = default_settings$funnel_group_colors[1])
      colourpicker::updateColourInput(session, "funnel_group_color2", value = default_settings$funnel_group_colors[2])
      
      # Update sunset inputs
      updateTextInput(session, "sunset_title", value = default_settings$sunset_title)
      updateNumericInput(session, "sunset_true_effect", value = default_settings$sunset_true_effect)  # ADD
      updateNumericInput(session, "sunset_sig_level", value = default_settings$sunset_sig_level)      # ADD
      updateSliderInput(session, "sunset_point_size", value = default_settings$sunset_point_size)
      colourpicker::updateColourInput(session, "sunset_point_color", value = default_settings$sunset_point_color)
      for(i in 1:9) {
        colourpicker::updateColourInput(session, paste0("sunset_power", i), value = default_settings$sunset_power_col[i])
      }
      
      # Update Cook's inputs
      updateTextInput(session, "cook_title", value = default_settings$cook_title)
      colourpicker::updateColourInput(session, "cook_color_below", value = default_settings$cook_color_below)
      colourpicker::updateColourInput(session, "cook_color_above", value = default_settings$cook_color_above)
      colourpicker::updateColourInput(session, "cook_threshold_line_color", 
                                      value = default_settings$cook_threshold_line_color) 
      colourpicker::updateColourInput(session, "cook_subtitle_color", value = default_settings$cook_subtitle_color)             
      
      # Update Studentized inputs
      updateTextInput(session, "stud_title", value = default_settings$stud_title)
      colourpicker::updateColourInput(session, "stud_color_below", value = default_settings$stud_color_below)
      colourpicker::updateColourInput(session, "stud_color_above", value = default_settings$stud_color_above)
      colourpicker::updateColourInput(session, "stud_threshold_line_color", 
                                      value = default_settings$stud_threshold_line_color)  
      colourpicker::updateColourInput(session, "stud_subtitle_color", value = default_settings$stud_subtitle_color)              
      
      
      # Update LOO inputs
      updateTextInput(session, "loo_title", value = default_settings$loo_title)
      updateSliderInput(session, "loo_point_size", value = default_settings$loo_point_size)
      colourpicker::updateColourInput(session, "loo_point_color", value = default_settings$loo_point_color)
      colourpicker::updateColourInput(session, "loo_ci_color", value = default_settings$loo_ci_color)
      colourpicker::updateColourInput(session, "loo_refline_color", value = default_settings$loo_refline_color)
      
      showNotification("Reset to default settings!", type = "warning", duration = 2)
    })
    
    # Return interface
    return(list(
      show_modal = show_modal,
      settings = settings
    ))
  })
}
