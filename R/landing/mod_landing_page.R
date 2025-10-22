# =============================================================================
# Landing Page Module
# =============================================================================

landingPageUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Hero Section
    div(
      class = "hero-section",
      div(
        class = "hero-content",
        
        h1("MetaSuite", class = "hero-title"),
        
        p("Comprehensive Toolkit for Evidence Synthesis",
          class = "hero-subtitle"),
        
        p("Streamline your systematic reviews with powerful tools for risk of bias assessment, 
          meta-analysis, and network meta-analysis. Designed for researchers, by researchers.",
          class = "hero-description"),
        
        div(
          class = "cta-buttons",
          
          actionButton(
            ns("get_started"),
            tagList(icon("rocket"), " Get Started"),
            class = "btn-hero-primary",
            onclick = "document.querySelector('a.nav-link[data-value=\"rob_tab\"]').click();"
          ),
          
          actionButton(
            ns("view_docs"),
            tagList(icon("book"), " Documentation"),
            class = "btn-hero-secondary",
            onclick = "document.querySelector('a.nav-link[data-value=\"docs_tab\"]').click();"
          )
        )
      )
    ),
    
    # Carousel Section
    div(
      class = "carousel-section",
      
      div(
        class = "carousel-container",
        style = "position: relative;",  # Add this for positioning
        
        # LEFT ARROW (outside slides)
        tags$button(
          id = "carousel-prev",
          class = "carousel-nav-arrow carousel-nav-left",
          icon("chevron-left")
        ),
        
        # RIGHT ARROW (outside slides)
        tags$button(
          id = "carousel-next",
          class = "carousel-nav-arrow carousel-nav-right",
          icon("chevron-right")
        ),
        
        # Slide 1: Risk of Bias
        div(
          class = "carousel-slide active",
          
          div(
            class = "slide-content",
            h2(icon("shield-halved"), " Risk of Bias Assessment"),
            p("Evaluate the methodological quality of studies with comprehensive ROB tools. 
              Generate publication-ready visualizations with customizable color schemes."),
            
            tags$ul(
              class = "slide-features",
              tags$li("Support for 5 ROB tools (ROB 2, QUADAS-2, QUIPS, ROBINS-I, ROBINS-E)"),
              tags$li("Interactive summary and traffic light plots"),
              tags$li("Domain-specific risk assessment with full descriptions"),
              tags$li("Custom color schemes and export options"),
              tags$li("Load sample data or upload your own assessments")
            ),
            
            actionButton(
              ns("goto_rob"),
              tagList("Risk of Bias Assessment", icon("arrow-right")),
              class = "slide-cta-btn",
              onclick = "document.querySelector('a.nav-link[data-value=\"rob_tab\"]').click();"
            )
          ),
          
          div(
            class = "slide-visual",
            div(class = "slide-visual-icon", icon("shield-halved"))
          )
        ),
        
        # Slide 2: Meta-Analysis
        div(
          class = "carousel-slide",
          
          div(
            class = "slide-content",
            h2(icon("chart-line"), " Meta-Analysis"),
            p("Synthesize quantitative evidence with forest plots, funnel plots, and 
              heterogeneity assessment. Perform subgroup analyses and detect publication bias."),
            
            tags$ul(
              class = "slide-features",
              tags$li("Forest plots with confidence intervals"),
              tags$li("Funnel plots for publication bias detection"),
              tags$li("Fixed and random effects models"),
              tags$li("Heterogeneity assessment (I², τ², Q-test)"),
              tags$li("Subgroup and sensitivity analyses")
            ),
            
            actionButton(
              ns("goto_meta"),
              tagList("Perform Meta-Analysis", icon("arrow-right")),
              class = "slide-cta-btn",
              onclick = "document.querySelector('a.nav-link[data-value=\"meta_tab\"]').click();"
            )
          ),
          
          div(
            class = "slide-visual",
            div(class = "slide-visual-icon", icon("chart-line"))
          )
        ),
        
        # Slide 3: Network Meta-Analysis
        div(
          class = "carousel-slide",
          
          div(
            class = "slide-content",
            h2(icon("diagram-project"), " Network Meta-Analysis"),
            p("Compare multiple interventions simultaneously with network meta-analysis. 
              Generate network plots, league tables, and ranking probabilities."),
            
            tags$ul(
              class = "slide-features",
              tags$li("Network geometry visualization"),
              tags$li("League tables for all comparisons"),
              tags$li("SUCRA rankings and cumulative rankograms"),
              tags$li("Consistency and inconsistency assessment"),
              tags$li("Node-splitting analysis")
            ),
            
            actionButton(
              ns("goto_network"),
              tagList("Explore Network Meta", icon("arrow-right")),
              class = "slide-cta-btn",
              onclick = "document.querySelector('a.nav-link[data-value=\"network_tab\"]').click();"
            )
          ),
          
          div(
            class = "slide-visual",
            div(class = "slide-visual-icon", icon("diagram-project"))
          )
        ),
        
        # Indicators at bottom (centered)
        div(
          class = "carousel-indicators-bottom",
          tags$button(class = "carousel-indicator active", `data-slide` = 0),
          tags$button(class = "carousel-indicator", `data-slide` = 1),
          tags$button(class = "carousel-indicator", `data-slide` = 2)
        )
      )
    ),
    
    
    # Why Choose Us Section
    # App Summary Section
    div(
      class = "summary-section",
      
      h2("App Overview", class = "section-title"),
      p("Evidence synthesis made efficient and transparent", class = "section-subtitle"),
      
      div(
        class = "summary-content",
        
        div(
          class = "summary-card",
          h3(icon("bullseye"), " Purpose"),
          p("MetaSuite is an open-source, web-based platform designed to streamline evidence synthesis workflows for systematic reviews and meta-analyses. Built with R Shiny, it provides researchers with publication-ready visualizations and robust analytical tools following Cochrane and PRISMA guidelines.")
        ),
        
        div(
          class = "summary-card",
          h3(icon("users"), " Target Audience"),
          p("Healthcare researchers, systematic reviewers, meta-analysts, epidemiologists, and evidence synthesis teams conducting high-quality research for clinical decision-making and policy development.")
        ),
        
        div(
          class = "summary-card",
          h3(icon("gear"), " Technical Stack"),
          p(strong("Backend:"), " R (Shiny, bslib, ggplot2, dplyr)", tags$br(),
            strong("Frontend:"), " HTML5, CSS3, JavaScript", tags$br(),
            strong("Standards:"), " PRISMA 2020, Cochrane ROB 2.0", tags$br(),
            strong("License:"), " Open Source (MIT)")
        )
      )
    ),
    
    # References Section
    div(
      class = "references-section",
      
      h2("Key References", class = "section-title"),
      p("Methodological foundations", class = "section-subtitle"),
      
      div(
        class = "references-content",
        
        div(
          class = "reference-card",
          div(class = "ref-number", "1"),
          div(
            class = "ref-content",
            p(strong("Sterne JAC, et al."), " (2019). RoB 2: a revised tool for assessing risk of bias in randomised trials. ", 
              tags$em("BMJ"), ", 366:l4898.",
              style = "margin: 0;"),
            a(href = "https://doi.org/10.1136/bmj.l4898", target = "_blank", 
              "doi:10.1136/bmj.l4898", class = "ref-link")
          )
        ),
        
        div(
          class = "reference-card",
          div(class = "ref-number", "2"),
          div(
            class = "ref-content",
            p(strong("Page MJ, et al."), " (2021). The PRISMA 2020 statement: an updated guideline for reporting systematic reviews. ",
              tags$em("BMJ"), ", 372:n71.",
              style = "margin: 0;"),
            a(href = "https://doi.org/10.1136/bmj.n71", target = "_blank",
              "doi:10.1136/bmj.n71", class = "ref-link")
          )
        ),
        
        div(
          class = "reference-card",
          div(class = "ref-number", "3"),
          div(
            class = "ref-content",
            p(strong("Whiting PF, et al."), " (2011). QUADAS-2: a revised tool for the quality assessment of diagnostic accuracy studies. ",
              tags$em("Ann Intern Med"), ", 155(8):529-536.",
              style = "margin: 0;"),
            a(href = "https://doi.org/10.7326/0003-4819-155-8-201110180-00009", target = "_blank",
              "doi:10.7326/0003-4819-155-8-201110180-00009", class = "ref-link")
          )
        ),
        
        div(
          class = "reference-card",
          div(class = "ref-number", "4"),
          div(
            class = "ref-content",
            p(strong("Sterne JA, et al."), " (2016). ROBINS-I: a tool for assessing risk of bias in non-randomised studies of interventions. ",
              tags$em("BMJ"), ", 355:i4919.",
              style = "margin: 0;"),
            a(href = "https://doi.org/10.1136/bmj.i4919", target = "_blank",
              "doi:10.1136/bmj.i4919", class = "ref-link")
          )
        )
      )
    ),
    
    # Future Development Section
    div(
      class = "future-section",
      
      h2("Roadmap & Future Development", class = "section-title"),
      p("Planned enhancements and upcoming features", class = "section-subtitle"),
      
      div(
        class = "timeline-container",
        
        # Phase 1 - Current
        div(
          class = "timeline-item timeline-current",
          div(class = "timeline-marker"),
          div(
            class = "timeline-content",
            h4(icon("check-circle"), " Phase 1: Foundation (Current)"),
            tags$ul(
              tags$li("Risk of Bias assessment tools (ROB 2, QUADAS-2, QUIPS, ROBINS-I/E)"),
              tags$li("Interactive visualizations with customizable themes"),
              tags$li("Data import/export functionality"),
              tags$li("Publication-ready figure generation")
            )
          )
        ),
        
        # Phase 2 - Q1 2025
        div(
          class = "timeline-item timeline-upcoming",
          div(class = "timeline-marker"),
          div(
            class = "timeline-content",
            h4(icon("clock"), " Phase 2: Meta-Analysis (Q1 2026)"),
            tags$ul(
              tags$li("Forest plot generation with subgroup analysis"),
              tags$li("Funnel plots for publication bias assessment"),
              tags$li("Heterogeneity analysis (I², τ², Q-test)"),
              tags$li("Sensitivity and meta-regression analysis")
            )
          )
        ),
        
        # Phase 3 - Q2 2025
        div(
          class = "timeline-item timeline-future",
          div(class = "timeline-marker"),
          div(
            class = "timeline-content",
            h4(icon("lightbulb"), " Phase 3: Network Meta-Analysis (Q2 2026)"),
            tags$ul(
              tags$li("Network geometry visualization"),
              tags$li("League tables and ranking probabilities"),
              tags$li("SUCRA calculations and rankograms"),
              tags$li("Consistency and node-splitting analysis")
            )
          )
        ),
        
        # Phase 4 - Beyond
        div(
          class = "timeline-item timeline-future",
          div(class = "timeline-marker"),
          div(
            class = "timeline-content",
            h4(icon("star"), " Phase 4: Advanced Features (Future)"),
            tags$ul(
              tags$li("AI-assisted data extraction"),
              tags$li("Automated report generation"),
              tags$li("Collaborative workspace for teams"),
              tags$li("Integration with reference management software"),
              tags$li("Real-time protocol registration")
            )
          )
        )
      )
    ),
    
    # Footer
    div(
      class = "footer-section",
      
      div(
        style = "text-align: center; padding: 40px 20px; background: #f8f9fa; border-top: 2px solid #e0e0e0;",
        
        p(
          style = "color: #1d3557; margin-bottom: 15px; font-size: 16px; font-weight: 600;",
          "MetaSuite: Open Source Evidence Synthesis Platform"
        ),
        
        p(
          style = "color: #6C757D; margin: 0; font-size: 14px;",
          "Version 1.0.0 | ",
          tags$a(href = "https://github.com/yourrepo/metasuite", target = "_blank", 
                 style = "color: #457b9d;", "GitHub Repository"),
          " | ",
          tags$a(href = "mailto:contact@metasuite.org", 
                 style = "color: #457b9d;", "Contact")
        ),
        
        p(
          style = "color: #6C757D; margin-top: 10px; font-size: 12px;",
          "© 2025 MetaSuite Contributors | Licensed under MIT"
        )
      )
    )
    
  )
}

landingPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Navigate to ROB tab - FIXED
    observeEvent(input$goto_rob, {
      shiny::updateNavbarPage(
        session = getDefaultReactiveDomain(),
        inputId = "main_navbar",
        selected = "rob_tab"
      )
    })
    
    # Navigate to Meta tab
    observeEvent(input$goto_meta, {
      shiny::updateNavbarPage(
        session = getDefaultReactiveDomain(),
        inputId = "main_navbar",
        selected = "meta_tab"
      )
    })
    
    # Navigate to Network tab
    observeEvent(input$goto_network, {
      shiny::updateNavbarPage(
        session = getDefaultReactiveDomain(),
        inputId = "main_navbar",
        selected = "network_tab"
      )
    })
    
    # Get Started button
    observeEvent(input$get_started, {
      shiny::updateNavbarPage(
        session = getDefaultReactiveDomain(),
        inputId = "main_navbar",
        selected = "rob_tab"
      )
    })
    
    # View Documentation - Navigate to docs tab
    observeEvent(input$view_docs, {
      updateNavbarPage(session = getDefaultReactiveDomain(), 
                       inputId = "main_navbar", 
                       selected = "docs_tab")
    })
    
  })
}
