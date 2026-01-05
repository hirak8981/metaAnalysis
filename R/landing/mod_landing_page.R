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
        
        p("Supporting transparent, reproducible, and methodologically rigorous workflows for risk-of-bias assessment, meta-analysis, and network meta-analysis.",
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
              tags$li("Custom risk-of-bias visualizations"),
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
          
          # Slide 1 visual
          div(
            class = "slide-visual",
            div(
              id = "rob-inner-carousel",
              class = "inner-carousel",
              img(src = "rob_heatmap.png",    class = "inner-slide active"),
              img(src = "rob_trafficlight.png", class = "inner-slide"),
              img(src = "rob_summary.png",    class = "inner-slide")
            )
          ),
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
              tags$li("Forest plots (fixed and random effects)"),
              tags$li("Funnel plots for publication bias detection"),
              tags$li("Subgroup and sensitivity analyses"),
              tags$li("Heterogeneity assessment (I², τ², Q-test)"),
              tags$li("Publication-ready plot exports")
            ),
            
            actionButton(
              ns("goto_meta"),
              tagList("Perform Meta-Analysis", icon("arrow-right")),
              class = "slide-cta-btn",
              onclick = "document.querySelector('a.nav-link[data-value=\"meta_tab\"]').click();"
            )
          ),
          
          # Slide 2 visual
          div(
            class = "slide-visual",
            div(
              id = "meta-inner-carousel",
              class = "inner-carousel",
              img(src = "meta_forest.png",  class = "inner-slide active"),
              img(src = "meta_funnel.png",  class = "inner-slide"),
              img(src = "meta_sunset.png",  class = "inner-slide"),
              img(src = "meta_loo.png",     class = "inner-slide")
            )
          ),
          
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
              tags$li("Multiple-treatment comparisons in a network"),
              tags$li("League tables for all comparisons"),
              tags$li("SUCRA rankings and cumulative rankograms"),
              tags$li("Consistency and evidence flow diagnostics"),
              tags$li("Advanced network visualizations")
            ),
            
            actionButton(
              ns("goto_network"),
              tagList("Explore Network Meta", icon("arrow-right")),
              class = "slide-cta-btn",
              onclick = "document.querySelector('a.nav-link[data-value=\"network_tab\"]').click();"
            )
          ),
          
          # Slide 3 visual
          div(
            class = "slide-visual",
            div(
              id = "nma-inner-carousel",
              class = "inner-carousel",
              img(src = "nma_network.png",       class = "inner-slide active"),
              img(src = "nma_directevidence.png",class = "inner-slide"),
              img(src = "nma_radialrank.png",    class = "inner-slide"),
              img(src = "nma_inconsistency.png", class = "inner-slide"),
              img(src = "nma_beading.png",       class = "inner-slide")
            )
          ),
          
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
          p("MetaSuite is an open-source, web-based platform designed to streamline evidence synthesis workflows for systematic reviews and meta-analyses. Built with R Shiny, it provides researchers with publication-ready visualizations and robust analytical tools following Cochrane guidelines.")
        ),
        
        div(
          class = "summary-card",
          h3(icon("users"), " Target Audience"),
          p("Healthcare researchers, systematic reviewers, meta-analysts, epidemiologists, and evidence synthesis teams conducting high-quality research for clinical decision-making and policy development.")
        ),
        
        div(
          class = "summary-card",
          h3(icon("gear"), " Technical Stack"),
          p(strong("Backend:"), " R (Shiny, bslib, ggplot2, dplyr, metafor, netmeta, rankinma)", tags$br(),
            strong("Frontend:"), " Bootstrap 5 (via bslib), HTML5, CSS3, JavaScript", tags$br(),
            strong("Standards:"), "Cochrane ROB 2.0", tags$br())
        )
      )
    ),
    
    # References Section
    # References Section
    div(
      class = "references-section",
      h2("Key References", class = "section-title"),
      p("Methodological foundations", class = "section-subtitle"),
      div(
        class = "references-content",
        
        # Reference 1
        div(
          class = "reference-card",
          div(class = "ref-number", "1"),
          div(
            class = "ref-content",
            p(strong("Sterne, J. A. C., Savović, J., Page, M. J., Elbers, R. G., Blencowe, N. S., Boutron, I., … Higgins, J. P. T."), 
              " (2019). RoB 2: A revised tool for assessing risk of bias in randomised trials. ",
              tags$em("BMJ"), ", 366, l4898.",
              style = "margin: 0;"),
            a(href = "https://doi.org/10.1136/bmj.l4898", target = "_blank",
              "https://doi.org/10.1136/bmj.l4898", class = "ref-link")
          )
        ),
        
        # Reference 2
        div(
          class = "reference-card",
          div(class = "ref-number", "2"),
          div(
            class = "ref-content",
            p(strong("McGuinness, L. A., & Higgins, J. P. T."), 
              " (2021). Risk-of-bias visualization (robvis): An R package and web app for visualizing risk-of-bias assessments. ",
              tags$em("Research Synthesis Methods"), ", 12(1), 55–61.",
              style = "margin: 0;"),
            a(href = "https://doi.org/10.1002/jrsm.1411", target = "_blank",
              "https://doi.org/10.1002/jrsm.1411", class = "ref-link")
          )
        ),
        
        # Reference 3
        div(
          class = "reference-card",
          div(class = "ref-number", "3"),
          div(
            class = "ref-content",
            p(strong("Higgins, J. P. T., Thomas, J., Chandler, J., Cumpston, M., Li, T., Page, M. J., & Welch, V. A. (Eds.)."), 
              " (2023). Cochrane handbook for systematic reviews of interventions (Version 6.4). Cochrane.",
              style = "margin: 0;"),
            a(href = "https://training.cochrane.org/handbook", target = "_blank",
              "https://training.cochrane.org/handbook", class = "ref-link")
          )
        ),
        
        # Reference 4
        div(
          class = "reference-card",
          div(class = "ref-number", "4"),
          div(
            class = "ref-content",
            p(strong("Viechtbauer, W."), 
              " (2010). Conducting meta-analyses in R with the metafor package. ",
              tags$em("Journal of Statistical Software"), ", 36(3), 1–48.",
              style = "margin: 0;"),
            a(href = "https://doi.org/10.18637/jss.v036.i03", target = "_blank",
              "https://doi.org/10.18637/jss.v036.i03", class = "ref-link")
          )
        ),
        
        # Reference 5
        div(
          class = "reference-card",
          div(class = "ref-number", "5"),
          div(
            class = "ref-content",
            p(strong("DerSimonian, R., & Laird, N."), 
              " (1986). Meta-analysis in clinical trials. ",
              tags$em("Controlled Clinical Trials"), ", 7(3), 177–188.",
              style = "margin: 0;"),
            a(href = "https://doi.org/10.1016/0197-2456(86)90046-2", target = "_blank",
              "https://doi.org/10.1016/0197-2456(86)90046-2", class = "ref-link")
          )
        ),
        
        # Reference 6
        div(
          class = "reference-card",
          div(class = "ref-number", "6"),
          div(
            class = "ref-content",
            p(strong("Higgins, J. P. T., Thompson, S. G., Deeks, J. J., & Altman, D. G."), 
              " (2003). Measuring inconsistency in meta-analyses. ",
              tags$em("BMJ"), ", 327(7414), 557–560.",
              style = "margin: 0;"),
            a(href = "https://doi.org/10.1136/bmj.327.7414.557", target = "_blank",
              "https://doi.org/10.1136/bmj.327.7414.557", class = "ref-link")
          )
        ),
        
        # Reference 7
        div(
          class = "reference-card",
          div(class = "ref-number", "7"),
          div(
            class = "ref-content",
            p(strong("Rücker, G., Schwarzer, G., Krahn, U., & König, J."), 
              " (2015). netmeta: Network meta-analysis using frequentist methods. ",
              tags$em("The R Journal"), ", 7(2), 315–324.",
              style = "margin: 0;"),
            a(href = "https://doi.org/10.32614/RJ-2015-023", target = "_blank",
              "https://doi.org/10.32614/RJ-2015-023", class = "ref-link")
          )
        ),
        
        # Reference 8
        div(
          class = "reference-card",
          div(class = "ref-number", "8"),
          div(
            class = "ref-content",
            p(strong("Krahn, U., Binder, H., & König, J."), 
              " (2013). A graphical tool for locating inconsistency in network meta-analyses. ",
              tags$em("BMC Medical Research Methodology"), ", 13, Article 35.",
              style = "margin: 0;"),
            a(href = "https://doi.org/10.1186/1471-2288-13-35", target = "_blank",
              "https://doi.org/10.1186/1471-2288-13-35", class = "ref-link")
          )
        ),
        
        # Reference 9
        div(
          class = "reference-card",
          div(class = "ref-number", "9"),
          div(
            class = "ref-content",
            p(strong("Veroniki, A. A., Straus, S. E., Fyraridis, A., & Tricco, A. C."), 
              " (2016). The rank-heat plot is a novel way to present the results from a network meta-analysis including multiple outcomes. ",
              tags$em("Journal of Clinical Epidemiology"), ", 76, 193–199.",
              style = "margin: 0;"),
            a(href = "https://doi.org/10.1016/j.jclinepi.2016.02.016", target = "_blank",
              "https://doi.org/10.1016/j.jclinepi.2016.02.016", class = "ref-link")
          )
        ),
        
        # Reference 10
        div(
          class = "reference-card",
          div(class = "ref-number", "10"),
          div(
            class = "ref-content",
            p(strong("Chen, C., Chuang, Y.-C., Chan, E., et al."), 
              " (2023, September 28). Beading plot: A novel graphics for ranking interventions in network evidence (Version 1) [Preprint]. ",
              tags$em("Research Square"), ".",
              style = "margin: 0;"),
            a(href = "https://doi.org/10.21203/rs.3.rs-3370844/v1", target = "_blank",
              "https://doi.org/10.21203/rs.3.rs-3370844/v1", class = "ref-link")
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
            h4(icon("check-circle"), " Phase 1: Risk of Bias Foundation"),
            tags$ul(
              tags$li("Comprehensive ROB assessment (ROB 2, QUADAS-2, QUIPS, ROBINS-I/E)"),
              tags$li("Customizable ROB visualizations"),
              tags$li("Flexible data import and export"),
              tags$li("Publication-ready risk-of-bias figures")
            )
          )
        ),
        
        # Phase 2 - Q1 2025
        div(
          class = "timeline-item timeline-current",
          div(class = "timeline-marker"),
          div(
            class = "timeline-content",
            h4(icon("check-circle"), " Phase 2: MMeta-Analysis Toolkit"),
            tags$ul(
              tags$li("Fixed- and random-effects meta-analysis"),
              tags$li("Forest plots with subgroup analysis"),
              tags$li("Funnel plots and publication bias diagnostics"),
              tags$li("Heterogeneity, sensitivity, and influence analyses")
            )
          )
        ),
        
        # Phase 3 - Q2 2025
        div(
          class = "timeline-item timeline-current",
          div(class = "timeline-marker"),
          div(
            class = "timeline-content",
            h4(icon("check-circle"), " Phase 3: Network Meta-Analysis Framework"),
            tags$ul(
              tags$li("Treatment network visualization"),
              tags$li("League tables and ranking probabilities"),
              tags$li("SUCRA, rankograms, and ranking summaries"),
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
            h4(icon("lightbulb"), " Phase 4: AAdvanced & Integrative Features"),
            tags$ul(
              tags$li("Meta-regression and advanced moderator analyses"),
              tags$li("Bayesian network meta-analysis models"),
              tags$li("Support for additional data types (survival, diagnostic tests)"),
              tags$li("AI-assisted data extraction"),
              tags$li("Automated report generation")
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
          tags$a(href = "https://github.com/hirak8981/metaAnalysis", target = "_blank", 
                 style = "color: #457b9d;", "GitHub Repository"),
          " | ",
          tags$a(href = "mailto:hirak@princepstech.com", 
                 style = "color: #457b9d;", "Contact")
        ),
        
        p(
          style = "color: #6C757D; margin-top: 10px; font-size: 12px;",
          "© 2026 MetaSuite Contributors"
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
