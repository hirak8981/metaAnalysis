# =============================================================================
# Documentation Module with Sidebar Navigation
# =============================================================================

#' Documentation UI
#' @param id Module namespace ID

documentationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Custom CSS for documentation
    tags$style(HTML("
      .doc-sidebar {
        background: #f8f9fa;
        padding: 30px 20px;
        border-right: 2px solid #e0e0e0;
        min-height: calc(100vh - 60px);
      }
      
      .doc-nav-item {
        padding: 15px 20px;
        margin-bottom: 10px;
        border-radius: 8px;
        cursor: pointer;
        transition: all 0.3s ease;
        font-weight: 500;
        color: #5A6169;
        border-left: 4px solid transparent;
      }
      
      .doc-nav-item:hover {
        background: white;
        border-left-color: #667eea;
        color: #1d3557;
        box-shadow: 0 2px 8px rgba(0,0,0,0.05);
      }
      
      .doc-nav-item.active {
        background: white;
        border-left-color: #1d3557;
        color: #1d3557;
        font-weight: 600;
        box-shadow: 0 2px 12px rgba(0,0,0,0.08);
      }
      
      .doc-content-area {
        padding: 40px;
        max-width: 1000px;
        margin: 0 auto;
      }
      
      .doc-section-badge {
        display: inline-block;
        padding: 4px 12px;
        border-radius: 12px;
        font-size: 11px;
        font-weight: 600;
        margin-left: 10px;
      }
      
      .badge-complete {
        background: #d4edda;
        color: #155724;
      }
      
      .badge-soon {
        background: #fff3cd;
        color: #856404;
      }
    ")),
    
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,
        bg = "#f8f9fa",
        
        # Sidebar Header
        div(
          style = "margin-bottom: 30px; padding-bottom: 20px; border-bottom: 2px solid #e0e0e0;",
          h4(icon("book"), " Documentation", style = "color: #1d3557; margin: 0; font-weight: 700;")
        ),
        
        # Navigation Items
        div(
          class = "doc-nav-item active",
          id = ns("nav_getting_started"),
          onclick = sprintf("Shiny.setInputValue('%s', 'getting_started', {priority: 'event'})", ns("section")),
          icon("rocket"),
          " Getting Started"
        ),
        
        div(
          class = "doc-nav-item",
          id = ns("nav_rob"),
          onclick = sprintf("Shiny.setInputValue('%s', 'rob', {priority: 'event'})", ns("section")),
          icon("shield-halved"),
          " Risk of Bias",
          tags$span("Complete", class = "doc-section-badge badge-complete")
        ),
        
        div(
          class = "doc-nav-item",
          id = ns("nav_meta"),
          onclick = sprintf("Shiny.setInputValue('%s', 'meta', {priority: 'event'})", ns("section")),
          icon("chart-line"),
          " Meta-Analysis",
          tags$span("Soon", class = "doc-section-badge badge-soon")
        ),
        
        div(
          class = "doc-nav-item",
          id = ns("nav_nma"),
          onclick = sprintf("Shiny.setInputValue('%s', 'nma', {priority: 'event'})", ns("section")),
          icon("diagram-project"),
          " Network Meta-Analysis",
          tags$span("Soon", class = "doc-section-badge badge-soon")
        ),
        
        div(
          class = "doc-nav-item",
          id = ns("nav_faqs"),
          onclick = sprintf("Shiny.setInputValue('%s', 'faqs', {priority: 'event'})", ns("section")),
          icon("circle-question"),
          " FAQs & Support"
        ),
        
        # Footer
        div(
          style = "margin-top: 40px; padding-top: 20px; border-top: 2px solid #e0e0e0;",
          tags$small(
            style = "color: #7F8C8D;",
            icon("quote-left"),
            " ",
            tags$a(
              "How to Cite",
              href = "#",
              onclick = sprintf("Shiny.setInputValue('%s', 'citation', {priority: 'event'})", ns("section")),
              style = "color: #667eea; text-decoration: none; font-weight: 600;"
            )
          )
        )
      ),
      
      # Main Content Area
      div(
        class = "doc-content-area",
        uiOutput(ns("content"))
      )
    ),
    
    # JavaScript for navigation highlighting
    tags$script(HTML(sprintf("
      $(document).ready(function() {
        $('.doc-nav-item').on('click', function() {
          $('.doc-nav-item').removeClass('active');
          $(this).addClass('active');
        });
      });
    ")))
  )
}

#' Documentation Server
#' @param id Module namespace ID

documentationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Track current section
    current_section <- reactiveVal("getting_started")
    
    # Handle section navigation
    observeEvent(input$section, {
      current_section(input$section)
    })
    
    # Render content based on section
    output$content <- renderUI({
      switch(current_section(),
             "getting_started" = getting_started_content(),
             "rob" = rob_content(),
             "meta" = meta_content(),
             "nma" = nma_content(),
             "faqs" = faqs_content(),
             "citation" = citation_content(),
             getting_started_content()
      )
    })
    
  })
}

# =============================================================================
# Content Functions
# =============================================================================

getting_started_content <- function() {
  tagList(
    div(
      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
               padding: 40px; border-radius: 12px; color: white; margin-bottom: 40px;",
      h1(icon("rocket"), " Getting Started with MetaSuite", style = "margin: 0 0 15px 0; font-weight: 700;"),
      p("Your complete guide to evidence synthesis in minutes", 
        style = "font-size: 18px; margin: 0; opacity: 0.95;")
    ),
    
    h2("What is MetaSuite?", style = "color: #1d3557; margin-top: 40px;"),
    p("MetaSuite is a comprehensive web-based toolkit designed to streamline evidence synthesis workflows. 
      It provides powerful tools for risk of bias assessment, meta-analysis, and network meta-analysis, 
      all in one integrated platform."),
    
    h3("Key Features", style = "color: #1d3557; margin-top: 30px;"),
    div(
      style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin: 25px 0;",
      
      div(
        style = "background: white; padding: 25px; border-radius: 8px; border-left: 4px solid #667eea; box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
        h4(icon("shield-halved", style = "color: #667eea;"), " Risk of Bias Assessment", 
           style = "color: #1d3557; margin-top: 0;"),
        tags$ul(
          tags$li("Support for 5 major ROB tools"),
          tags$li("Interactive visualizations"),
          tags$li("Editable data tables"),
          tags$li("High-resolution plot exports")
        )
      ),
      
      div(
        style = "background: white; padding: 25px; border-radius: 8px; border-left: 4px solid #20B2AA; box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
        h4(icon("chart-line", style = "color: #20B2AA;"), " Meta-Analysis", 
           style = "color: #1d3557; margin-top: 0;"),
        tags$ul(
          tags$li("Forest plots"),
          tags$li("Funnel plots"),
          tags$li("Subgroup analysis"),
          tags$li("Publication bias assessment")
        ),
        tags$span("Coming Soon", 
                  style = "background: #fff3cd; color: #856404; padding: 4px 10px; border-radius: 8px; font-size: 12px; font-weight: 600;")
      )
    ),
    
    h3("Quick Start (5 Minutes)", style = "color: #1d3557; margin-top: 40px;"),
    
    create_step_card(1, "Navigate to Module", 
                     "Click on 'Risk of Bias' in the main navigation to get started with ROB assessment."),
    create_step_card(2, "Load Data", 
                     "Choose a sample dataset or upload your own Excel/CSV file with ROB assessments."),
    create_step_card(3, "Generate Visualizations", 
                     "Click 'Generate Plots' to create summary, traffic light, and heatmap visualizations."),
    create_step_card(4, "Customize & Export", 
                     "Adjust colors, fonts, and layout. Download high-resolution plots for publications.")
  )
}

rob_content <- function() {
  tagList(
    # Header
    div(
      style = "background: linear-gradient(135deg, #457b9d 0%, #1d3557 100%); 
               padding: 40px; border-radius: 12px; color: white; margin-bottom: 40px;",
      h1(icon("shield-halved"), " Risk of Bias Assessment", 
         style = "margin: 0 0 15px 0; font-weight: 700;"),
      p("Complete guide to ROB assessment in MetaSuite", 
        style = "font-size: 18px; margin: 0; opacity: 0.95;")
    ),
    
    # Overview Section
    h2("Overview", style = "color: #1d3557; margin-top: 30px;"),
    div(
      style = "background: #E8F4F8; padding: 25px; border-radius: 8px; margin: 20px 0;",
      p("MetaSuite provides comprehensive support for Risk of Bias assessment across multiple study types. 
        The module includes powerful visualization tools and data management capabilities.")
    ),
    
    # Supported Tools
    h3("Supported ROB Tools", style = "color: #1d3557; margin-top: 40px;"),
    p("MetaSuite supports five major risk of bias assessment frameworks:"),
    
    tags$table(
      class = "table table-bordered",
      style = "background: white; margin: 25px 0;",
      tags$thead(
        style = "background: #1d3557; color: white;",
        tags$tr(
          tags$th("Tool", style = "padding: 15px;"),
          tags$th("Study Type", style = "padding: 15px;"),
          tags$th("Domains", style = "padding: 15px;"),
          tags$th("Risk Categories", style = "padding: 15px;")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td(tags$strong("ROB 2"), style = "padding: 12px;"),
          tags$td("Randomized Trials", style = "padding: 12px;"),
          tags$td("5 + Overall", style = "padding: 12px;"),
          tags$td("Low, Some Concerns, High", style = "padding: 12px;")
        ),
        tags$tr(
          tags$td(tags$strong("ROBINS-I"), style = "padding: 12px;"),
          tags$td("Non-randomized Interventions", style = "padding: 12px;"),
          tags$td("7 + Overall", style = "padding: 12px;"),
          tags$td("Low, Moderate, Serious, Critical", style = "padding: 12px;")
        ),
        tags$tr(
          tags$td(tags$strong("ROBINS-E"), style = "padding: 12px;"),
          tags$td("Non-randomized Exposures", style = "padding: 12px;"),
          tags$td("7 + Overall", style = "padding: 12px;"),
          tags$td("Low, Some Concerns, High, Very High", style = "padding: 12px;")
        ),
        tags$tr(
          tags$td(tags$strong("QUADAS-2"), style = "padding: 12px;"),
          tags$td("Diagnostic Accuracy", style = "padding: 12px;"),
          tags$td("4 + Overall", style = "padding: 12px;"),
          tags$td("Low, High, Unclear", style = "padding: 12px;")
        ),
        tags$tr(
          tags$td(tags$strong("QUIPS"), style = "padding: 12px;"),
          tags$td("Prognosis Studies", style = "padding: 12px;"),
          tags$td("6 + Overall", style = "padding: 12px;"),
          tags$td("Low, Moderate, High", style = "padding: 12px;")
        )
      )
    ),
    
    # Data Format
    h3("Data Format Requirements", style = "color: #1d3557; margin-top: 40px;"),
    p("Your dataset must follow a specific column structure. Each row represents one study, 
      and columns represent domains plus an overall assessment."),
    
    div(
      style = "background: #f8f9fa; padding: 20px; border-radius: 8px; margin: 20px 0; border-left: 4px solid #667eea;",
      h5("Required Columns:", style = "color: #1d3557; margin-top: 0;"),
      tags$ul(
        tags$li(tags$code("Study"), " - Study identifier (e.g., 'Smith 2020')"),
        tags$li(tags$code("D1, D2, D3..."), " - Domain assessments with risk values"),
        tags$li(tags$code("Overall"), " - Overall risk of bias judgment")
      ),
      p(
        icon("info-circle", style = "color: #667eea;"),
        " For detailed format specifications, click the ",
        tags$strong("'Data Format Guide'"),
        " button in the Risk of Bias module.",
        style = "margin: 15px 0 0 0; color: #5A6169;"
      )
    ),
    
    # Features
    h3("Visualization Features", style = "color: #1d3557; margin-top: 40px;"),
    
    div(
      style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 20px; margin: 25px 0;",
      
      # Summary Plot
      div(
        style = "background: white; padding: 25px; border-radius: 8px; border-left: 4px solid #667eea; box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
        h4(icon("chart-bar", style = "color: #667eea;"), " Summary Plot", 
           style = "color: #1d3557; margin-top: 0;"),
        p("Stacked bar chart showing the percentage distribution of risk levels across all domains."),
        tags$ul(
          style = "font-size: 14px; color: #5A6169;",
          tags$li("Domain-level aggregation"),
          tags$li("Color-coded risk categories"),
          tags$li("Study counts and percentages")
        )
      ),
      
      # Traffic Light Plot
      div(
        style = "background: white; padding: 25px; border-radius: 8px; border-left: 4px solid #764ba2; box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
        h4(icon("traffic-light", style = "color: #764ba2;"), " Traffic Light Plot", 
           style = "color: #1d3557; margin-top: 0;"),
        p("Study-by-domain grid with color-coded risk symbols for pattern identification."),
        tags$ul(
          style = "font-size: 14px; color: #5A6169;",
          tags$li("Individual study assessment"),
          tags$li("Symbolic representations"),
          tags$li("Domain name captions")
        )
      ),
      
      # Heatmap Plot
      div(
        style = "background: white; padding: 25px; border-radius: 8px; border-left: 4px solid #20B2AA; box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
        h4(icon("th", style = "color: #20B2AA;"), " Heatmap Plot", 
           style = "color: #1d3557; margin-top: 0;"),
        p("Numeric visualization with row and column averages for quantitative assessment."),
        tags$ul(
          style = "font-size: 14px; color: #5A6169;",
          tags$li("Continuous color scale"),
          tags$li("Study-level averages"),
          tags$li("Domain-level averages")
        )
      )
    ),
    
    # Data Table Features
    h3("Data Table Features", style = "color: #1d3557; margin-top: 40px;"),
    div(
      style = "background: white; padding: 25px; border-radius: 8px; margin: 20px 0;",
      tags$ul(
        style = "line-height: 2;",
        tags$li(icon("table"), " Interactive table view with sorting and filtering"),
        tags$li(icon("edit"), " In-place cell editing (toggle edit mode)"),
        tags$li(icon("download"), " Export data as CSV or Excel"),
        tags$li(icon("search"), " Global search across all columns"),
        tags$li(icon("eye"), " Show all rows or paginated view")
      )
    ),
    
    # Customization
    h3("Customization Options", style = "color: #1d3557; margin-top: 40px;"),
    p("Use the Plot Customization panel in the sidebar to adjust:"),
    
    div(
      style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin: 25px 0;",
      
      div(
        style = "background: #f8f9fa; padding: 20px; border-radius: 8px;",
        h5(icon("palette"), " Colors", style = "color: #1d3557;"),
        p("Customize colors for each risk level using the color pickers")
      ),
      
      div(
        style = "background: #f8f9fa; padding: 20px; border-radius: 8px;",
        h5(icon("text-height"), " Typography", style = "color: #1d3557;"),
        p("Adjust font sizes for plot text and labels")
      ),
      
      div(
        style = "background: #f8f9fa; padding: 20px; border-radius: 8px;",
        h5(icon("toggle-on"), " Display Options", style = "color: #1d3557;"),
        p("Toggle domain names and overall risk display")
      ),
      
      div(
        style = "background: #f8f9fa; padding: 20px; border-radius: 8px;",
        h5(icon("download"), " Export", style = "color: #1d3557;"),
        p("Download high-resolution PNG or JPG plots")
      )
    ),
    
    # Tips Section
    div(
      style = "background: #FFF3CD; padding: 25px; border-radius: 8px; margin-top: 40px; border-left: 4px solid #F39C12;",
      h4(icon("lightbulb"), " Pro Tips", style = "color: #856404; margin-top: 0;"),
      tags$ul(
        style = "color: #856404; margin: 0; line-height: 1.8;",
        tags$li("Use sample datasets to explore features before uploading your data"),
        tags$li("Adjust colors to match journal requirements before exporting"),
        tags$li("Enable edit mode to make quick corrections without re-uploading"),
        tags$li("Download plots at 300 DPI for publication-ready quality"),
        tags$li("Domain names are automatically mapped from ROB tool specifications")
      )
    )
  )
}

meta_content <- function() {
  tagList(
    div(
      style = "background: linear-gradient(135deg, #29AB87 0%, #20B2AA 100%); 
               padding: 40px; border-radius: 12px; color: white; margin-bottom: 40px; text-align: center;",
      h1(icon("chart-line"), " Meta-Analysis", style = "margin: 0 0 15px 0; font-weight: 700;"),
      p("Advanced meta-analysis features", 
        style = "font-size: 18px; margin: 0 0 20px 0; opacity: 0.95;"),
      tags$span("Coming Soon", 
                style = "background: rgba(255,255,255,0.3); color: white; padding: 8px 20px; 
                         border-radius: 20px; font-size: 14px; font-weight: 600;")
    ),
    
    h3("Planned Features", style = "color: #1d3557;"),
    tags$ul(
      style = "line-height: 2;",
      tags$li("Forest plot generation"),
      tags$li("Random and fixed-effects models"),
      tags$li("Heterogeneity assessment (I², τ²)"),
      tags$li("Subgroup and sensitivity analysis"),
      tags$li("Publication bias detection (funnel plots, Egger's test)"),
      tags$li("Meta-regression")
    )
  )
}

nma_content <- function() {
  tagList(
    div(
      style = "background: linear-gradient(135deg, #fa709a 0%, #fee140 100%); 
               padding: 40px; border-radius: 12px; color: white; margin-bottom: 40px; text-align: center;",
      h1(icon("diagram-project"), " Network Meta-Analysis", style = "margin: 0 0 15px 0; font-weight: 700;"),
      p("Network meta-analysis and treatment ranking", 
        style = "font-size: 18px; margin: 0 0 20px 0; opacity: 0.95;"),
      tags$span("Coming Soon", 
                style = "background: rgba(255,255,255,0.3); color: white; padding: 8px 20px; 
                         border-radius: 20px; font-size: 14px; font-weight: 600;")
    ),
    
    h3("Planned Features", style = "color: #1d3557;"),
    tags$ul(
      style = "line-height: 2;",
      tags$li("Network plot visualization"),
      tags$li("League tables"),
      tags$li("Treatment ranking (SUCRA scores)"),
      tags$li("Consistency and inconsistency assessment"),
      tags$li("Node-splitting analysis"),
      tags$li("Contribution plots")
    )
  )
}

faqs_content <- function() {
  tagList(
    h1(icon("circle-question"), " Frequently Asked Questions", 
       style = "color: #1d3557; margin-bottom: 30px;"),
    
    # FAQ 1
    div(
      style = "background: white; padding: 25px; border-radius: 8px; margin-bottom: 20px; border-left: 4px solid #667eea; box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
      h4(icon("question-circle", style = "color: #667eea;"), " What file formats are supported?", 
         style = "color: #1d3557; margin-top: 0;"),
      p("MetaSuite accepts Excel (.xlsx, .xls) and CSV (.csv) files. Ensure your data follows 
        the format guidelines available via the 'Data Format Guide' button in the Risk of Bias module.")
    ),
    
    # FAQ 2
    div(
      style = "background: white; padding: 25px; border-radius: 8px; margin-bottom: 20px; border-left: 4px solid #764ba2; box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
      h4(icon("question-circle", style = "color: #764ba2;"), " Can I customize plot colors?", 
         style = "color: #1d3557; margin-top: 0;"),
      p("Yes! Use the 'Plot Customization' accordion in the left sidebar. You can adjust colors for 
        each risk level, modify font sizes, and toggle various display options. Changes apply in real-time.")
    ),
    
    # FAQ 3
    div(
      style = "background: white; padding: 25px; border-radius: 8px; margin-bottom: 20px; border-left: 4px solid #20B2AA; box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
      h4(icon("question-circle", style = "color: #20B2AA;"), " How do I edit my data?", 
         style = "color: #1d3557; margin-top: 0;"),
      p("Navigate to the 'Data Table' tab, click 'Enable Editing', then click any cell to modify 
        its value. Changes are saved automatically in your session and will be reflected in plots 
        when you regenerate them.")
    ),
    
    # FAQ 4
    div(
      style = "background: white; padding: 25px; border-radius: 8px; margin-bottom: 20px; border-left: 4px solid #fa709a; box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
      h4(icon("question-circle", style = "color: #fa709a;"), " What resolution are exported plots?", 
         style = "color: #1d3557; margin-top: 0;"),
      p("Plots are exported at 300 DPI (high resolution) in PNG or JPG format, making them suitable 
        for publication in academic journals and presentations.")
    ),
    
    # FAQ 5
    div(
      style = "background: white; padding: 25px; border-radius: 8px; margin-bottom: 20px; border-left: 4px solid #667eea; box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
      h4(icon("question-circle", style = "color: #667eea;"), " Is my data stored on the server?", 
         style = "color: #1d3557; margin-top: 0;"),
      p("No. All data processing happens in your browser session. Your data is never uploaded to or 
        stored on any server, and is completely deleted when you close the application.")
    ),
    
    # FAQ 6
    div(
      style = "background: white; padding: 25px; border-radius: 8px; margin-bottom: 20px; border-left: 4px solid #764ba2; box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
      h4(icon("question-circle", style = "color: #764ba2;"), " Can I use MetaSuite offline?", 
         style = "color: #1d3557; margin-top: 0;"),
      p("Currently, MetaSuite requires an internet connection to load the application. However, 
        a standalone desktop version is planned for future release.")
    ),
    
    # FAQ 7
    div(
      style = "background: white; padding: 25px; border-radius: 8px; margin-bottom: 20px; border-left: 4px solid #20B2AA; box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
      h4(icon("question-circle", style = "color: #20B2AA;"), " How do I report bugs or request features?", 
         style = "color: #1d3557; margin-top: 0;"),
      p("Please contact the development team through your institutional support channels or submit 
        an issue through the project repository if available.")
    )
  )
}

citation_content <- function() {
  tagList(
    h1(icon("quote-left"), " How to Cite MetaSuite", 
       style = "color: #1d3557; margin-bottom: 30px;"),
    
    p("If you use MetaSuite in your research, please cite it as follows:"),
    
    # APA Citation
    div(
      style = "background: #f8f9fa; padding: 25px; border-radius: 8px; margin: 25px 0; border-left: 4px solid #1d3557;",
      h4("APA Format", style = "color: #1d3557; margin-top: 0;"),
      p(
        style = "font-family: 'Georgia', serif; line-height: 1.8; margin: 0;",
        "MetaSuite Collaborative. (2025). ", 
        tags$em("MetaSuite: A comprehensive toolkit for evidence synthesis and meta-analysis"), 
        " (Version 1.0.0) [Computer software]. https://metasuite.org"
      )
    ),
    
    # BibTeX
    h4("BibTeX Entry", style = "color: #1d3557; margin-top: 40px;"),
    div(
      style = "background: #2d2d2d; padding: 25px; border-radius: 8px; margin: 20px 0;",
      tags$pre(
        style = "color: #f8f9fa; margin: 0; overflow-x: auto; font-size: 13px;",
        "@software{metasuite2025,
  author = {{MetaSuite Collaborative}},
  title = {MetaSuite: A Comprehensive Toolkit for Evidence Synthesis},
  year = {2025},
  version = {1.0.0},
  url = {https://metasuite.org}
}"
      )
    ),
    
    # License
    div(
      style = "background: #E8F4F8; padding: 25px; border-radius: 8px; margin: 40px 0 0 0;",
      h4(icon("scale-balanced", style = "color: #457b9d;"), " License Information", 
         style = "color: #1d3557; margin-top: 0;"),
      p("MetaSuite is released under the MIT License. You are free to use, modify, and distribute 
        this software for academic and commercial purposes with proper attribution.")
    )
  )
}

# Helper function for step cards
create_step_card <- function(number, title, description) {
  div(
    style = "background: #f8f9fa; padding: 25px; border-radius: 8px; margin-bottom: 20px; 
             border-left: 4px solid #667eea;",
    h4(
      tags$span(
        number,
        style = "display: inline-block; width: 35px; height: 35px; line-height: 35px; 
                 text-align: center; background: #667eea; color: white; border-radius: 50%; 
                 margin-right: 15px; font-weight: 700;"
      ),
      title,
      style = "color: #1d3557; margin-top: 0;"
    ),
    p(description, style = "margin: 0; color: #5A6169;")
  )
}
