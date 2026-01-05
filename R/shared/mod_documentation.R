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
      
      .badge-complete {
        background: #d4edda;
        color: #155724;
      }"
      )),
    
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
          tags$span("Complete", class = "doc-section-badge badge-complete")
        ),
        
        div(
          class = "doc-nav-item",
          id = ns("nav_nma"),
          onclick = sprintf("Shiny.setInputValue('%s', 'nma', {priority: 'event'})", ns("section")),
          icon("diagram-project"),
          " Network Meta-Analysis",
          tags$span("Complete", class = "doc-section-badge badge-complete")
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
      style = "background: linear-gradient(135deg, #457b9d 0%, #1d3557 100%); 
               padding: 40px; border-radius: 12px; color: white; margin-bottom: 40px;",
      h1(icon("rocket"), " Getting Started with MetaSuite", style = "margin: 0 0 15px 0; font-weight: 700; color: #f1faee !important;"),
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
          tags$li("Interactive risk-of-bias visualizations"),
          tags$li("Editable study-level data tables"),
          tags$li("Domain and study summaries"),
          tags$li("Publication-ready plot exports")
        )
      ),
      
      div(
        style = "background: white; padding: 25px; border-radius: 8px; border-left: 4px solid #20B2AA; box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
        h4(icon("chart-line", style = "color: #20B2AA;"), " Meta-Analysis", 
           style = "color: #1d3557; margin-top: 0;"),
        tags$ul(
          tags$li("Forest plots (fixed and random effects)"),
          tags$li("Funnel plots and bias assessment"),
          tags$li("Subgroup and sensitivity analyses"),
          tags$li("Heterogeneity and influence diagnostics"),
          tags$li("Publication-ready plot exports")
        )
      ),
      div(
        style = "background: white; padding: 25px; border-radius: 8px; border-left: 4px solid #D79730; box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
        h4(icon("diagram-project", style = "color: #D79730;"), " Network Meta-Analysis", 
           style = "color: #1d3557; margin-top: 0;"),
        tags$ul(
          tags$li("Multiple-treatment comparisons in a network"),
          tags$li("Consistency and evidence flow diagnostics"),
          tags$li("Treatment ranking with SUCRA"),
          tags$li("Advanced rank visualizations"),
          tags$li("Publication-ready plot exports")
        )
      )
    ),
    
    h3("Quick Start", style = "color: #1d3557; margin-top: 40px;"),
    
    h4(icon("shield-halved", style = "color: #1d3557;")," Risk of Bias Assessment",
       style = "color: #1d3557; margin-top: 40px;"),
    
    create_step_card(1, "Navigate to Module", 
                     "Click on 'Risk of Bias' in the main navigation to get started with ROB assessment."),
    create_step_card(2, "Load Data", 
                     "Choose a sample dataset or upload your own Excel/CSV file with ROB assessments."),
    create_step_card(3, "Generate Visualizations", 
                     "Click 'Generate Plots' to create summary, traffic light, and heatmap visualizations."),
    create_step_card(4, "Customize & Export", 
                     "Adjust plot appearance through 'Plot Customization' and export high-resolution figures and results tables."),
    
    h4(icon("chart-line", style = "color: #1d3557;"), " Meta-Analysis",
       style = "color: #1d3557; margin-top: 40px;"),
    create_step_card(1, "Navigate to Module", 
                     "Click on 'Meta-Analysis' in the main navigation to start meta analysis."),
    create_step_card(2, "Load Data", 
                     "Choose a sample dataset or upload your own Excel/CSV file with study-level data."),
    create_step_card(3, "Generate Results and Visualizations", 
                     "Click 'Run Analysis' to generate results, forest plots, pooled estimates, and diagnostics."),
    create_step_card(4, "Customize & Export", 
                     "Adjust plot appearance through 'Plot Customization' and export high-resolution figures and results tables."),
    
    h4(icon("diagram-project", style = "color: #1d3557;"), " Network Meta-Analysis",
       style = "color: #1d3557; margin-top: 40px;"),
    
    create_step_card(1, "Navigate to Module", 
                     "Click on 'Network Meta-Analysis' in the main navigation to begin network analysis."),
    create_step_card(2, "Load Data", 
                     "Choose a sample dataset or upload arm-level Excel/CSV data with treatment information."),
    create_step_card(3, "Generate Results and Visualizations", 
                     "Click 'Run Analysis' to generate network estimates, diagnostics, and rankings."),
    create_step_card(4, "Customize & Export", 
                     "Adjust plot appearance through 'Plot Customization' and export high-resolution figures and results tables.")
  )
}

rob_content <- function() {
  tagList(
    # Header
    div(
      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
               padding: 40px; border-radius: 12px; color: white; margin-bottom: 40px;",
      h1(icon("shield-halved"), " Risk of Bias Assessment", 
         style = "margin: 0 0 15px 0; font-weight: 700;"),
      p("Complete guide to Risk of Bias assessment in MetaSuite", 
        style = "font-size: 18px; margin: 0; opacity: 0.95;")
    ),
    
    # Overview Section
    h2("Overview", style = "color: #1d3557; margin-top: 30px;"),
    div(
      style = "background: #E8F4F8; padding: 25px; border-radius: 8px; margin: 20px 0;",
      p("MetaSuite provides comprehensive support for Risk of Bias assessment across multiple study types. 
        The module includes comprehensive visualization tools and data management capabilities.")
    ),
    
    # ==========================================================================
    # SUPPORTED ROB TOOLS
    # ==========================================================================
    
    h2("Supported ROB Tools", style = "color: #1d3557; margin-top: 50px; 
                                       border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    p("MetaSuite supports five major Risk of Bias assessment frameworks:"),
    
    div(
      style = "overflow-x: auto; margin: 25px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white;",
        tags$thead(
          style = "background: #1d3557; color: white;",
          tags$tr(
            tags$th("Tool", style = "padding: 12px;"),
            tags$th("Study Type", style = "padding: 12px;"),
            tags$th("Domains", style = "padding: 12px;"),
            tags$th("Risk Categories", style = "padding: 12px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$strong("ROB 2"), style = "padding: 10px;"),
            tags$td("Randomized Trials", style = "padding: 10px;"),
            tags$td("5 + Overall", style = "padding: 10px;"),
            tags$td("Low, Some Concerns, High", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(tags$strong("ROBINS-I"), style = "padding: 10px;"),
            tags$td("Non-randomized Interventions", style = "padding: 10px;"),
            tags$td("7 + Overall", style = "padding: 10px;"),
            tags$td("Low, Moderate, Serious, Critical", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(tags$strong("ROBINS-E"), style = "padding: 10px;"),
            tags$td("Non-randomized Exposures", style = "padding: 10px;"),
            tags$td("7 + Overall", style = "padding: 10px;"),
            tags$td("Low, Some Concerns, High, Very High", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(tags$strong("QUADAS-2"), style = "padding: 10px;"),
            tags$td("Diagnostic Accuracy", style = "padding: 10px;"),
            tags$td("4 + Overall", style = "padding: 10px;"),
            tags$td("Low, High, Unclear", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(tags$strong("QUIPS"), style = "padding: 10px;"),
            tags$td("Prognosis Studies", style = "padding: 10px;"),
            tags$td("6 + Overall", style = "padding: 10px;"),
            tags$td("Low, Moderate, High", style = "padding: 10px;")
          )
        )
      )
    ),
    
    # ==========================================================================
    # RISK OF BIAS DOMAIN REFERENCE
    # ==========================================================================
    
    h2("Risk of Bias Domain Reference", style = "color: #1d3557; margin-top: 50px; 
                                                  border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    div(
      style = "overflow-x: auto; margin: 25px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white; font-size: 13px;",
        tags$thead(
          style = "background: #1d3557; color: white;",
          tags$tr(
            tags$th("QUADAS-2", style = "padding: 10px;"),
            tags$th("QUIPS", style = "padding: 10px;"),
            tags$th("ROB 2", style = "padding: 10px;"),
            tags$th("ROBINS-I", style = "padding: 10px;"),
            tags$th("ROBINS-E", style = "padding: 10px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Patient selection", style = "padding: 8px;"),
            tags$td("Bias due to participation", style = "padding: 8px;"),
            tags$td("Bias arising from randomization process", style = "padding: 8px;"),
            tags$td("Bias due to confounding", style = "padding: 8px;"),
            tags$td("Bias due to confounding", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td("Index test", style = "padding: 8px;"),
            tags$td("Bias due to attrition", style = "padding: 8px;"),
            tags$td("Bias due to deviations from intended interventions", style = "padding: 8px;"),
            tags$td("Bias due to selection of participants", style = "padding: 8px;"),
            tags$td("Bias arising from measurement of the exposure", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td("Reference standard", style = "padding: 8px;"),
            tags$td("Bias due to prognostic factor measurement", style = "padding: 8px;"),
            tags$td("Bias due to missing outcome data", style = "padding: 8px;"),
            tags$td("Bias in classification of interventions", style = "padding: 8px;"),
            tags$td("Bias in selection of participants into the study (or into the analysis)", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td("Flow & timing", style = "padding: 8px;"),
            tags$td("Bias due to outcome measurement", style = "padding: 8px;"),
            tags$td("Bias in measurement of the outcome", style = "padding: 8px;"),
            tags$td("Bias due to deviations from intended interventions", style = "padding: 8px;"),
            tags$td("Bias due to post-exposure interventions", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td("Overall", style = "padding: 8px;"),
            tags$td("Bias due to confounding", style = "padding: 8px;"),
            tags$td("Bias in selection of the reported results", style = "padding: 8px;"),
            tags$td("Bias due to missing data", style = "padding: 8px;"),
            tags$td("Bias due to missing data", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td("", style = "padding: 8px;"),
            tags$td("Bias in statistical analysis and reporting", style = "padding: 8px;"),
            tags$td("Overall", style = "padding: 8px;"),
            tags$td("Bias in measurement of outcomes", style = "padding: 8px;"),
            tags$td("Bias arising from measurement of the outcome", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td("", style = "padding: 8px;"),
            tags$td("Overall", style = "padding: 8px;"),
            tags$td("", style = "padding: 8px;"),
            tags$td("Bias in selection of the reported result", style = "padding: 8px;"),
            tags$td("Bias in selection of the reported result", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td("", style = "padding: 8px;"),
            tags$td("", style = "padding: 8px;"),
            tags$td("", style = "padding: 8px;"),
            tags$td("Overall", style = "padding: 8px;"),
            tags$td("Overall", style = "padding: 8px;")
          )
        )
      )
    ),
    
    # ==========================================================================
    # DATA FORMAT REQUIREMENTS
    # ==========================================================================
    
    h2("Data Format Requirements", style = "color: #1d3557; margin-top: 50px; 
                                            border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    p("Your dataset must follow a specific column structure. Each row represents one study, 
      and columns represent domains plus an overall assessment."),
    
    div(
      style = "background: #f8f9fa; padding: 20px; border-radius: 8px; margin: 20px 0; 
               border-left: 4px solid #667eea;",
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
    
    # ==========================================================================
    # DATA TABLE FEATURES
    # ==========================================================================
    
    h2("Data Table Features", style = "color: #1d3557; margin-top: 50px; 
                                       border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
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
    
    # ==========================================================================
    # VISUALIZATION FEATURES
    # ==========================================================================
    
    h2("Visualization Features", style = "color: #1d3557; margin-top: 50px; 
                                          border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    h3("Risk-of-Bias Plot Interpretation", style = "color: #1d3557; margin-top: 30px;"),
    
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white;",
        tags$thead(
          style = "background: #457b9d; color: white;",
          tags$tr(
            tags$th("Plot", style = "padding: 12px;"),
            tags$th("What It Shows", style = "padding: 12px;"),
            tags$th("How to Read", style = "padding: 12px;"),
            tags$th("Best Use", style = "padding: 12px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(strong("Weighted Bar Plot"), style = "padding: 10px;"),
            tags$td("Distribution of risk judgments across domains", style = "padding: 10px;"),
            tags$td("Larger segments indicate higher contribution of that risk level", 
                    style = "padding: 10px;"),
            tags$td("Domain-level overview", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Traffic Light Plot"), style = "padding: 10px;"),
            tags$td("Study-by-domain risk matrix", style = "padding: 10px;"),
            tags$td("Scan rows for study profiles, columns for domain issues", 
                    style = "padding: 10px;"),
            tags$td("Study-level transparency", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Heatmap Plot"), style = "padding: 10px;"),
            tags$td("Numeric risk patterns with averages", style = "padding: 10px;"),
            tags$td("Row/column averages highlight high-risk studies or domains", 
                    style = "padding: 10px;"),
            tags$td("Pattern detection and prioritization", style = "padding: 10px;")
          )
        )
      )
    ),
    
    h3("Detailed Plot Interpretation", style = "color: #1d3557; margin-top: 40px;"),
    
    tags$ul(
      style = "line-height: 2;",
      tags$li(
        strong("Weighted Bar Plot:"), 
        " Provides a high-level summary of risk-of-bias judgments across all studies. 
        Each bar represents a domain, with color segments showing the proportion of ",
        tags$em("Low"), ", ", tags$em("Some Concerns"), ", or ", tags$em("High-risk"), 
        " judgments (optionally weighted).",
        tags$ul(
          tags$li("Use this plot to:"),
          tags$ul(
            tags$li("Identify domains with consistently high risk"),
            tags$li("Communicate overall quality in reports or presentations")
          )
        )
      ),
      tags$li(
        strong("Traffic Light Plot:"), 
        " Displays every individual judgment in a matrix format. Rows represent studies 
        and columns represent domains, mirroring the underlying dataset structure.",
        tags$ul(
          tags$li("Use this plot to:"),
          tags$ul(
            tags$li("Inspect individual study risk profiles"),
            tags$li("Support transparent reporting and audit trails")
          )
        )
      ),
      tags$li(
        strong("Heatmap Plot:"), 
        " Provides a quantitative overview of risk-of-bias patterns, including 
        study-level averages (row averages), domain-level averages (column averages), 
        and optional overall scores.",
        tags$ul(
          tags$li("Use this plot to:"),
          tags$ul(
            tags$li("Detect patterns and gradients"),
            tags$li("Identify outlier studies or dominant bias domains"),
            tags$li("Support sensitivity and subgroup decisions")
          )
        )
      )
    ),
    
    # ==========================================================================
    # PLOT CUSTOMIZATION OPTIONS
    # ==========================================================================
    
    h2("Plot Customization Options", style = "color: #1d3557; margin-top: 50px; 
                                              border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white; font-size: 13px;",
        tags$thead(
          style = "background: #667eea; color: white;",
          tags$tr(
            tags$th("Category", style = "padding: 10px;"),
            tags$th("Applies To", style = "padding: 10px;"),
            tags$th("Customizable Elements", style = "padding: 10px;"),
            tags$th("Notes / Best Practice", style = "padding: 10px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(strong("Color Customization"), style = "padding: 8px;"),
            tags$td("Weighted Bar Plot", style = "padding: 8px;"),
            tags$td("Risk Colors", style = "padding: 8px;"),
            tags$td("Use high contrast", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Color Customization"), style = "padding: 8px;"),
            tags$td("Traffic Light Plot", style = "padding: 8px;"),
            tags$td("Risk Colors", style = "padding: 8px;"),
            tags$td("Use high contrast", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Color Customization"), style = "padding: 8px;"),
            tags$td("Heatmap Plot", style = "padding: 8px;"),
            tags$td("Risk Colors", style = "padding: 8px;"),
            tags$td("Use high contrast", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Font Size Guidance"), style = "padding: 8px;"),
            tags$td("All plots", style = "padding: 8px;"),
            tags$td("Scale text up for presentations; down for publications", 
                    style = "padding: 8px;"),
            tags$td("Adjust for viewing distance", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Export Format"), style = "padding: 8px;"),
            tags$td("All plots", style = "padding: 8px;"),
            tags$td("PNG (transparent), JPG (white background)", style = "padding: 8px;"),
            tags$td("PNG preferred for publications", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Export Quality"), style = "padding: 8px;"),
            tags$td("All plots", style = "padding: 8px;"),
            tags$td("300 DPI resolution", style = "padding: 8px;"),
            tags$td("Publication-ready", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Export Dimensions"), style = "padding: 8px;"),
            tags$td("All plots", style = "padding: 8px;"),
            tags$td("Auto-optimized (≈6×4 in default)", style = "padding: 8px;"),
            tags$td("Fits standard journals", style = "padding: 8px;")
          )
        )
      )
    ),
    
    # ==========================================================================
    # ANALYSIS WORKFLOW
    # ==========================================================================
    
    h2("Analysis Workflow", style = "color: #1d3557; margin-top: 50px; 
                                     border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    p("MetaSuite follows a structured 5-step workflow for comprehensive Risk of Bias assessment:"),
    
    create_step_card(1, "Load Your Data",
                     "Click 'Select Dataset' for sample data OR 'Upload File' for Excel/CSV. 
                     File must contain Study column, domain columns (D1, D2, D3...), and Overall column. 
                     Risk values must match selected ROB tool's categories."),
    
    create_step_card(2, "Select ROB Tool",
                     "Choose appropriate tool: ROB 2 (RCTs), ROBINS-I (Non-randomized Interventions), 
                     ROBINS-E (Non-randomized Exposures), QUADAS-2 (Diagnostic Accuracy), or QUIPS (Prognosis). 
                     Tool selection determines domain mapping and valid risk categories."),
    
    create_step_card(3, "Review Data Table",
                     "Navigate to Data Table tab. Review all assessments for accuracy. 
                     Enable 'Edit Mode' for on-the-fly corrections. Export as CSV/Excel to preserve edits."),
    
    create_step_card(4, "Customize Visualizations",
                     "Navigate to Plots tab. Use Plot Customization panel to adjust colors, typography, 
                     and display options. Preview changes in real-time. Available plots: Summary, Traffic Light, Heatmap."),
    
    create_step_card(5, "Export Results",
                     "Select plot and format (PNG/JPG). Click Download button. All plots exported at 300 DPI. 
                     Export data table as CSV/Excel to preserve manual edits."),
    
    # ==========================================================================
    # PRO TIPS
    # ==========================================================================
    
    div(
      style = "background: #FFF3CD; padding: 25px; border-radius: 8px; margin-top: 40px; 
               border-left: 4px solid #F39C12;",
      h4(icon("lightbulb"), " Pro Tips", style = "color: #856404; margin-top: 0;"),
      tags$ul(
        style = "color: #856404; margin: 0; line-height: 1.8;",
        tags$li("Use sample datasets to explore features before uploading your data"),
        tags$li("Adjust colors to match journal requirements before exporting"),
        tags$li("Enable edit mode to make quick corrections without re-uploading"),
        tags$li("Download plots at 300 DPI for publication-ready quality"),
        tags$li("Domain names are automatically mapped from ROB tool specifications")
      )
    ),
    
    # ==========================================================================
    # TROUBLESHOOTING
    # ==========================================================================
    
    h2("Troubleshooting Guide", style = "color: #1d3557; margin-top: 50px; 
                                         border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    h3("Common Errors and Solutions", style = "color: #1d3557; margin-top: 30px;"),
    
    create_troubleshooting_item_rob(
      icon_name = "triangle-exclamation",
      icon_color = "#dc3545",
      title = 'Error: "Invalid data format"',
      causes = c("Column names don't match specifications exactly", 
                 "Data type mismatch (text in numeric column, vice versa)", 
                 "Extra spaces or special characters in headers"),
      solution = "Go through the Data Format Requirements before uploading your data"
    ),
    
    create_troubleshooting_item_rob(
      icon_name = "database",
      icon_color = "#6c757d",
      title = "Error: Missing Data",
      causes = c("Datasets with missing values (NA) in required columns"),
      solution = "Make sure dataset does not contain missing values"
    ),
    
    # ==========================================================================
    # ADDITIONAL RESOURCES
    # ==========================================================================
    
    h2("Additional Resources", style = "color: #1d3557; margin-top: 50px; 
                                        border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    h3("Key References", style = "color: #1d3557; margin-top: 30px;"),
    
    tags$ol(
      style = "line-height: 2;",
      tags$li(
        "Sterne, J. A. C., Savović, J., Page, M. J., Elbers, R. G., Blencowe, N. S., Boutron, I., ... Higgins, J. P. T. (2019). 
        RoB 2: A revised tool for assessing risk of bias in randomised trials. ",
        tags$em("BMJ"), ", 366, l4898. ",
        tags$a("https://doi.org/10.1136/bmj.l4898", 
               href = "https://doi.org/10.1136/bmj.l4898", 
               target = "_blank")
      ),
      tags$li(
        "Page, M. J., McKenzie, J. E., Bossuyt, P. M., Boutron, I., Hoffmann, T. C., Mulrow, C. D., ... Moher, D. (2021). 
        The PRISMA 2020 statement: An updated guideline for reporting systematic reviews. ",
        tags$em("BMJ"), ", 372, n71. ",
        tags$a("https://doi.org/10.1136/bmj.n71", 
               href = "https://doi.org/10.1136/bmj.n71", 
               target = "_blank")
      ),
      tags$li(
        "Whiting, P. F., Rutjes, A. W. S., Westwood, M. E., Mallett, S., Deeks, J. J., Reitsma, J. B., ... Bossuyt, P. M. (2011). 
        QUADAS-2: A revised tool for the quality assessment of diagnostic accuracy studies. ",
        tags$em("Annals of Internal Medicine"), ", 155(8), 529-536. ",
        tags$a("https://doi.org/10.7326/0003-4819-155-8-201110180-00009", 
               href = "https://doi.org/10.7326/0003-4819-155-8-201110180-00009", 
               target = "_blank")
      ),
      tags$li(
        "Sterne, J. A., Hernán, M. A., Reeves, B. C., Savović, J., Berkman, N. D., Viswanathan, M., ... Higgins, J. P. T. (2016). 
        ROBINS-I: A tool for assessing risk of bias in non-randomised studies of interventions. ",
        tags$em("BMJ"), ", 355, i4919. ",
        tags$a("https://doi.org/10.1136/bmj.i4919", 
               href = "https://doi.org/10.1136/bmj.i4919", 
               target = "_blank")
      ),
      tags$li(
        "McGuinness, L. A., & Higgins, J. P. T. (2021). 
        Risk-of-bias visualization (robvis): An R package and web app for visualizing risk-of-bias assessments. ",
        tags$em("Research Synthesis Methods"), ", 12(1), 55-61. ",
        tags$a("https://doi.org/10.1002/jrsm.1411", 
               href = "https://doi.org/10.1002/jrsm.1411", 
               target = "_blank")
      ),
      tags$li(
        "Higgins, J. P. T., Altman, D. G., Gøtzsche, P. C., Jüni, P., Moher, D., Oxman, A. D., ... Cochrane Bias Methods Group. (2011). 
        The Cochrane Collaboration's tool for assessing risk of bias in randomised trials. ",
        tags$em("BMJ"), ", 343, d5928. ",
        tags$a("https://doi.org/10.1136/bmj.d5928", 
               href = "https://doi.org/10.1136/bmj.d5928", 
               target = "_blank")
      ),
      tags$li(
        "Higgins, J. P. T., Thomas, J., Chandler, J., Cumpston, M., Li, T., Page, M. J., & Welch, V. A. (Eds.). (2023). ",
        tags$em("Cochrane Handbook for Systematic Reviews of Interventions"), " (Version 6.4). Cochrane. ",
        tags$a("https://training.cochrane.org/handbook", 
               href = "https://training.cochrane.org/handbook", 
               target = "_blank")
      ),
      tags$li(
        "Hayden, J. A., van der Windt, D. A., Cartwright, J. L., Côté, P., & Bombardier, C. (2013). 
        Assessing bias in studies of prognostic factors. ",
        tags$em("Annals of Internal Medicine"), ", 158(4), 280-286. ",
        tags$a("https://doi.org/10.7326/0003-4819-158-4-201302190-00009", 
               href = "https://doi.org/10.7326/0003-4819-158-4-201302190-00009", 
               target = "_blank")
      ),
      tags$li(
        "Morgan, R. L., Thayer, K. A., Santesso, N., Holloway, A. C., Blain, R., Eftim, S. E., ... Schünemann, H. J. (2019). 
        Evaluation of the risk of bias in non-randomized studies of exposures (ROBINS-E). ",
        tags$em("BMJ"), ", 365, l1887. ",
        tags$a("https://doi.org/10.1136/bmj.l1887", 
               href = "https://doi.org/10.1136/bmj.l1887", 
               target = "_blank")
      )
    ),
    # ==========================================================================
    # GETTING HELP SECTION (SAME STYLE AS META & NMA MODULES)
    # ==========================================================================
    h3("Getting Help", style = "color: #1d3557; margin-top: 30px;"),
    div(
      style = "background: white; padding: 25px; border-radius: 8px; margin: 20px 0;",
      tags$ul(
        style = "line-height: 2; margin: 0;",
        tags$li(icon("database"), " ", strong("Review Sample Datasets:"), 
                " Available in the Data Loader section. Demonstrates proper formatting for each data type."),
        tags$li(icon("book"), " ", strong("Data Format Guide:"), 
                " Click 'Data Format Guide' button in Risk of Bias module. 
            Shows detailed column specifications and examples."),
        tags$li(icon("circle-question"), " ", strong("Consult Documentation:"), 
                " Complete documentation available in Help tab. FAQs address common questions."),
        tags$li(icon("link"), " ", strong("Check References:"), 
                " See Key References section above. McGuinness et al. (2021) and 
            Sterne et al. (2019) recommended for comprehensive guidance.")
      )
    )
  )
}

# =============================================================================
# Helper Function for ROB Troubleshooting
# =============================================================================

create_troubleshooting_item_rob <- function(icon_name, icon_color, title, causes, solution) {
  div(
    style = "background: white; padding: 25px; border-radius: 8px; margin-bottom: 20px; 
             box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
    h4(
      icon(icon_name, style = sprintf("color: %s;", icon_color)), 
      " ", 
      title,
      style = "color: #1d3557; margin-top: 0;"
    ),
    
    div(
      style = "margin: 15px 0;",
      h5("Causes:", style = "color: #1d3557; margin-bottom: 8px; font-size: 15px;"),
      tags$ul(
        style = "color: #5A6169; margin: 0;",
        lapply(causes, function(x) tags$li(x))
      )
    ),
    
    div(
      style = "margin: 15px 0 0 0;",
      h5("Solution:", style = "color: #1d3557; margin-bottom: 8px; font-size: 15px;"),
      p(solution, style = "color: #5A6169; margin: 0;")
    )
  )
}

# =============================================================================
# Reuse create_step_card from meta documentation (if not already defined)
# =============================================================================

create_step_card <- function(step_number, title, description) {
  div(
    style = "background: white; padding: 25px; border-radius: 8px; margin-bottom: 20px; 
             border-left: 4px solid #1d3557; box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
    h4(
      div(
        style = "display: inline-block; background: #1d3557; color: white; 
                 width: 35px; height: 35px; border-radius: 50%; text-align: center; 
                 line-height: 35px; margin-right: 12px; font-weight: bold;",
        step_number
      ),
      title,
      style = "color: #1d3557; margin-top: 0; display: flex; align-items: center;"
    ),
    p(description, style = "color: #5A6169; margin: 10px 0 0 47px;")
  )
}


meta_content <- function() {
  tagList(
    # Header
    div(
      style = "background: linear-gradient(135deg, #20B2AA 0%, #29AB87 100%);
              padding: 40px; border-radius: 12px; color: white; margin-bottom: 40px;",
      h1(icon("chart-line"), " Meta-Analysis",
         style = "margin: 0 0 15px 0; font-weight: 700;"),
      p("Complete guide to meta-analysis in MetaSuite",
        style = "font-size: 18px; margin: 0; opacity: 0.95;")
    ),
    
    # Overview Section
    h2("Overview", style = "color: #1d3557; margin-top: 30px;"),
    div(
      style = "background: #E8F4F8; padding: 25px; border-radius: 8px; margin: 20px 0;",
      p("Meta Analysis module supports continuous outcomes, dichotomous outcomes, and correlation data, 
        with tools for heterogeneity assessment, subgroup analysis, publication bias detection, and 
        influence diagnostics.")
    ),
    
    # Key Capabilities
    h3("Key Capabilities", style = "color: #1d3557; margin-top: 40px;"),
    tags$ul(
      style = "line-height: 2;",
      tags$li(strong("Multiple Datasets:"), " Continuous, dichotomous, and correlation outcomes"),
      tags$li(strong("Diverse Effect Measures:"), " 9 effect size measures across three data types"),
      tags$li(strong("5 different Models:"), " REML (Restricted Maximum Likelihood), ML (Maximum Likelihood), DL (DerSimonian-Laird), FE (Fixed Effects), HS (Hunter Schmidt)"),
      tags$li(strong("Heterogeneity Assessment:"), " I², τ², Q-statistic, and visual diagnostics"),
      tags$li(strong("Publication Bias Detection:"), " Funnel plots, Egger's test, Rank correlation, Trim-and-fill"),
      tags$li(strong("Influence Diagnostics:"), " Cook's distance, Studentized residuals, Leave-one-out analysis"),
      tags$li(strong("Subgroup Analysis:"), " Categorical moderator analysis with statistical testing"),
      tags$li(strong("Publication-Quality Visualizations:"), " 300 DPI exports in PNG and JPG formats")
    ),
    
    # ==========================================================================
    # SUPPORTED DATA TYPES WITH EXACT COLUMN NAMES
    # ==========================================================================
    
    h2("Supported Data Types", style = "color: #1d3557; margin-top: 50px; 
                                        border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    p("MetaSuite supports three main data types for meta-analysis. Each requires specific columns and supports different effect measures."),
    
    # Continuous Outcomes
    h3(icon("chart-line", style = "color: #20B2AA;"), " Continuous Outcomes", 
       style = "color: #1d3557; margin-top: 30px;"),
    p(strong("Use for:"), " Continuous measurements (blood pressure, test scores, symptom severity, etc.)"),
    p(strong("Required Columns:"), " ", 
      tags$code("study, n.e, mean.e, sd.e, n.c, mean.c, sd.c, group (optional)")),
    
    # Column Descriptions Table
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-sm table-bordered",
        style = "background: white; font-size: 13px;",
        tags$thead(
          style = "background: #20B2AA; color: white;",
          tags$tr(
            tags$th("COLUMN NAME", style = "padding: 10px; text-align: center;"),
            tags$th("DESCRIPTION", style = "padding: 10px;"),
            tags$th("VALID VALUES", style = "padding: 10px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$code("study"), style = "padding: 8px; text-align: center;"),
            tags$td("Study identifier", style = "padding: 8px;"),
            tags$td('Any text (e.g., "Smith 2020")', style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("n.e"), style = "padding: 8px; text-align: center;"),
            tags$td("Sample size — experimental group", style = "padding: 8px;"),
            tags$td("Positive integer", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("mean.e"), style = "padding: 8px; text-align: center;"),
            tags$td("Mean — experimental group", style = "padding: 8px;"),
            tags$td("Any number", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("sd.e"), style = "padding: 8px; text-align: center;"),
            tags$td("Standard deviation — experimental group", style = "padding: 8px;"),
            tags$td("Positive number", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("n.c"), style = "padding: 8px; text-align: center;"),
            tags$td("Sample size — control group", style = "padding: 8px;"),
            tags$td("Positive integer", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("mean.c"), style = "padding: 8px; text-align: center;"),
            tags$td("Mean — control group", style = "padding: 8px;"),
            tags$td("Any number", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("sd.c"), style = "padding: 8px; text-align: center;"),
            tags$td("Standard deviation — control group", style = "padding: 8px;"),
            tags$td("Positive number", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("group"), style = "padding: 8px; text-align: center;"),
            tags$td("Subgroup variable (OPTIONAL)", style = "padding: 8px;"),
            tags$td("Categorical text", style = "padding: 8px;")
          )
        )
      )
    ),
    
    # Example Data Table
    p(strong("Example Continuous Data:")),
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-sm table-bordered",
        style = "background: white; font-size: 13px;",
        tags$thead(
          style = "background: #20B2AA; color: white;",
          tags$tr(
            tags$th("STUDY", style = "padding: 10px;"),
            tags$th("N.E", style = "padding: 10px; text-align: center;"),
            tags$th("MEAN.E", style = "padding: 10px; text-align: center;"),
            tags$th("SD.E", style = "padding: 10px; text-align: center;"),
            tags$th("N.C", style = "padding: 10px; text-align: center;"),
            tags$th("MEAN.C", style = "padding: 10px; text-align: center;"),
            tags$th("SD.C", style = "padding: 10px; text-align: center;"),
            tags$th("GROUP", style = "padding: 10px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Study_1", style = "padding: 8px;"),
            tags$td("14", style = "padding: 8px; text-align: center;"),
            tags$td("22.1", style = "padding: 8px; text-align: center;"),
            tags$td("0.8", style = "padding: 8px; text-align: center;"),
            tags$td("14", style = "padding: 8px; text-align: center;"),
            tags$td("13.7", style = "padding: 8px; text-align: center;"),
            tags$td("14.7", style = "padding: 8px; text-align: center;"),
            tags$td("A", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td("Study_2", style = "padding: 8px;"),
            tags$td("27", style = "padding: 8px; text-align: center;"),
            tags$td("16", style = "padding: 8px; text-align: center;"),
            tags$td("2.9", style = "padding: 8px; text-align: center;"),
            tags$td("28", style = "padding: 8px; text-align: center;"),
            tags$td("6.4", style = "padding: 8px; text-align: center;"),
            tags$td("8.6", style = "padding: 8px; text-align: center;"),
            tags$td("A", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td("Study_3", style = "padding: 8px;"),
            tags$td("22", style = "padding: 8px; text-align: center;"),
            tags$td("6.8", style = "padding: 8px; text-align: center;"),
            tags$td("1", style = "padding: 8px; text-align: center;"),
            tags$td("16", style = "padding: 8px; text-align: center;"),
            tags$td("3.3", style = "padding: 8px; text-align: center;"),
            tags$td("4.8", style = "padding: 8px; text-align: center;"),
            tags$td("B", style = "padding: 8px;")
          )
        )
      )
    ),
    
    tags$hr(style = "margin: 40px 0;"),
    
    # Dichotomous Outcomes
    h3(icon("toggle-on", style = "color: #667eea;"), " Dichotomous Outcomes", 
       style = "color: #1d3557; margin-top: 40px;"),
    p(strong("Use for:"), " Binary outcomes (event/no event, success/failure, etc.)"),
    p(strong("Required Columns:"), " ", 
      tags$code("study, event.e, n.e, event.c, n.c, group (optional)")),
    
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-sm table-bordered",
        style = "background: white; font-size: 13px;",
        tags$thead(
          style = "background: #667eea; color: white;",
          tags$tr(
            tags$th("COLUMN NAME", style = "padding: 10px; text-align: center;"),
            tags$th("DESCRIPTION", style = "padding: 10px;"),
            tags$th("VALID VALUES", style = "padding: 10px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$code("study"), style = "padding: 8px; text-align: center;"),
            tags$td("Study identifier", style = "padding: 8px;"),
            tags$td('Any text (e.g., "Smith 2020")', style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("event.e"), style = "padding: 8px; text-align: center;"),
            tags$td("Number of events — experimental group", style = "padding: 8px;"),
            tags$td("Non-negative integer", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("n.e"), style = "padding: 8px; text-align: center;"),
            tags$td("Total sample size — experimental group", style = "padding: 8px;"),
            tags$td("Positive integer", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("event.c"), style = "padding: 8px; text-align: center;"),
            tags$td("Number of events — control group", style = "padding: 8px;"),
            tags$td("Non-negative integer", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("n.c"), style = "padding: 8px; text-align: center;"),
            tags$td("Total sample size — control group", style = "padding: 8px;"),
            tags$td("Positive integer", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("group"), style = "padding: 8px; text-align: center;"),
            tags$td("Subgroup variable (OPTIONAL)", style = "padding: 8px;"),
            tags$td("Categorical text", style = "padding: 8px;")
          )
        )
      )
    ),
    
    p(strong("Example Dichotomous Data:")),
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-sm table-bordered",
        style = "background: white; font-size: 13px;",
        tags$thead(
          style = "background: #667eea; color: white;",
          tags$tr(
            tags$th("STUDY", style = "padding: 10px;"),
            tags$th("EVENT.E", style = "padding: 10px; text-align: center;"),
            tags$th("N.E", style = "padding: 10px; text-align: center;"),
            tags$th("EVENT.C", style = "padding: 10px; text-align: center;"),
            tags$th("N.C", style = "padding: 10px; text-align: center;"),
            tags$th("GROUP", style = "padding: 10px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("study 1", style = "padding: 8px;"),
            tags$td("16", style = "padding: 8px; text-align: center;"),
            tags$td("35", style = "padding: 8px; text-align: center;"),
            tags$td("5", style = "padding: 8px; text-align: center;"),
            tags$td("32", style = "padding: 8px; text-align: center;"),
            tags$td("B", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td("study 2", style = "padding: 8px;"),
            tags$td("5", style = "padding: 8px; text-align: center;"),
            tags$td("52", style = "padding: 8px; text-align: center;"),
            tags$td("1", style = "padding: 8px; text-align: center;"),
            tags$td("50", style = "padding: 8px; text-align: center;"),
            tags$td("B", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td("study 3", style = "padding: 8px;"),
            tags$td("5", style = "padding: 8px; text-align: center;"),
            tags$td("50", style = "padding: 8px; text-align: center;"),
            tags$td("17", style = "padding: 8px; text-align: center;"),
            tags$td("45", style = "padding: 8px; text-align: center;"),
            tags$td("A", style = "padding: 8px;")
          )
        )
      )
    ),
    
    tags$hr(style = "margin: 40px 0;"),
    
    # Correlation Data
    h3(icon("link", style = "color: #764ba2;"), " Correlation Data", 
       style = "color: #1d3557; margin-top: 40px;"),
    p(strong("Use for:"), " Correlation coefficients from observational studies"),
    p(strong("Required Columns:"), " ", tags$code("study, r, n, group (optional)")),
    
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-sm table-bordered",
        style = "background: white; font-size: 13px;",
        tags$thead(
          style = "background: #764ba2; color: white;",
          tags$tr(
            tags$th("COLUMN NAME", style = "padding: 10px; text-align: center;"),
            tags$th("DESCRIPTION", style = "padding: 10px;"),
            tags$th("VALID VALUES", style = "padding: 10px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$code("study"), style = "padding: 8px; text-align: center;"),
            tags$td("Study identifier", style = "padding: 8px;"),
            tags$td("Any text", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("r"), style = "padding: 8px; text-align: center;"),
            tags$td("Correlation coefficient", style = "padding: 8px;"),
            tags$td("Number between −1 and 1", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("n"), style = "padding: 8px; text-align: center;"),
            tags$td("Sample size", style = "padding: 8px;"),
            tags$td("Positive integer (≥ 4)", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("group"), style = "padding: 8px; text-align: center;"),
            tags$td("Subgroup variable (OPTIONAL)", style = "padding: 8px;"),
            tags$td("Categorical text", style = "padding: 8px;")
          )
        )
      )
    ),
    
    p(strong("Example Correlation Data:")),
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-sm table-bordered",
        style = "background: white; font-size: 13px;",
        tags$thead(
          style = "background: #764ba2; color: white;",
          tags$tr(
            tags$th("study", style = "padding: 10px;"),
            tags$th("r", style = "padding: 10px; text-align: center;"),
            tags$th("n", style = "padding: 10px; text-align: center;"),
            tags$th("group", style = "padding: 10px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("S1", style = "padding: 8px;"),
            tags$td("0.75", style = "padding: 8px; text-align: center;"),
            tags$td("60", style = "padding: 8px; text-align: center;"),
            tags$td("A", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td("S2", style = "padding: 8px;"),
            tags$td("0.65", style = "padding: 8px; text-align: center;"),
            tags$td("75", style = "padding: 8px; text-align: center;"),
            tags$td("A", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td("S3", style = "padding: 8px;"),
            tags$td("0.29", style = "padding: 8px; text-align: center;"),
            tags$td("68", style = "padding: 8px; text-align: center;"),
            tags$td("B", style = "padding: 8px;")
          )
        )
      )
    ),
    
    # ==========================================================================
    # EFFECT SIZE MEASURES - COMPREHENSIVE TABLE
    # ==========================================================================
    
    h2("Effect Size Measures", style = "color: #1d3557; margin-top: 50px; 
                                        border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    h3("Continuous Outcomes (6 Measures)", style = "color: #1d3557; margin-top: 30px;"),
    
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white; font-size: 13px;",
        tags$thead(
          style = "background: #20B2AA; color: white;",
          tags$tr(
            tags$th("Measure Name", style = "padding: 10px;"),
            tags$th("Code", style = "padding: 10px;"),
            tags$th("Description", style = "padding: 10px;"),
            tags$th("When to Use", style = "padding: 10px;"),
            tags$th("Interpretation", style = "padding: 10px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(strong("Standardized Mean Difference"), style = "padding: 8px;"),
            tags$td(tags$code("SMD"), style = "padding: 8px;"),
            tags$td("Standardizes mean difference using pooled SD; unitless; applies Hedges' g correction; 
                    allows synthesis across different scales", style = "padding: 8px;"),
            tags$td("Default for continuous outcomes; different scales/units; heterogeneous sample sizes", 
                    style = "padding: 8px;"),
            tags$td("Small = 0.2, Medium = 0.5, Large = 0.8; >0 favors treatment; <0 favors control", 
                    style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Standardized Mean Difference (Heteroscedastic)"), style = "padding: 8px;"),
            tags$td(tags$code("SMDH"), style = "padding: 8px;"),
            tags$td("Like SMD but allows unequal variances; uses separate SDs; robust to heteroscedasticity", 
                    style = "padding: 8px;"),
            tags$td("Variances differ across groups; treatment affects variability; SD ratio > 2:1", 
                    style = "padding: 8px;"),
            tags$td("Same benchmarks as SMD; unbiased when variances differ", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Standardized Mean Difference — Control SD"), style = "padding: 8px;"),
            tags$td(tags$code("SMD1"), style = "padding: 8px;"),
            tags$td("Uses only control-group SD for standardization; avoids treatment-induced SD bias", 
                    style = "padding: 8px;"),
            tags$td("Control SD most meaningful; treatment affects variability; only control SD reported", 
                    style = "padding: 8px;"),
            tags$td("Same benchmarks; asymmetric (depends on group labeling)", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Standardized Mean Difference — Control SD (Heteroscedastic)"), 
                    style = "padding: 8px;"),
            tags$td(tags$code("SMD1H"), style = "padding: 8px;"),
            tags$td("Combines SMD1 and SMDH; control SD reference with unequal variances", 
                    style = "padding: 8px;"),
            tags$td("Intervention affects mean and variance; maximum robustness needed", 
                    style = "padding: 8px;"),
            tags$td("Same benchmarks; most conservative standardized option", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Mean Difference"), style = "padding: 8px;"),
            tags$td(tags$code("MD"), style = "padding: 8px;"),
            tags$td("Raw mean difference in original units; no standardization", style = "padding: 8px;"),
            tags$td("Same scale/units across studies; clinical interpretability prioritized", 
                    style = "padding: 8px;"),
            tags$td("Directly interpretable (e.g., 5 mmHg reduction); cannot mix scales", 
                    style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Ratio of Means"), style = "padding: 8px;"),
            tags$td(tags$code("ROM"), style = "padding: 8px;"),
            tags$td("Log ratio of treatment/control means; multiplicative effect", style = "padding: 8px;"),
            tags$td("Positive ratio-scale outcomes; wide value ranges; multiplicative interpretation", 
                    style = "padding: 8px;"),
            tags$td("log(ROM)=0 → no effect; exp(ROM) gives ratio (e.g., 0.35 → 1.42)", 
                    style = "padding: 8px;")
          )
        )
      )
    ),
    
    h3("Dichotomous Outcomes (3 Measures)", style = "color: #1d3557; margin-top: 40px;"),
    
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white; font-size: 13px;",
        tags$thead(
          style = "background: #667eea; color: white;",
          tags$tr(
            tags$th("Measure Name", style = "padding: 10px;"),
            tags$th("Code", style = "padding: 10px;"),
            tags$th("Description", style = "padding: 10px;"),
            tags$th("When to Use", style = "padding: 10px;"),
            tags$th("Interpretation", style = "padding: 10px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(strong("Odds Ratio"), style = "padding: 8px;"),
            tags$td(tags$code("OR"), style = "padding: 8px;"),
            tags$td("Ratio of odds between groups; standard in case-control studies; 
                    0.5 continuity correction", style = "padding: 8px;"),
            tags$td("Case-control designs; rare events; logistic models", style = "padding: 8px;"),
            tags$td("OR=1 no effect; >1 higher odds; <1 lower odds; overestimates RR for common events", 
                    style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Risk Ratio (Relative Risk)"), style = "padding: 8px;"),
            tags$td(tags$code("RR"), style = "padding: 8px;"),
            tags$td("Ratio of event probabilities; intuitive for common events", style = "padding: 8px;"),
            tags$td("Cohort studies, RCTs; prospective designs; baseline risk important", 
                    style = "padding: 8px;"),
            tags$td("RR=1 neutral; <1 benefit; e.g., 0.6 → 40% risk reduction", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Risk Difference"), style = "padding: 8px;"),
            tags$td(tags$code("RD"), style = "padding: 8px;"),
            tags$td("Absolute difference in event rates; enables NNT calculation", style = "padding: 8px;"),
            tags$td("Clinical decision-making; public health; absolute effects needed", 
                    style = "padding: 8px;"),
            tags$td("RD=0 no effect; RD=-0.10 → 10% reduction; NNT=1/|RD|", style = "padding: 8px;")
          )
        )
      )
    ),
    
    h3("Correlation Outcomes (2 Measures)", style = "color: #1d3557; margin-top: 40px;"),
    
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white; font-size: 13px;",
        tags$thead(
          style = "background: #764ba2; color: white;",
          tags$tr(
            tags$th("Measure Name", style = "padding: 10px;"),
            tags$th("Code", style = "padding: 10px;"),
            tags$th("Description", style = "padding: 10px;"),
            tags$th("When to Use", style = "padding: 10px;"),
            tags$th("Interpretation", style = "padding: 10px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(strong("Fisher's z-transformed Correlation"), style = "padding: 8px;"),
            tags$td(tags$code("ZCOR"), style = "padding: 8px;"),
            tags$td("Variance-stabilized transformation of r; required for meta-analysis", 
                    style = "padding: 8px;"),
            tags$td("Synthesizing correlations; accurate weighting; preferred method", 
                    style = "padding: 8px;"),
            tags$td("z=0 no correlation; back-transform: r = tanh(z)", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Raw Correlation"), style = "padding: 8px;"),
            tags$td(tags$code("COR"), style = "padding: 8px;"),
            tags$td("Untransformed Pearson correlation; descriptive only", style = "padding: 8px;"),
            tags$td("Descriptive summaries; visualization; not for synthesis", style = "padding: 8px;"),
            tags$td("r=0 none; benchmarks: weak (0.1-0.3), moderate (0.3-0.5), strong (>0.5)", 
                    style = "padding: 8px;")
          )
        )
      )
    ),
    
    # ==========================================================================
    # ANALYSIS WORKFLOW
    # ==========================================================================
    
    h2("Analysis Workflow", style = "color: #1d3557; margin-top: 50px; 
                                     border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    p("MetaSuite follows a structured 5-step workflow for comprehensive meta-analysis:"),
    
    create_step_card(1, "Load Your Data",
                     "Click 'Select Dataset' for sample data OR 'Upload File' for Excel/CSV. 
                     Verify column headers match specifications exactly (case-sensitive)."),
    
    create_step_card(2, "Review Data Table",
                     "Navigate to Data Table tab. Enable 'Edit Data' mode for on-the-fly corrections. 
                     Export as CSV/Excel to preserve edits."),
    
    create_step_card(3, "Configure Model Settings",
                     "Select Dataset type → Effect Size Measure → Display Transformation → 
                     Model Method (REML/ML/DL/FE/HS) → Optional Subgroup → Confidence Level (default 95%)."),
    
    create_step_card(4, "Run Analysis",
                     "Click 'Run Analysis'. Effect sizes, heterogeneity stats, and publication bias tests 
                     computed automatically. Auto-redirect to Results tab."),
    
    create_step_card(5, "Explore Results & Visualizations",
                     "Review Results tab (pooled effect, I²/τ²/Q, bias tests), Effect Plots tab 
                     (forest/funnel/sunset), and Diagnostics tab (Cook's/residuals/leave-one-out)."),
    
    div(
      style = "background: #FFF3CD; padding: 20px; border-radius: 8px; margin: 20px 0; 
               border-left: 4px solid #F39C12;",
      p(icon("info-circle", style = "color: #F39C12;"), 
        strong(" Note:"), 
        " Publication bias tests are ", strong("not available"), " for subgroup analysis. 
        Leave-one-out plot is also ", strong("not available"), " for subgroup analysis.",
        style = "margin: 0; color: #856404;")
    ),
    
    # ==========================================================================
    # VISUALIZATION FEATURES - CONCISE TABLE
    # ==========================================================================
    
    h2("Visualization Features", style = "color: #1d3557; margin-top: 50px; 
                                          border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    h3("Effect Size & Bias Assessment Plots", style = "color: #1d3557; margin-top: 30px;"),
    
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white;",
        tags$thead(
          style = "background: #1d3557; color: white;",
          tags$tr(
            tags$th("Plot", style = "padding: 12px;"),
            tags$th("Purpose", style = "padding: 12px;"),
            tags$th("Shows", style = "padding: 12px;"),
            tags$th("How to Read", style = "padding: 12px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(strong("Forest Plot"), style = "padding: 10px;"),
            tags$td("Summarizes study effects and pooled result", style = "padding: 10px;"),
            tags$td("Study effect sizes with 95% CIs, pooled estimate, heterogeneity (I², τ², Q)", 
                    style = "padding: 10px;"),
            tags$td("CI not crossing null = significant study; diamond crossing null = non-significant pooled effect; 
                    I² > 75% = high heterogeneity", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Funnel Plot"), style = "padding: 10px;"),
            tags$td("Detects publication bias", style = "padding: 10px;"),
            tags$td("Effect size vs. standard error, 95% funnel, Egger's line, trim-and-fill", 
                    style = "padding: 10px;"),
            tags$td("Symmetry = low bias; asymmetry or empty corners = possible missing studies", 
                    style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Sunset Plot"), style = "padding: 10px;"),
            tags$td("Assesses statistical power", style = "padding: 10px;"),
            tags$td("Power contours (50%, 80%, 95%), pooled effect line", style = "padding: 10px;"),
            tags$td("Upper zones = well powered; lower zones = underpowered studies", style = "padding: 10px;")
          )
        )
      )
    ),
    
    h3("Influence & Sensitivity Diagnostics", style = "color: #1d3557; margin-top: 40px;"),
    
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white;",
        tags$thead(
          style = "background: #1d3557; color: white;",
          tags$tr(
            tags$th("Plot", style = "padding: 12px;"),
            tags$th("Purpose", style = "padding: 12px;"),
            tags$th("Shows", style = "padding: 12px;"),
            tags$th("How to Read", style = "padding: 12px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(strong("Cook's Distance"), style = "padding: 10px;"),
            tags$td("Identifies influential studies", style = "padding: 10px;"),
            tags$td("Influence values with robust threshold", style = "padding: 10px;"),
            tags$td("Points above threshold = studies strongly affecting pooled result", 
                    style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Studentized Residuals"), style = "padding: 10px;"),
            tags$td("Detects outliers", style = "padding: 10px;"),
            tags$td("Standardized residuals with Bonferroni limits", style = "padding: 10px;"),
            tags$td("Values beyond limits = outlier studies", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Leave-One-Out"), style = "padding: 10px;"),
            tags$td("Tests result robustness", style = "padding: 10px;"),
            tags$td("Pooled effect after removing each study", style = "padding: 10px;"),
            tags$td("Large shifts or null crossing = study-driven results", style = "padding: 10px;")
          )
        )
      )
    ),
    
    div(
      style = "background: #FFF3CD; padding: 15px; border-radius: 8px; margin: 20px 0; 
               border-left: 4px solid #F39C12;",
      p(icon("info-circle", style = "color: #F39C12;"), 
        strong(" Note:"), 
        " Currently Leave-One-Out plot is ", strong("not available"), " for subgroup analysis.",
        style = "margin: 0; color: #856404;")
    ),
    
    # ==========================================================================
    # STATISTICAL TESTS - CONCISE TABLE
    # ==========================================================================
    
    h2("Statistical Tests & Measures", style = "color: #1d3557; margin-top: 50px; 
                                                 border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    h3("Heterogeneity Statistics", style = "color: #1d3557; margin-top: 30px;"),
    
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white;",
        tags$thead(
          style = "background: #20B2AA; color: white;",
          tags$tr(
            tags$th("Statistic", style = "padding: 12px;"),
            tags$th("What It Measures", style = "padding: 12px;"),
            tags$th("How to Interpret", style = "padding: 12px;"),
            tags$th("Guidance", style = "padding: 12px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(strong("I²"), style = "padding: 10px;"),
            tags$td("% of variability due to between-study heterogeneity", style = "padding: 10px;"),
            tags$td("0-40% low · 30-60% moderate · 50-90% substantial · 75-100% considerable", 
                    style = "padding: 10px;"),
            tags$td("<50% fixed-effects · 50-75% random-effects · >75% investigate heterogeneity", 
                    style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("τ²"), style = "padding: 10px;"),
            tags$td("Between-study variance (random-effects)", style = "padding: 10px;"),
            tags$td("0 = none · larger values = more heterogeneity", style = "padding: 10px;"),
            tags$td("Use to quantify absolute heterogeneity (variance scale)", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Q (Cochran's Q)"), style = "padding: 10px;"),
            tags$td("Tests if heterogeneity > 0", style = "padding: 10px;"),
            tags$td("p < 0.05 = significant heterogeneity", style = "padding: 10px;"),
            tags$td("Low power with k < 10; interpret with I² and τ²", style = "padding: 10px;")
          )
        )
      )
    ),
    
    div(
      style = "background: #E8F4F8; padding: 20px; border-radius: 8px; margin: 20px 0;",
      p(strong("Note:"), " Always interpret ", strong("I², τ², and Q together"), 
        ", and treat ", strong("bias tests as supportive evidence"), ", not definitive proof.",
        style = "margin: 0; color: #1d3557;")
    ),
    
    # ==========================================================================
    # QUICK REFERENCE - INTERPRETATION TABLE
    # ==========================================================================
    
    h2("Quick Reference: Effect Size Interpretation", style = "color: #1d3557; margin-top: 50px; 
                                                                border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white; font-size: 13px;",
        tags$thead(
          style = "background: #1d3557; color: white;",
          tags$tr(
            tags$th("Measure", style = "padding: 10px;"),
            tags$th("Scale", style = "padding: 10px;"),
            tags$th("Neutral Value", style = "padding: 10px;"),
            tags$th("Direction", style = "padding: 10px;"),
            tags$th("Interpretation Benchmarks", style = "padding: 10px;"),
            tags$th("Example", style = "padding: 10px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(strong("SMD / SMDH / SMD1 / SMD1H"), style = "padding: 8px;"),
            tags$td("Standardized", style = "padding: 8px;"),
            tags$td("0", style = "padding: 8px; text-align: center;"),
            tags$td(">0 higher · <0 lower", style = "padding: 8px;"),
            tags$td("|d| <0.2 negligible · 0.2-0.5 small · 0.5-0.8 medium · >0.8 large", 
                    style = "padding: 8px;"),
            tags$td("d = 0.6 → medium", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Mean Difference (MD)"), style = "padding: 8px;"),
            tags$td("Original units", style = "padding: 8px;"),
            tags$td("0", style = "padding: 8px; text-align: center;"),
            tags$td(">0 higher · <0 lower", style = "padding: 8px;"),
            tags$td("Clinical relevance depends on units", style = "padding: 8px;"),
            tags$td("MD = −5 mmHg", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Ratio of Means (ROM)"), style = "padding: 8px;"),
            tags$td("Log ratio", style = "padding: 8px;"),
            tags$td("0 (ratio=1)", style = "padding: 8px; text-align: center;"),
            tags$td(">0 higher · <0 lower", style = "padding: 8px;"),
            tags$td("exp(ROM) = mean ratio", style = "padding: 8px;"),
            tags$td("ROM = 0.35 → 1.42×", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Odds Ratio (OR)"), style = "padding: 8px;"),
            tags$td("Ratio", style = "padding: 8px;"),
            tags$td("1", style = "padding: 8px; text-align: center;"),
            tags$td(">1 higher odds · <1 lower odds", style = "padding: 8px;"),
            tags$td("Larger distance from 1 = stronger effect", style = "padding: 8px;"),
            tags$td("OR = 2.0", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Risk Ratio (RR)"), style = "padding: 8px;"),
            tags$td("Ratio", style = "padding: 8px;"),
            tags$td("1", style = "padding: 8px; text-align: center;"),
            tags$td(">1 higher risk · <1 lower risk", style = "padding: 8px;"),
            tags$td("RR <1 indicates risk reduction", style = "padding: 8px;"),
            tags$td("RR = 0.75 (25% RRR)", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Risk Difference (RD)"), style = "padding: 8px;"),
            tags$td("Absolute", style = "padding: 8px;"),
            tags$td("0", style = "padding: 8px; text-align: center;"),
            tags$td(">0 higher · <0 lower", style = "padding: 8px;"),
            tags$td("Absolute effect; NNT = 1/|RD|", style = "padding: 8px;"),
            tags$td("RD = -0.08 → NNT ≈ 13", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("ZCOR"), style = "padding: 8px;"),
            tags$td("Fisher's z", style = "padding: 8px;"),
            tags$td("0", style = "padding: 8px; text-align: center;"),
            tags$td(">0 positive · <0 negative", style = "padding: 8px;"),
            tags$td("Back-transform: r = tanh(z)", style = "padding: 8px;"),
            tags$td("z = 0.4", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("COR"), style = "padding: 8px;"),
            tags$td("Pearson r", style = "padding: 8px;"),
            tags$td("0", style = "padding: 8px; text-align: center;"),
            tags$td(">0 positive · <0 negative", style = "padding: 8px;"),
            tags$td("0.1-0.3 weak · 0.3-0.5 moderate · >0.5 strong", style = "padding: 8px;"),
            tags$td("r = 0.45", style = "padding: 8px;")
          )
        )
      )
    ),
    
    # ==========================================================================
    # CUSTOMIZATION OPTIONS - TABLE
    # ==========================================================================
    
    h2("Customization Options", style = "color: #1d3557; margin-top: 50px; 
                                         border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white; font-size: 13px;",
        tags$thead(
          style = "background: #667eea; color: white;",
          tags$tr(
            tags$th("Category", style = "padding: 10px;"),
            tags$th("Applies To", style = "padding: 10px;"),
            tags$th("Customizable Elements", style = "padding: 10px;"),
            tags$th("Notes / Best Practice", style = "padding: 10px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(strong("Color Customization"), style = "padding: 8px;"),
            tags$td("Forest Plot", style = "padding: 8px;"),
            tags$td("Study points, CI lines, pooled diamond", style = "padding: 8px;"),
            tags$td("Use high contrast for pooled estimate", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Color Customization"), style = "padding: 8px;"),
            tags$td("Funnel Plot", style = "padding: 8px;"),
            tags$td("Study points (by subgroup), contour shading, regression & funnel lines", 
                    style = "padding: 8px;"),
            tags$td("Supports up to 4 subgroup colors", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Color Customization"), style = "padding: 8px;"),
            tags$td("Sunset Plot", style = "padding: 8px;"),
            tags$td("Study points, power gradient, power contours", style = "padding: 8px;"),
            tags$td("Gradient reflects study power", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Color Customization"), style = "padding: 8px;"),
            tags$td("Influence Plots", style = "padding: 8px;"),
            tags$td("Point color (above/below threshold), threshold line, background", 
                    style = "padding: 8px;"),
            tags$td("Keep background neutral for clarity", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Typography"), style = "padding: 8px;"),
            tags$td("All plots", style = "padding: 8px;"),
            tags$td("Axis labels, legends, titles, annotations", style = "padding: 8px;"),
            tags$td("≥10-12 pt for readability", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Font Size Guidance"), style = "padding: 8px;"),
            tags$td("All plots", style = "padding: 8px;"),
            tags$td("Scale text up for presentations; down for publications", style = "padding: 8px;"),
            tags$td("Adjust for viewing distance", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Plot Titles"), style = "padding: 8px;"),
            tags$td("All plots", style = "padding: 8px;"),
            tags$td("Fully editable per plot type", style = "padding: 8px;"),
            tags$td("Include outcome + effect measure (e.g., SMD, OR)", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Export Format"), style = "padding: 8px;"),
            tags$td("All plots", style = "padding: 8px;"),
            tags$td("PNG (transparent), JPG (white background)", style = "padding: 8px;"),
            tags$td("PNG preferred for publications", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Export Quality"), style = "padding: 8px;"),
            tags$td("All plots", style = "padding: 8px;"),
            tags$td("300 DPI resolution", style = "padding: 8px;"),
            tags$td("Publication-ready", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Export Dimensions"), style = "padding: 8px;"),
            tags$td("All plots", style = "padding: 8px;"),
            tags$td("Auto-optimized (≈6×4 in default)", style = "padding: 8px;"),
            tags$td("Fits standard journals", style = "padding: 8px;")
          )
        )
      )
    ),
    
    # ==========================================================================
    # SUBGROUP ANALYSIS
    # ==========================================================================
    
    h2("Subgroup Analysis Guide", style = "color: #1d3557; margin-top: 50px; 
                                           border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    tags$ol(
      style = "line-height: 2;",
      tags$li(strong("Load Data:"), " Upload file with ", tags$code("group"), " column; verify column appears in data table"),
      tags$li(strong("Configure Analysis:"), " In Model Settings, select subgroup column from dropdown; all other settings remain same"),
      tags$li(strong("Run Analysis:"), " Click 'Run Analysis'; results automatically stratified")
    ),
    
    h3("Features", style = "color: #1d3557; margin-top: 30px;"),
    
    tags$ul(
      tags$li(strong("Forest Plot Stratification:"), " Separate effect estimates for each subgroup; 
              individual study grouping by subgroup color; subgroup-specific pooled estimates; 
              residual heterogeneity within groups"),
      tags$li(strong("Funnel Plot Display:"), " Color-coded points by subgroup (up to 4 groups); 
              legend identifying subgroup assignments; separate funnel regions for each group"),
      tags$li(strong("Statistical Testing:"), " QM test for between-group differences; 
              I² calculation for overall and within-group heterogeneity; 
              group-level effect estimates and confidence intervals")
    ),
    
    h3("Interpretation Guidelines", style = "color: #1d3557; margin-top: 30px;"),
    
    tags$ul(
      tags$li(strong("Between-Group Differences:"), " QM p < 0.05: Subgroups differ significantly; 
              QM p ≥ 0.05: No evidence of group differences"),
      tags$li(strong("Heterogeneity Assessment:"), " Check I² within each subgroup (should be lower than overall I² if moderator identified); 
              If residual I² > 75%, additional heterogeneity sources remain"),
      tags$li(strong("Effect Direction:"), " Examine confidence intervals: overlapping CIs suggest no meaningful difference; 
              Non-overlapping CIs provide visual evidence of differences; 
              Magnitude of difference depends on context (clinical vs statistical significance)")
    ),
    
    # ==========================================================================
    # TROUBLESHOOTING
    # ==========================================================================
    
    h2("Troubleshooting Guide", style = "color: #1d3557; margin-top: 50px; 
                                         border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    h3("Common Issues & Solutions", style = "color: #1d3557; margin-top: 30px;"),
    
    create_troubleshooting_item(
      icon_name = "triangle-exclamation",
      icon_color = "#dc3545",
      title = 'Error: "Invalid data format"',
      causes = c("Column names don't match specifications exactly", 
                 "Data type mismatch (text in numeric column, vice versa)", 
                 "Extra spaces or special characters in headers"),
      solutions = c("Check column names are exactly: study, n.e, mean.e, sd.e, n.c, mean.c, sd.c (or appropriate set)",
                    "Names are case-sensitive",
                    "Remove leading/trailing spaces from headers",
                    "Verify numeric columns contain only numbers (no text)",
                    "Download sample dataset and compare formatting")
    ),
    
    create_troubleshooting_item(
      icon_name = "ban",
      icon_color = "#F39C12",
      title = 'Error: "Cannot compute publication bias tests"',
      causes = c("Insufficient number of studies (requires k ≥ 10 for adequate power)", 
                 "Perfect separation in data (no variation in effect sizes)"),
      solutions = c("Ensure k ≥ 10 studies for statistical power",
                    "Review effect sizes for unusual patterns",
                    "Check data entry for errors",
                    "With small k (3-9), report visual funnel plot inspection instead")
    ),
    
    create_troubleshooting_item(
      icon_name = "chart-line",
      icon_color = "#F39C12",
      title = 'Warning: "Heterogeneity is high (I² > 75%)"',
      causes = c("Substantial differences between studies", 
                 "Poor measurement standardization",
                 "Missing important moderators",
                 "Inclusion of diverse populations"),
      solutions = c("Examine heterogeneity sources: design, population, intervention, outcome measurement",
                    "Perform subgroup analysis on potential moderators",
                    "Consider meta-regression (future feature) to explore heterogeneity sources",
                    "Document heterogeneity and discuss implications",
                    "Consider sensitivity analyses (e.g., leave-one-out) to identify extreme studies")
    ),
    
    create_troubleshooting_item(
      icon_name = "circle-info",
      icon_color = "#0d6efd",
      title = "Tip: Zero Events",
      causes = c("Studies with zero events in one or both groups problematic for binary outcome measures"),
      solutions = c("Zero events in one group only: 0.5 continuity correction automatically applied",
                    "Zero events in both groups: Study automatically excluded from analysis",
                    "Document exclusions in methods section")
    ),
    
    create_troubleshooting_item(
      icon_name = "database",
      icon_color = "#6c757d",
      title = "Tip: Missing Data",
      causes = c("Studies with missing values (NA) in required columns"),
      solutions = c("Complete-case analysis: Studies with any NA excluded",
                    "Imputation: Not currently supported",
                    "Best practice: Exclude studies with missing effect size data",
                    "Document number of excluded studies and reasons")
    ),
    
    # ==========================================================================
    # PRO TIPS
    # ==========================================================================
    
    # ==========================================================================
    # PRO TIPS & BEST PRACTICES (WITH YELLOW BACKGROUND LIKE ROB MODULE)
    # ==========================================================================
    h2("Analysis Workflow", style = "color: #1d3557; margin-top: 50px;
                                     border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    p("MetaSuite follows a structured 5-step workflow for comprehensive meta-analysis:"),
    
    create_step_card(1, "Load Your Data",
                     "Click 'Select Dataset' for sample data OR 'Upload File' for Excel/CSV.
                     Verify column headers match specifications exactly (case-sensitive)."),
    
    create_step_card(2, "Review Data Table",
                     "Navigate to Data Table tab. Enable 'Edit Data' mode for on-the-fly corrections.
                     Export as CSV/Excel to preserve edits."),
    
    create_step_card(3, "Configure Model Settings",
                     "Select Dataset type → Effect Size Measure → Display Transformation → 
                     Model Method (REML/ML/DL/FE/HS) → Optional Subgroup → Confidence Level (default 95%)."),
    
    create_step_card(4, "Run Analysis",
                     "Click 'Run Analysis'. Effect sizes, heterogeneity stats, and publication bias tests computed automatically.
                     Auto-redirect to Results tab."),
    
    create_step_card(5, "Explore Results & Visualizations",
                     "Review Results tab (pooled effect, I²/τ², bias tests), 
                     Effect Plots tab (forest/funnel/sunset), and 
                     Diagnostics tab (Cook's/residuals/leave-one-out)."),
    
    # PRO TIPS WITH YELLOW BACKGROUND CARD
    div(
      style = "background: #FFF3CD; padding: 25px; border-radius: 8px; margin-top: 40px;
              border-left: 4px solid #F39C12;",
      h4(icon("lightbulb"), " Pro Tips",
         style = "color: #856404; margin-top: 0;"),
      tags$ul(
        style = "color: #856404; margin: 0; line-height: 1.8;",
        tags$li("Use sample datasets to explore features before uploading your data"),
        tags$li("Always start with random-effects model unless I² <25% and studies are nearly identical"),
        tags$li("For continuous outcomes, prefer SMD over MD unless all studies use identical scale"),
        tags$li("Download plots at 300 DPI for publication-ready quality (PNG with transparency recommended)"),
        tags$li("Enable edit mode to make quick corrections without re-uploading entire dataset"),
        tags$li("Report both effect estimates and heterogeneity statistics (I², τ², Q) in your publications")
      )
    ),
    
    
    h2("Additional Resources", style = "color: #1d3557; margin-top: 50px;
                                       border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    h3("Key References", style = "color: #1d3557; margin-top: 30px;"),
    tags$ol(
      style = "line-height: 2;",
      tags$li(
        "Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein, H. R. (2009). ",
        tags$em("Introduction to meta-analysis"), ". Wiley. ",
        tags$a("https://doi.org/10.1002/9780470743386",
               href = "https://doi.org/10.1002/9780470743386",
               target = "_blank")
      ),
      tags$li(
        "Higgins, J. P. T., & Green, S. (Eds.). (2011). ",
        tags$em("Cochrane handbook for systematic reviews of interventions"), 
        " (Version 5.1.0). The Cochrane Collaboration. ",
        tags$a("https://handbook-5-1.cochrane.org/",
               href = "https://handbook-5-1.cochrane.org/",
               target = "_blank")
      ),
      tags$li(
        "Viechtbauer, W. (2010). Conducting meta-analyses in R with the ",
        tags$em("metafor"), " package. ",
        tags$em("Journal of Statistical Software"), ", 36(3), 1-48. ",
        tags$a("https://doi.org/10.18637/jss.v036.i03",
               href = "https://doi.org/10.18637/jss.v036.i03",
               target = "_blank")
      ),
      tags$li(
        "Egger, M., Davey Smith, G., Schneider, M., & Minder, C. (1997). 
        Bias in meta-analysis detected by a simple, graphical test. ",
        tags$em("BMJ"), ", 315(7109), 629-634. ",
        tags$a("https://doi.org/10.1136/bmj.315.7109.629",
               href = "https://doi.org/10.1136/bmj.315.7109.629",
               target = "_blank")
      ),
      tags$li(
        "Begg, C. B., & Mazumdar, M. (1994). 
        Operating characteristics of a rank correlation test for publication bias. ",
        tags$em("Biometrics"), ", 50(4), 1088-1101. ",
        tags$a("https://doi.org/10.2307/2533446",
               href = "https://doi.org/10.2307/2533446",
               target = "_blank")
      ),
      tags$li(
        "Harrer, M., Cuijpers, P., Furukawa, T. A., & Ebert, D. D. (2021). ",
        tags$em("Doing meta-analysis with R: A hands-on guide"), 
        " (1st ed.). Chapman and Hall/CRC. ",
        tags$a("https://doi.org/10.1201/9781003107347",
               href = "https://doi.org/10.1201/9781003107347",
               target = "_blank")
      ),
      tags$li(
        "DerSimonian, R., & Laird, N. (1986). 
        Meta-analysis in clinical trials. ",
        tags$em("Controlled Clinical Trials"), ", 7(3), 177-188. ",
        tags$a("https://doi.org/10.1016/0197-2456(86)90046-2",
               href = "https://doi.org/10.1016/0197-2456(86)90046-2",
               target = "_blank")
      ),
      tags$li(
        "Higgins, J. P. T., Thompson, S. G., Deeks, J. J., & Altman, D. G. (2003). 
        Measuring inconsistency in meta-analyses. ",
        tags$em("BMJ"), ", 327(7414), 557-560. ",
        tags$a("https://doi.org/10.1136/bmj.327.7414.557",
               href = "https://doi.org/10.1136/bmj.327.7414.557",
               target = "_blank")
      ),
      tags$li(
        "Thompson, S. G., & Sharp, S. J. (1999). 
        Explaining heterogeneity in meta-analysis: A comparison of methods. ",
        tags$em("Statistics in Medicine"), ", 18(20), 2693-2708. ",
        tags$a("https://doi.org/10.1002/(SICI)1097-0258(19991030)18:20<2693::AID-SIM235>3.0.CO;2-V",
               href = "https://doi.org/10.1002/(SICI)1097-0258(19991030)18:20<2693::AID-SIM235>3.0.CO;2-V",
               target = "_blank")
      ),
      tags$li(
        "Schwarzer, G., Carpenter, J. R., & Rücker, G. (2015). ",
        tags$em("Meta-analysis with R"), ". Springer. ",
        tags$a("https://doi.org/10.1007/978-3-319-21416-0",
               href = "https://doi.org/10.1007/978-3-319-21416-0",
               target = "_blank")
      ),
      tags$li(
        "11.	Kossmeier, M., Tran, U., & Voracek, M. (2020). ",
        tags$em("metaviz: Forest plots, funnel plots, and visual funnel plot inference for meta-analysis "), "(R package version 0.3.1)",
        tags$a("https://github.com/Mkossmeier/metaviz",
               href = "https://github.com/Mkossmeier/metaviz",
               target = "_blank")
      )
    ),
    
    # GETTING HELP SECTION (SAME STYLE AS NMA MODULE)
    h3("Getting Help", style = "color: #1d3557; margin-top: 30px;"),
    div(
      style = "background: white; padding: 25px; border-radius: 8px; margin: 20px 0;",
      tags$ul(
        style = "line-height: 2; margin: 0;",
        tags$li(icon("database"), " ", strong("Review Sample Datasets:"), 
                " Available in the Data Loader section. Demonstrates proper formatting for each data type."),
        tags$li(icon("book"), " ", strong("Data Format Guide:"), 
                " Click 'Data Format Guide' button in Meta-Analysis module. 
                Shows detailed column specifications and examples."),
        tags$li(icon("circle-question"), " ", strong("Consult Documentation:"), 
                " Complete documentation available in Help tab. FAQs address common questions."),
        tags$li(icon("link"), " ", strong("Check References:"), 
                " See Key References section above. Borenstein et al. (2009) and 
                Higgins & Green (2011) recommended for comprehensive meta-analysis guidance.")
      )
    )
  )
}

# =============================================================================
# Helper Functions (keep same as before)
# =============================================================================

create_troubleshooting_item <- function(icon_name, icon_color, title, causes, solutions) {
  div(
    style = "background: white; padding: 25px; border-radius: 8px; margin-bottom: 20px; 
             box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
    h4(
      icon(icon_name, style = sprintf("color: %s;", icon_color)), 
      " ", 
      title,
      style = "color: #1d3557; margin-top: 0;"
    ),
    
    div(
      style = "margin: 15px 0;",
      h5("Causes:", style = "color: #1d3557; margin-bottom: 8px; font-size: 15px;"),
      tags$ul(
        style = "color: #5A6169; margin: 0;",
        lapply(causes, function(x) tags$li(x))
      )
    ),
    
    div(
      style = "margin: 15px 0 0 0;",
      h5("Solutions:", style = "color: #1d3557; margin-bottom: 8px; font-size: 15px;"),
      tags$ul(
        style = "color: #5A6169; margin: 0;",
        lapply(solutions, function(x) tags$li(x))
      )
    )
  )
}



# =============================================================================
# Helper Functions for Meta-Analysis Documentation
# =============================================================================

# Create effect measure card
create_effect_measure_card <- function(title, code, color, description, when_to_use, interpretation) {
  div(
    style = sprintf("background: white; padding: 20px; border-radius: 8px; margin: 15px 0; 
                     border-left: 4px solid %s; box-shadow: 0 2px 8px rgba(0,0,0,0.05);", color),
    h4(
      tags$span(code, 
                style = sprintf("background: %s; color: white; padding: 4px 10px; border-radius: 4px; 
                                 font-size: 13px; margin-right: 10px;", color)),
      title,
      style = "color: #1d3557; margin-top: 0; margin-bottom: 15px;"
    ),
    p(description, style = "margin-bottom: 12px;"),
    p(strong("When to Use: "), when_to_use, style = "margin-bottom: 12px; color: #5A6169;"),
    p(strong("Interpretation: "), interpretation, style = "margin: 0; color: #5A6169;")
  )
}

# Create troubleshooting item
create_troubleshooting_item <- function(icon_name, icon_color, title, causes, solutions) {
  div(
    style = "background: white; padding: 25px; border-radius: 8px; margin-bottom: 20px; 
             box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
    h4(
      icon(icon_name, style = sprintf("color: %s;", icon_color)), 
      " ", 
      title,
      style = "color: #1d3557; margin-top: 0;"
    ),
    
    div(
      style = "margin: 15px 0;",
      h5("Causes:", style = "color: #1d3557; margin-bottom: 8px; font-size: 15px;"),
      tags$ul(
        style = "color: #5A6169; margin: 0;",
        lapply(causes, function(x) tags$li(x))
      )
    ),
    
    div(
      style = "margin: 15px 0 0 0;",
      h5("Solutions:", style = "color: #1d3557; margin-bottom: 8px; font-size: 15px;"),
      tags$ul(
        style = "color: #5A6169; margin: 0;",
        lapply(solutions, function(x) tags$li(x))
      )
    )
  )
}




nma_content <- function() {
  tagList(
    # Header
    div(
      style = "background: linear-gradient(135deg, #BCA951 0%, #D79730 100%);
              padding: 40px; border-radius: 12px; color: white; margin-bottom: 40px;",
      h1(icon("diagram-project"), " Network Meta-Analysis",
         style = "margin: 0 0 15px 0; font-weight: 700;"),
      p("Complete guide to Network Meta-Analysis in MetaSuite",
        style = "font-size: 18px; margin: 0; opacity: 0.95;")
    ),
    
    # Overview Section
    h2("Overview", style = "color: #1d3557; margin-top: 30px;"),
    div(
      style = "background: #E8F4F8; padding: 25px; border-radius: 8px; margin: 20px 0;",
      p("The Network Meta-Analysis module in MetaSuite extends pairwise meta-analysis to enable 
        simultaneous comparison of multiple treatments by combining direct and indirect evidence 
        within a connected evidence network.")
    ),
    
    # Key Capabilities
    h3("Key Capabilities", style = "color: #1d3557; margin-top: 30px;"),
    tags$ul(
      style = "line-height: 2;",
      tags$li(strong("Multiple Datasets:"), " Continuous and dichotomous outcomes"),
      tags$li(strong("Diverse Effect Measures:"), " 5 effect size measures across data types"),
      tags$li(strong("Multiple Treatments:"), " Simultaneous comparison of ≥ 3 interventions"),
      tags$li(strong("5 different Models:"), " REML, ML, DL, HS, PM"),
      tags$li(strong("Heterogeneity Assessment:"), " I², τ², Q-statistic, and visual diagnostics"),
      tags$li(strong("Direct & Indirect Evidence:"), " Combines head-to-head and inferred comparisons"),
      tags$li(strong("Network Diagnostics:"), " Consistency, evidence flow, design-based tests and league tables"),
      tags$li(strong("Treatment Ranking:"), " SUCRA, rank probabilities, rank heat plots"),
      tags$li(strong("Publication-Quality Visualizations:"), " 300 DPI exports in PNG and JPG formats")
    ),
    
    # ==========================================================================
    # SUPPORTED DATA TYPES
    # ==========================================================================
    h2("Supported Data Types", style = "color: #1d3557; margin-top: 50px;
                                        border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    p("MetaSuite supports two main data types for network meta-analysis. Each requires specific columns 
      and supports different effect measures."),
    
    # Continuous Outcomes
    h3(icon("chart-line", style = "color: #667eea;"), " Continuous Outcomes", 
       style = "color: #1d3557; margin-top: 30px;"),
    p(strong("Use for:"), " Continuous measurements (e.g., blood pressure, symptom scores, biomarker levels)"),
    p(strong("Required Columns:"), " ", tags$code("study"), ", ",
      tags$code("treatment"), ", ", tags$code("mean"), ", ",
      tags$code("sd"), ", ", tags$code("n"), ", ",
      tags$code("treatment_class"), " (optional)"),
    
    div(
      style = "overflow-x: auto; margin: 25px 0;",
      tags$table(
        class = "table table-sm table-bordered",
        style = "background: white; font-size: 13px;",
        tags$thead(
          style = "background: #667eea; color: white;",
          tags$tr(
            tags$th("COLUMN NAME", style = "padding: 10px; text-align: center;"),
            tags$th("DESCRIPTION", style = "padding: 10px;"),
            tags$th("VALID VALUES", style = "padding: 10px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$code("study"), style = "padding: 8px; text-align: center;"),
            tags$td("Study identifier", style = "padding: 8px;"),
            tags$td("Any text (e.g., \"Smith 2020\")", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("treatment"), style = "padding: 8px; text-align: center;"),
            tags$td("Treatment/intervention name", style = "padding: 8px;"),
            tags$td("Text (e.g., \"Drug A\", \"Placebo\")", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("mean"), style = "padding: 8px; text-align: center;"),
            tags$td("Mean outcome value", style = "padding: 8px;"),
            tags$td("Any number", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("sd"), style = "padding: 8px; text-align: center;"),
            tags$td("Standard deviation", style = "padding: 8px;"),
            tags$td("Positive number", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("n"), style = "padding: 8px; text-align: center;"),
            tags$td("Sample size for this arm", style = "padding: 8px;"),
            tags$td("Positive integer", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("treatment_class"), style = "padding: 8px; text-align: center;"),
            tags$td("Treatment class (OPTIONAL)", style = "padding: 8px;"),
            tags$td("Text (e.g., \"Placebo\", \"Active\")", style = "padding: 8px;")
          )
        )
      )
    ),
    
    # Example Continuous Data
    p(strong("Example Continuous Data")),
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-sm table-bordered",
        style = "background: white; font-size: 13px;",
        tags$thead(
          style = "background: #667eea; color: white;",
          tags$tr(
            tags$th("study", style = "padding: 10px;"),
            tags$th("treatment", style = "padding: 10px;"),
            tags$th("mean", style = "padding: 10px; text-align: center;"),
            tags$th("sd", style = "padding: 10px; text-align: center;"),
            tags$th("n", style = "padding: 10px; text-align: center;"),
            tags$th("treatment_class", style = "padding: 10px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Study1", style = "padding: 8px;"),
            tags$td("Placebo", style = "padding: 8px;"),
            tags$td("9.43", style = "padding: 8px; text-align: center;"),
            tags$td("6.42", style = "padding: 8px; text-align: center;"),
            tags$td("69", style = "padding: 8px; text-align: center;"),
            tags$td("Placebo", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td("Study3", style = "padding: 8px;"),
            tags$td("Placebo", style = "padding: 8px;"),
            tags$td("16.93", style = "padding: 8px; text-align: center;"),
            tags$td("3.3", style = "padding: 8px; text-align: center;"),
            tags$td("110", style = "padding: 8px; text-align: center;"),
            tags$td("Placebo", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td("Study3", style = "padding: 8px;"),
            tags$td("Drug A", style = "padding: 8px;"),
            tags$td("14.89", style = "padding: 8px; text-align: center;"),
            tags$td("3.45", style = "padding: 8px; text-align: center;"),
            tags$td("80", style = "padding: 8px; text-align: center;"),
            tags$td("Active", style = "padding: 8px;")
          )
        )
      )
    ),
    
    tags$hr(style = "margin: 40px 0;"),
    
    # Dichotomous Outcomes
    h3(icon("toggle-on", style = "color: #667eea;"), " Dichotomous Outcomes", 
       style = "color: #1d3557; margin-top: 40px;"),
    p(strong("Use for:"), " Binary outcomes (event/no event, success/failure, etc.)"),
    p(strong("Required Columns:"), " ", tags$code("study"), ", ",
      tags$code("treatment"), ", ", tags$code("events"), ", ",
      tags$code("n"), ", ", tags$code("treatment_class"), " (optional)"),
    
    div(
      style = "overflow-x: auto; margin: 25px 0;",
      tags$table(
        class = "table table-sm table-bordered",
        style = "background: white; font-size: 13px;",
        tags$thead(
          style = "background: #667eea; color: white;",
          tags$tr(
            tags$th("COLUMN NAME", style = "padding: 10px; text-align: center;"),
            tags$th("DESCRIPTION", style = "padding: 10px;"),
            tags$th("VALID VALUES", style = "padding: 10px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$code("study"), style = "padding: 8px; text-align: center;"),
            tags$td("Study identifier", style = "padding: 8px;"),
            tags$td("Any text (e.g., \"Smith 2020\")", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("treatment"), style = "padding: 8px; text-align: center;"),
            tags$td("Treatment/intervention name", style = "padding: 8px;"),
            tags$td("Text (e.g., \"Trt X\", \"Control\")", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("events"), style = "padding: 8px; text-align: center;"),
            tags$td("Number of events in arm", style = "padding: 8px;"),
            tags$td("Non-negative integer", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("n"), style = "padding: 8px; text-align: center;"),
            tags$td("Total sample size for arm", style = "padding: 8px;"),
            tags$td("Positive integer", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(tags$code("treatment_class"), style = "padding: 8px; text-align: center;"),
            tags$td("Treatment class (OPTIONAL)", style = "padding: 8px;"),
            tags$td("Text (e.g., \"Control\", \"Active\")", style = "padding: 8px;")
          )
        )
      )
    ),
    
    # Example Dichotomous Data
    p(strong("Example Dichotomous Data")),
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-sm table-bordered",
        style = "background: white; font-size: 13px;",
        tags$thead(
          style = "background: #667eea; color: white;",
          tags$tr(
            tags$th("study", style = "padding: 10px;"),
            tags$th("treatment", style = "padding: 10px;"),
            tags$th("events", style = "padding: 10px; text-align: center;"),
            tags$th("n", style = "padding: 10px; text-align: center;"),
            tags$th("treatment_class", style = "padding: 10px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Study1", style = "padding: 8px;"),
            tags$td("Control", style = "padding: 8px;"),
            tags$td("1003", style = "padding: 8px; text-align: center;"),
            tags$td("2100", style = "padding: 8px; text-align: center;"),
            tags$td("Control", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td("Study1", style = "padding: 8px;"),
            tags$td("Trt X", style = "padding: 8px;"),
            tags$td("945", style = "padding: 8px; text-align: center;"),
            tags$td("2050", style = "padding: 8px; text-align: center;"),
            tags$td("Active", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td("Study4", style = "padding: 8px;"),
            tags$td("Trt Y", style = "padding: 8px;"),
            tags$td("408", style = "padding: 8px; text-align: center;"),
            tags$td("1100", style = "padding: 8px; text-align: center;"),
            tags$td("Active", style = "padding: 8px;")
          )
        )
      )
    ),
    
    # ==========================================================================
    # EFFECT SIZE MEASURES
    # ==========================================================================
    h2("Effect Size Measures", style = "color: #1d3557; margin-top: 50px;
                                        border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    h3("Continuous Outcomes (2 Measures)", style = "color: #1d3557; margin-top: 30px;"),
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white;",
        tags$thead(
          style = "background: #1d3557; color: white;",
          tags$tr(
            tags$th("Measure Name", style = "padding: 12px;"),
            tags$th("Code", style = "padding: 12px;"),
            tags$th("Description", style = "padding: 12px;"),
            tags$th("When to Use", style = "padding: 12px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(strong("Mean Difference"), style = "padding: 10px;"),
            tags$td(tags$code("MD"), style = "padding: 10px;"),
            tags$td("Difference in means on original scale", style = "padding: 10px;"),
            tags$td("Same units across studies", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Standardized Mean Difference"), style = "padding: 10px;"),
            tags$td(tags$code("SMD"), style = "padding: 10px;"),
            tags$td("Standardized using pooled SD", style = "padding: 10px;"),
            tags$td("Different scales", style = "padding: 10px;")
          )
        )
      )
    ),
    
    tags$hr(style = "border-color: #DEE2E6; margin: 30px 0;"),
    
    h3("Dichotomous Outcomes (3 Measures)", style = "color: #1d3557; margin-top: 30px;"),
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white;",
        tags$thead(
          style = "background: #1d3557; color: white;",
          tags$tr(
            tags$th("Measure Name", style = "padding: 12px;"),
            tags$th("Measure Code", style = "padding: 12px;"),
            tags$th("Description", style = "padding: 12px;"),
            tags$th("When to Use", style = "padding: 12px;"),
            tags$th("Interpretation", style = "padding: 12px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(strong("Odds Ratio"), style = "padding: 10px;"),
            tags$td(tags$code("OR"), style = "padding: 10px;"),
            tags$td("Ratio of odds between groups; 0.5 continuity correction", style = "padding: 10px;"),
            tags$td("Case-control designs; rare events", style = "padding: 10px;"),
            tags$td("OR=1 no effect; >1 higher odds; <1 lower odds", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Risk Ratio"), style = "padding: 10px;"),
            tags$td(tags$code("RR"), style = "padding: 10px;"),
            tags$td("Ratio of event probabilities", style = "padding: 10px;"),
            tags$td("Cohort studies, RCTs", style = "padding: 10px;"),
            tags$td("RR=1 neutral; <1 benefit; e.g., 0.6 → 40% risk reduction", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Risk Difference"), style = "padding: 10px;"),
            tags$td(tags$code("RD"), style = "padding: 10px;"),
            tags$td("Absolute difference in event rates", style = "padding: 10px;"),
            tags$td("Clinical decision-making; absolute effects needed", style = "padding: 10px;"),
            tags$td("RD=0 no effect; RD=-0.10 → 10% reduction; NNT=1/|RD|", style = "padding: 10px;")
          )
        )
      )
    ),
    
    # ==========================================================================
    # ANALYSIS WORKFLOW
    # ==========================================================================
    h2("Analysis Workflow", style = "color: #1d3557; margin-top: 50px;
                                     border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    p("MetaSuite follows a structured 5-step workflow for comprehensive network meta-analysis:"),
    
    create_step_card(1, "Load Your Data",
                     "Click 'Select Dataset' for sample data OR 'Upload File' for Excel/CSV.
                     File must match data type specifications (continuous or dichotomous).
                     Column headers must be exact (case-sensitive). Each study must have ≥ 2 treatment arms."),
    
    create_step_card(2, "Review Data Table",
                     "Navigate to Data Table tab. Review all imported data for accuracy.
                     Verify treatment names, sample sizes, and outcome data.
                     Enable 'Edit Data' mode for on-the-fly corrections. Export as CSV/Excel to preserve edits."),
    
    create_step_card(3, "Configure Model Settings",
                     "Select Outcome Type (Continuous/Dichotomous) → Effect Measure (MD/SMD/OR/RR/RD) →
                     Reference Treatment (anchor for estimates) → Model Type (Random/Common effects) →
                     τ² Estimator (REML recommended)."),
    
    create_step_card(4, "Run Analysis",
                     "Click 'Run Analysis'. Network estimates computed, direct and indirect evidence combined,
                     consistency diagnostics generated, ranking probabilities simulated.
                     Monitor loading animation while calculations proceed."),
    
    create_step_card(5, "Explore Results & Visualizations",
                     "Review Results tab (network summary, treatment effects, heterogeneity, league table).
                     Network & Forest tab (network diagram, forest plot, direct evidence).
                     Diagnostics tab (design inconsistency, radial rank plot, SUCRA beading, stacked bar plot)."),
    
    # ==========================================================================
    # VISUALIZATION FEATURES
    # ==========================================================================
    h2("Visualization Features", style = "color: #1d3557; margin-top: 50px;
                                         border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    h3("Network and Forest Plots", style = "color: #1d3557; margin-top: 30px;"),
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white;",
        tags$thead(
          style = "background: #667eea; color: white;",
          tags$tr(
            tags$th("Plot", style = "padding: 12px;"),
            tags$th("Purpose", style = "padding: 12px;"),
            tags$th("Shows", style = "padding: 12px;"),
            tags$th("How to Read", style = "padding: 12px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(strong("Network Plot"), style = "padding: 10px;"),
            tags$td("Visualize evidence structure", style = "padding: 10px;"),
            tags$td("Nodes (treatments), edges (comparisons)", style = "padding: 10px;"),
            tags$td("Thick edges = more studies", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Network Forest Plot"), style = "padding: 10px;"),
            tags$td("Compare treatment effects against reference", style = "padding: 10px;"),
            tags$td("Network effect estimates with 95% CIs", style = "padding: 10px;"),
            tags$td("Points = effects vs reference; intervals crossing null = no significance",
                    style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Direct vs Indirect Evidence"), style = "padding: 10px;"),
            tags$td("Quantify contribution of evidence sources", style = "padding: 10px;"),
            tags$td("Proportion of direct and indirect evidence for comparisons", style = "padding: 10px;"),
            tags$td("Larger indirect proportion = greater reliance on network assumptions",
                    style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Minimal Parallelism"), style = "padding: 10px;"),
            tags$td("Assess robustness of indirect comparisons", style = "padding: 10px;"),
            tags$td("Minimum number of independent evidence paths", style = "padding: 10px;"),
            tags$td("Low values = fragile support; higher values = stronger redundancy",
                    style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Mean Path Length"), style = "padding: 10px;"),
            tags$td("Evaluate complexity of indirect evidence", style = "padding: 10px;"),
            tags$td("Average number of steps linking treatments through indirect paths",
                    style = "padding: 10px;"),
            tags$td("Shorter paths = more reliable; longer paths = increased uncertainty",
                    style = "padding: 10px;")
          )
        )
      )
    ),
    
    tags$hr(style = "border-color: #DEE2E6; margin: 30px 0;"),
    
    h3("Ranking & Inconsistency Analysis", style = "color: #1d3557; margin-top: 30px;"),
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white;",
        tags$thead(
          style = "background: #667eea; color: white;",
          tags$tr(
            tags$th("Plot", style = "padding: 12px;"),
            tags$th("Purpose", style = "padding: 12px;"),
            tags$th("Shows", style = "padding: 12px;"),
            tags$th("How to Read", style = "padding: 12px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(strong("Net Heat Matrix"), style = "padding: 10px;"),
            tags$td("Identify potential sources of inconsistency", style = "padding: 10px;"),
            tags$td("Design-specific residual contributions to inconsistency", style = "padding: 10px;"),
            tags$td("Darker cells = greater inconsistency contribution; diagnostic only",
                    style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Design Q Plot"), style = "padding: 10px;"),
            tags$td("Assess between-design inconsistency", style = "padding: 10px;"),
            tags$td("Q statistics for individual designs", style = "padding: 10px;"),
            tags$td("Significant values (p < 0.05) indicate potential inconsistency",
                    style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Radial Rank Heat Plot"), style = "padding: 10px;"),
            tags$td("Visualize full ranking probability distribution", style = "padding: 10px;"),
            tags$td("Rank probabilities across all possible ranks", style = "padding: 10px;"),
            tags$td("Inner rings = best ranks; outer rings = worst; color intensity = probability",
                    style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Beading Plot (SUCRA)"), style = "padding: 10px;"),
            tags$td("Summarize average treatment ranking", style = "padding: 10px;"),
            tags$td("SUCRA values for each treatment", style = "padding: 10px;"),
            tags$td("Higher values = higher average ranking, not clinical superiority",
                    style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Stacked Ranking Plot"), style = "padding: 10px;"),
            tags$td("Visualize ranking uncertainty", style = "padding: 10px;"),
            tags$td("Cumulative rank probabilities", style = "padding: 10px;"),
            tags$td("Wide distributions indicate greater uncertainty in ranking",
                    style = "padding: 10px;")
          )
        )
      )
    ),
    
    # ==========================================================================
    # STATISTICAL TESTS & MEASURES
    # ==========================================================================
    h2("Statistical Tests & Measures", style = "color: #1d3557; margin-top: 50px;
                                               border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    h3("Heterogeneity and Inconsistency", style = "color: #1d3557; margin-top: 30px;"),
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white;",
        tags$thead(
          style = "background: #1d3557; color: white;",
          tags$tr(
            tags$th("Statistic", style = "padding: 12px;"),
            tags$th("What It Measures", style = "padding: 12px;"),
            tags$th("Interpretation", style = "padding: 12px;"),
            tags$th("Guidance", style = "padding: 12px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(strong("I²"), style = "padding: 10px;"),
            tags$td("Proportion of variability due to heterogeneity", style = "padding: 10px;"),
            tags$td("Higher values indicate greater between-study variability", style = "padding: 10px;"),
            tags$td("Descriptive; >75% suggests substantial heterogeneity", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("τ²"), style = "padding: 10px;"),
            tags$td("Absolute heterogeneity variance", style = "padding: 10px;"),
            tags$td("Larger values indicate more dispersion of true effects", style = "padding: 10px;"),
            tags$td("Compare across models, not outcomes", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Q (Total)"), style = "padding: 10px;"),
            tags$td("Overall heterogeneity in the network", style = "padding: 10px;"),
            tags$td("Significant p-value indicates heterogeneity", style = "padding: 10px;"),
            tags$td("Low power with few studies", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Q (Within-Design)"), style = "padding: 10px;"),
            tags$td("Heterogeneity within designs", style = "padding: 10px;"),
            tags$td("Reflects variability among similar comparisons", style = "padding: 10px;"),
            tags$td("Interpreted alongside τ²", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Design Q"), style = "padding: 10px;"),
            tags$td("Between-design inconsistency", style = "padding: 10px;"),
            tags$td("Significant p-value suggests disagreement between designs", style = "padding: 10px;"),
            tags$td("Indicates inconsistency, not model failure", style = "padding: 10px;")
          )
        )
      )
    ),
    
    div(
      style = "background: #E8F4F8; padding: 20px; border-radius: 8px; margin: 25px 0;
              border-left: 4px solid #667eea;",
      p(
        strong("Note:"),
        " Always interpret I², τ², and Q together, and treat inconsistency tests as supportive evidence, 
        not definitive proof.",
        style = "margin: 0; color: #1d3557;"
      )
    ),
    
    # ==========================================================================
    # QUICK REFERENCE
    # ==========================================================================
    h2("Quick Reference: Effect Size Interpretation", style = "color: #1d3557; margin-top: 50px;
                                                              border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white;",
        tags$thead(
          style = "background: #667eea; color: white;",
          tags$tr(
            tags$th("Measure", style = "padding: 12px;"),
            tags$th("Scale", style = "padding: 12px;"),
            tags$th("Neutral Value", style = "padding: 12px;"),
            tags$th("Direction", style = "padding: 12px;"),
            tags$th("Interpretation Benchmarks", style = "padding: 12px;"),
            tags$th("Example", style = "padding: 12px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(strong("Mean Difference (MD)"), style = "padding: 10px;"),
            tags$td("Original units", style = "padding: 10px;"),
            tags$td("0", style = "padding: 10px;"),
            tags$td(">0 higher · <0 lower", style = "padding: 10px;"),
            tags$td("Clinical relevance depends on units", style = "padding: 10px;"),
            tags$td("MD = −5 mmHg", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Odds Ratio (OR)"), style = "padding: 10px;"),
            tags$td("Ratio", style = "padding: 10px;"),
            tags$td("1", style = "padding: 10px;"),
            tags$td(">1 higher odds · <1 lower odds", style = "padding: 10px;"),
            tags$td("Distance from 1 = stronger effect", style = "padding: 10px;"),
            tags$td("OR = 2.0", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Risk Ratio (RR)"), style = "padding: 10px;"),
            tags$td("Ratio", style = "padding: 10px;"),
            tags$td("1", style = "padding: 10px;"),
            tags$td(">1 higher risk · <1 lower risk", style = "padding: 10px;"),
            tags$td("RR <1 indicates risk reduction", style = "padding: 10px;"),
            tags$td("RR = 0.75 (25% RRR)", style = "padding: 10px;")
          ),
          tags$tr(
            tags$td(strong("Risk Difference (RD)"), style = "padding: 10px;"),
            tags$td("Absolute", style = "padding: 10px;"),
            tags$td("0", style = "padding: 10px;"),
            tags$td(">0 higher · <0 lower", style = "padding: 10px;"),
            tags$td("Absolute effect; NNT = 1/|RD|", style = "padding: 10px;"),
            tags$td("RD = -0.10 → NNT=10", style = "padding: 10px;")
          )
        )
      )
    ),
    
    # ==========================================================================
    # CUSTOMIZATION OPTIONS
    # ==========================================================================
    h2("Customization Options", style = "color: #1d3557; margin-top: 50px;
                                        border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    div(
      style = "overflow-x: auto; margin: 20px 0;",
      tags$table(
        class = "table table-bordered",
        style = "background: white; font-size: 13px;",
        tags$thead(
          style = "background: #667eea; color: white;",
          tags$tr(
            tags$th("Category", style = "padding: 10px;"),
            tags$th("Applies To", style = "padding: 10px;"),
            tags$th("Customizable Elements", style = "padding: 10px;"),
            tags$th("Notes / Best Practice", style = "padding: 10px;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(strong("Color Customization"), style = "padding: 8px;"),
            tags$td("Dual Network Plot", style = "padding: 8px;"),
            tags$td("Treatment nodes, edges, classes, color palettes", style = "padding: 8px;"),
            tags$td("Maintain identical color mapping across panels", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Color Customization"), style = "padding: 8px;"),
            tags$td("Network Forest Plot", style = "padding: 8px;"),
            tags$td("Points, CIs, reference line", style = "padding: 8px;"),
            tags$td("Use high contrast for reference treatment", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Color Customization"), style = "padding: 8px;"),
            tags$td("Net Heat Matrix, Design Q", style = "padding: 8px;"),
            tags$td("Heat intensity scale, significant colors", style = "padding: 8px;"),
            tags$td("Darker = higher inconsistency; diagnostic only", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Color Customization"), style = "padding: 8px;"),
            tags$td("Ranking Plots", style = "padding: 8px;"),
            tags$td("Points, tiles, gradients, color palettes", style = "padding: 8px;"),
            tags$td("Color intensity = probability, not effect magnitude", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Node Size Adjustment"), style = "padding: 8px;"),
            tags$td("Network Plot", style = "padding: 8px;"),
            tags$td("Treatment nodes", style = "padding: 8px;"),
            tags$td("Node size reflects evidence volume, not treatment effect", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Edge Width Adjustment"), style = "padding: 8px;"),
            tags$td("Network Plot", style = "padding: 8px;"),
            tags$td("Comparison edges", style = "padding: 8px;"),
            tags$td("Thicker edges = more studies contributing", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Typography"), style = "padding: 8px;"),
            tags$td("All plots", style = "padding: 8px;"),
            tags$td("Axis labels, legends, titles", style = "padding: 8px;"),
            tags$td("Recommended ≥10-12 pt for readability", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Font Size Guidance"), style = "padding: 8px;"),
            tags$td("All plots", style = "padding: 8px;"),
            tags$td("Global text scaling", style = "padding: 8px;"),
            tags$td("Increase for presentations; reduce for publications", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Export Format"), style = "padding: 8px;"),
            tags$td("All NMA plots", style = "padding: 8px;"),
            tags$td("PNG (transparent), JPG (white background)", style = "padding: 8px;"),
            tags$td("PNG preferred for publications", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Export Quality"), style = "padding: 8px;"),
            tags$td("All NMA plots", style = "padding: 8px;"),
            tags$td("300 DPI resolution", style = "padding: 8px;"),
            tags$td("Publication-ready figures", style = "padding: 8px;")
          ),
          tags$tr(
            tags$td(strong("Export Dimensions"), style = "padding: 8px;"),
            tags$td("All NMA plots", style = "padding: 8px;"),
            tags$td("Output width and height", style = "padding: 8px;"),
            tags$td("Default ≈6×4 inches; suitable for journals", style = "padding: 8px;")
          )
        )
      )
    ),
    
    # ==========================================================================
    # TROUBLESHOOTING GUIDE
    # ==========================================================================
    h2("Troubleshooting Guide", style = "color: #1d3557; margin-top: 50px;
                                        border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    h3("Common Issues & Solutions", style = "color: #1d3557; margin-top: 30px;"),
    
    create_troubleshooting_item_rob(
      icon_name = "triangle-exclamation",
      icon_color = "#dc3545",
      title = "Error: \"Invalid data format\"",
      causes = c("Column names don't match specifications exactly",
                 "Data type mismatch (text in numeric column, vice versa)",
                 "Extra spaces or special characters in headers"),
      solution = "Check column names are exactly: study, treatment, mean, sd, n (continuous) OR 
                  study, treatment, events, n (dichotomous). Names are case-sensitive. 
                  Remove leading/trailing spaces. Download sample dataset and compare formatting."
    ),
    
    create_troubleshooting_item_rob(
      icon_name = "project-diagram",
      icon_color = "#f39c12",
      title = "Error: \"Network is not connected\"",
      causes = c("At least one treatment lacks valid comparison path linking to rest of network",
                 "Island subnetworks with no connecting studies",
                 "Inconsistent treatment naming across studies"),
      solution = "Inspect Network Plot to identify isolated treatments. Verify consistent treatment naming. 
                  Remove isolated treatments or include valid comparisons to restore connectivity. 
                  Note: A connected network is a prerequisite for network meta-analysis."
    ),
    
    create_troubleshooting_item_rob(
      icon_name = "chart-bar",
      icon_color = "#667eea",
      title = "Warning: \"Rankings unstable\"",
      causes = c("High uncertainty in treatment ranking probabilities",
                 "Sparse network structure",
                 "Few studies contributing to key comparisons",
                 "High heterogeneity across studies"),
      solution = "Examine rankograms and radial rank heat plots to assess ranking dispersion. 
                  Avoid relying solely on SUCRA or average rank summaries. 
                  Emphasize effect estimates and confidence intervals when interpreting results. 
                  Explicitly report ranking uncertainty."
    ),
    
    # ==========================================================================
    # PRO TIPS & BEST PRACTICES (WITH YELLOW BACKGROUND LIKE ROB MODULE)
    # ==========================================================================
    h2("Pro Tips & Best Practices", style = "color: #1d3557; margin-top: 50px;
                                         border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    
    div(
      style = "background: #FFF3CD; padding: 25px; border-radius: 8px; margin-top: 20px;
          border-left: 4px solid #F39C12;",
      h4(icon("lightbulb"), " Pro Tips",
         style = "color: #856404; margin-top: 0;"),
      tags$ul(
        style = "color: #856404; margin: 0; line-height: 1.8;",
        tags$li(strong("Use Sample Datasets:"), " Load sample datasets first to explore features, familiarize yourself with expected output, and verify your data formatting matches examples"),
        tags$li(strong("Data Preparation:"), " Double-check data entry (common source of errors), ensure consistent units across studies, standardize study identifier format (e.g., Author Year), create backup of original dataset before editing"),
        tags$li(strong("Model Selection:"), " Always start with random-effects model; only switch to common-effects if I² <25% AND studies are nearly identical; document model choice and justification in methods"),
        tags$li(strong("Effect Measure Selection:"), " For continuous outcomes, use SMD as default; consider MD only if all studies use identical scale. For dichotomous outcomes: prefer RR for RCTs/cohorts, OR for case-control, RD when absolute risk interpretation is required"),
        tags$li(strong("High Heterogeneity (I² >75%):"), " Examine study design, populations, interventions, and outcome definitions; interpret pooled estimates with caution; clearly report heterogeneity statistics and discuss implications in limitations"),
        tags$li(strong("Visualization:"), " Adjust color schemes to comply with journal requirements before exporting; use consistent color mapping across all network plots; increase text size for presentations, reduce for publications while maintaining readability"),
        tags$li(strong("High-Resolution Export:"), " All figures should be exported at 300 DPI for publication use; PNG format preferred for transparent backgrounds; always verify plot appearance in target format prior to final submission"),
        tags$li(strong("Comprehensive Reporting:"), " Present effect estimates with 95% confidence intervals; report heterogeneity and inconsistency statistics; describe model choice and assumptions explicitly; include network characteristics (number of studies, treatments, comparisons)")
      )
    ),
    
    # ==========================================================================
    # ADDITIONAL RESOURCES - ALL 10 REFERENCES
    # ==========================================================================
    h2("Additional Resources", style = "color: #1d3557; margin-top: 50px; border-bottom: 2px solid #1d3557; padding-bottom: 10px;"),
    h3("Key References", style = "color: #1d3557; margin-top: 30px;"),
    tags$ol(
      style = "line-height: 2;",
      tags$li(
        "Caldwell, D. M., Ades, A. E., & Higgins, J. P. T. (2005). Simultaneous comparison of multiple treatments: Combining direct and indirect evidence. ",
        tags$em("BMJ"), ", ", tags$em("331"), "(7521), 897–900. ",
        tags$a("https://doi.org/10.1136/bmj.331.7521.897", href = "https://doi.org/10.1136/bmj.331.7521.897", target = "_blank")
      ),
      tags$li(
        "Lu, G., & Ades, A. E. (2004). Combination of direct and indirect evidence in mixed treatment comparisons. ",
        tags$em("Statistics in Medicine"), ", ", tags$em("23"), "(20), 3105–3124. ",
        tags$a("https://doi.org/10.1002/sim.1875", href = "https://doi.org/10.1002/sim.1875", target = "_blank")
      ),
      tags$li(
        "Salanti, G., Ades, A. E., & Ioannidis, J. P. A. (2011). Graphical methods and numerical summaries for presenting results from multiple-treatment meta-analysis: An overview and tutorial. ",
        tags$em("Journal of Clinical Epidemiology"), ", ", tags$em("64"), "(2), 163–171. ",
        tags$a("https://doi.org/10.1016/j.jclinepi.2010.03.016", href = "https://doi.org/10.1016/j.jclinepi.2010.03.016", target = "_blank")
      ),
      tags$li(
        "Hutton, B., Salanti, G., Caldwell, D. M., Chaimani, A., Schmid, C. H., Cameron, C., … Moher, D. (2015). The PRISMA extension statement for reporting of systematic reviews incorporating network meta-analyses of health care interventions. ",
        tags$em("Annals of Internal Medicine"), ", ", tags$em("162"), "(11), 777–784. ",
        tags$a("https://doi.org/10.7326/M14-2385", href = "https://doi.org/10.7326/M14-2385", target = "_blank")
      ),
      tags$li(
        "Veroniki, A. A., Straus, S. E., Fyraridis, A., & Tricco, A. C. (2016). The rank-heat plot is a novel way to present the results from a network meta-analysis including multiple outcomes. ",
        tags$em("Journal of Clinical Epidemiology"), ", ", tags$em("76"), ", 193–199. ",
        tags$a("https://doi.org/10.1016/j.jclinepi.2016.02.016", href = "https://doi.org/10.1016/j.jclinepi.2016.02.016", target = "_blank")
      ),
      tags$li(
        "Chen, C., Chuang, Y.-C., Chan, E., et al. (2023, September 28). Beading plot: A novel graphics for ranking interventions in network evidence (Version 1) [Preprint]. ",
        tags$em("Research Square"), ". ",
        tags$a("https://doi.org/10.21203/rs.3.rs-3370844/v1", href = "https://doi.org/10.21203/rs.3.rs-3370844/v1", target = "_blank")
      ),
      tags$li(
        "Krahn, U., Binder, H., & König, J. (2013). A graphical tool for locating inconsistency in network meta-analyses. ",
        tags$em("BMC Medical Research Methodology"), ", ", tags$em("13"), ", Article 35. ",
        tags$a("https://doi.org/10.1186/1471-2288-13-35", href = "https://doi.org/10.1186/1471-2288-13-35", target = "_blank")
      ),
      tags$li(
        "Rücker, G., Schwarzer, G., Krahn, U., & König, J. (2015). Netmeta: Network meta-analysis using frequentist methods. ",
        tags$em("R Journal"), ", ", tags$em("7"), "(2), 315–324. ",
        tags$a("https://doi.org/10.32614/RJ-2015-023", href = "https://doi.org/10.32614/RJ-2015-023", target = "_blank")
      ),
      tags$li(
        "Jansen, J. P., Trikalinos, T., Cappelleri, J. C., Daw, J., Andes, S., Eldessouki, R., … Salanti, G. (2014). Indirect treatment comparison/network meta-analysis study questionnaire to assess relevance and credibility to inform health care decision making. ",
        tags$em("Value in Health"), ", ", tags$em("17"), "(2), 157–173. ",
        tags$a("https://doi.org/10.1016/j.jval.2014.01.004", href = "https://doi.org/10.1016/j.jval.2014.01.004", target = "_blank")
      ),
      tags$li(
        "Nikolakopoulou, A., Chaimani, A., Veroniki, A. A., Vasiliadis, H. S., Schmid, C. H., & Salanti, G. (2014). Characteristics of networks of interventions: A description of a database of 186 published networks. ",
        tags$em("PLoS ONE"), ", ", tags$em("9"), "(1), e86754. ",
        tags$a("https://doi.org/10.1371/journal.pone.0086754", href = "https://doi.org/10.1371/journal.pone.0086754", target = "_blank")
      )
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

