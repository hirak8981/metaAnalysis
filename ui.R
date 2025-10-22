# =============================================================================
# ui.R - Main Navigation
# =============================================================================

ui <- bslib::page_navbar(
  id = "main_navbar",
  title = "MetaSuite",
  theme = get_app_theme(),
  fillable = FALSE,
  
  # Use header parameter for non-navigation content
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "css/loading-indicator.css"),
      tags$link(rel = "stylesheet", href = "css/banter-loader.css"),
      tags$link(rel = "stylesheet", href = "css/landing-page.css"),
      tags$link(rel = "stylesheet", href = "css/session-handler.css"),
      # Font Awesome for icons
      tags$link(rel = "stylesheet", 
                href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css")
    ),
    tags$script(src = "js/banter-loader.js"),
    tags$script(src = "js/loading-indicator.js"),
    tags$script(src = "js/carousel.js"),
    tags$script(src = "js/session-handler.js"),
    shinyjs::useShinyjs()
  ),
  
  # Navigation Tabs - Only nav_panel() items here
  bslib::nav_panel(
    title = tagList(icon("home"), " Home"),
    value = "home_tab",
    landingPageUI("landing")
  ),
  
  bslib::nav_panel(
    title = tagList(icon("shield-halved"), " Risk of Bias"),
    value = "rob_tab",
    robMainUI("rob")
  ),
  
  bslib::nav_panel(
    title = tagList(icon("chart-line"), " Meta-Analysis"),
    value = "meta_tab",
    metaMainUI("meta")
  ),
  
  bslib::nav_panel(
    title = tagList(icon("diagram-project"), " Network Meta-Analysis"),
    value = "network_tab",
    networkMainUI("network")
  ),
  
  bslib::nav_panel(
    title = tagList(icon("book"), " Documentation"),
    value = "docs_tab",
    documentationUI("docs")
  )
  
)
