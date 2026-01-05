# =============================================================================
# app.R - Main Entry Point
# =============================================================================

source("global.R")
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shinyApp(ui = ui, server = server)


