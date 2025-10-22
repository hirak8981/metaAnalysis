# =============================================================================
# server.R - Main Server Router
# =============================================================================

server <- function(input, output, session) {
  
  # Set session timeout (30 minutes = 1800000 milliseconds)
  session$allowReconnect(FALSE)  # Disable reconnection attempts
  
  # Initialize module servers
  landingPageServer("landing")
  robMainServer("rob")
  metaMainServer("meta")
  networkMainServer("network")
  documentationServer("docs")
  
  # Session tracking
  session$onSessionEnded(function() {
    cat("Session ended at", as.character(Sys.time()), "\n")
  })
  
  # Optional: Handle session warning (from JS)
  observeEvent(input$session_warning, {
    showNotification(
      "Your session will expire in 2 minutes due to inactivity.",
      type = "warning",
      duration = 120
    )
  })
}
