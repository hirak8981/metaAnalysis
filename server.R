# =============================================================================
# server.R - Main Server Router
# =============================================================================

server <- function(input, output, session) {
  
  # Initialize module servers
  landingPageServer("landing")
  robMainServer("rob")
  metaMainServer("meta")
  networkMainServer("network")
  
  # Session tracking
  session$onSessionEnded(function() {
    cat("Session ended at", as.character(Sys.time()), "\n")
  })
}
