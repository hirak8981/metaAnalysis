# =============================================================================
# R/theme.R - Complete theme configuration
# Penn Blue (#1d3557) color scheme with Lux theme
# =============================================================================

#' Get app theme - Using Lux with Penn Blue
#' @return bslib theme object
get_app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bootswatch = "lux",
    primary = "#1d3557",
    secondary = "#95A5A6",
    success = "#27AE60",
    info = "#457b9d",
    warning = "#F39C12",
    danger = "#e63946"
  )
}

#' Get custom CSS - Bold DataTable and styling
#' @return HTML tags with CSS
get_custom_css <- function() {
  tags$head(
    tags$style(HTML("
      /* =================================================================
         GENERAL STYLING
         ================================================================= */
      
      body {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
      }
      
      h1, h2, h3, h4, h5, h6 {
        font-weight: 600;
        color: #1d3557;
      }
      
      /* =================================================================
         DATATABLE - BOLD TEXT
         ================================================================= */
      
      .dataTables_wrapper {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
        font-size: 14px;
      }
      
      /* Table header - bold and dark blue */
      table.dataTable thead th {
        background-color: #1d3557 !important;
        color: #ffffff !important;
        font-weight: 700 !important;
        font-size: 14px !important;
        text-align: center;
        padding: 12px 10px !important;
      }
      
      /* Table body cells - BOLD */
      table.dataTable tbody td {
        font-size: 10px !important;
        font-weight: 500 !important;
        padding: 8px !important;
        text-align: center;
        color: #5A6169 !important;
      }
      
      /* Filter inputs */
      table.dataTable thead input,
      table.dataTable thead select {
        font-size: 13px !important;
        font-weight: 400;
      }
      
      /* DataTable info text */
      .dataTables_wrapper .dataTables_info {
        font-size: 14px !important;
        font-weight: 400;
        color: #5A6169;
      }
      
      /* Row hover effect */
      table.dataTable tbody tr:hover {
        background-color: #f8f9fa;
      }
      
      /* Striped rows */
      table.dataTable.stripe tbody tr.odd {
        background-color: #ffffff;
      }
      
      table.dataTable.stripe tbody tr.even {
        background-color: #fafbfc;
      }
      
      /* =================================================================
         BUTTON STYLING
         ================================================================= */
      
      .btn-primary {
        background-color: #1d3557;
        border-color: #1d3557;
        color: #ffffff;
        font-weight: 600;
      }
      
      .btn-primary:hover {
        background-color: #2a4a73;
        border-color: #2a4a73;
      }
      
      .btn-sm {
        font-size: 12px;
        font-weight: 600;
      }
      
      /* =================================================================
         CARD STYLING
         ================================================================= */
      
      .card {
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.08);
        margin-bottom: 20px;
        border: 1px solid #e0e0e0;
      }
      
      .card-header {
        background-color: #1d3557;
        color: #ffffff;
        border-bottom: none;
        font-weight: 600;
        padding: 15px 20px;
        border-radius: 8px 8px 0 0;
      }
      
      .card-body {
        padding: 20px;
      }
      
      /* =================================================================
         SIDEBAR
         ================================================================= */
      
      .bslib-sidebar-layout > .sidebar {
        background-color: #fafbfc;
        border-right: 1px solid #e0e0e0;
      }
      
      .sidebar h4, .sidebar h5 {
        color: #1d3557;
        font-weight: 700;
      }
      
      /* =================================================================
         FORM CONTROLS
         ================================================================= */
      
      .form-control, .form-select {
        font-size: 14px;
        font-weight: 500;
      }
      
      .form-control:focus,
      .form-select:focus {
        border-color: #1d3557;
        box-shadow: 0 0 0 0.2rem rgba(29, 53, 87, 0.25);
      }
      
      label {
        font-weight: 600;
        font-size: 13px;
        color: #1d3557;
      }
      
      /* Color picker labels */
      .colourpicker-input-container label {
        color: #1d3557;
        font-weight: 600;
      }
      
      /* =================================================================
         GENERAL TEXT
         ================================================================= */
      
      p {
        font-size: 14px;
        line-height: 1.6;
      }
    "))
  )
}
