# =============================================================================
# ROB Domain Name Mapper (Updated with multi-line captions)
# =============================================================================

#' Get domain name mapping for ROB tools
#' @param tool_name Name of ROB tool ("quadas", "quips", "rob2", "robins_i", "robins_e")
#' @return Named vector with short codes (D1, D2...) as names and full descriptions as values
get_domain_names <- function(tool_name) {
  
  domain_maps <- list(
    
    # QUADAS-2 (4 domains + Overall)
    quadas = c(
      "D1" = "Patient selection",
      "D2" = "Index test",
      "D3" = "Reference standard",
      "D4" = "Flow & timing",
      "Overall" = "Overall"
    ),
    
    # QUIPS (6 domains + Overall)
    quips = c(
      "D1" = "Bias due to participation",
      "D2" = "Bias due to attrition",
      "D3" = "Bias due to prognostic factor measurement",
      "D4" = "Bias due to outcome measurement",
      "D5" = "Bias due to confounding",
      "D6" = "Bias in statistical analysis and reporting",
      "Overall" = "Overall"
    ),
    
    # ROB 2 (5 domains + Overall)
    rob2 = c(
      "D1" = "Bias arising from randomization process",
      "D2" = "Bias due to deviations from intended interventions",
      "D3" = "Bias due to missing outcome data",
      "D4" = "Bias in measurement of the outcome",
      "D5" = "Bias in selection of the reported results",
      "Overall" = "Overall"
    ),
    
    # ROBINS-I (7 domains + Overall)
    robins_i = c(
      "D1" = "Bias due to confounding",
      "D2" = "Bias due to selection of participants",
      "D3" = "Bias in classification of interventions",
      "D4" = "Bias due to deviations from intended interventions",
      "D5" = "Bias due to missing data",
      "D6" = "Bias in measurement of outcomes",
      "D7" = "Bias in selection of the reported result",
      "Overall" = "Overall"
    ),
    
    # ROBINS-E (7 domains + Overall)
    robins_e = c(
      "D1" = "Bias due to confounding",
      "D2" = "Bias arising from measurement of the exposure",
      "D3" = "Bias in selection of participants into the study",
      "D4" = "Bias due to post-exposure interventions",
      "D5" = "Bias due to missing data",
      "D6" = "Bias arising from measurement of the outcome",
      "D7" = "Bias in selection of the reported result",
      "Overall" = "Overall"
    )
  )
  
  if (tool_name %in% names(domain_maps)) {
    return(domain_maps[[tool_name]])
  } else {
    warning(paste("Unknown tool:", tool_name, ". Returning NULL."))
    return(NULL)
  }
}

#' Create multi-line caption text for traffic plot with domain descriptions
#' @param domain_names Named vector from get_domain_names()
#' @return Formatted text string for plot caption (with line breaks)
create_domain_caption <- function(domain_names) {
  # Remove Overall from caption
  domains_only <- domain_names[names(domain_names) != "Overall"]
  
  # Format each domain on a new line: "D1: Description"
  caption_lines <- paste0(names(domains_only), ": ", domains_only)
  
  # Join with newlines
  caption_text <- paste(caption_lines, collapse = "\n")
  
  return(caption_text)
}

#' Replace domain codes with full names in data frame
#' @param df Data frame with D1, D2, D3... columns
#' @param domain_names Named vector from get_domain_names()
#' @return Data frame with renamed columns
rename_domains <- function(df, domain_names) {
  if (is.null(domain_names)) return(df)
  
  # Get column names
  cols <- colnames(df)
  
  # Replace domain codes with full names
  for (code in names(domain_names)) {
    if (code %in% cols) {
      cols[cols == code] <- domain_names[code]
    }
  }
  
  colnames(df) <- cols
  return(df)
}
