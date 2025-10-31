# /modules/mod_rating_criteria.R

#-------------------------------------------------------------------------------
# 1. Main Rating Criteria Module
# - This module acts as the container for the entire "Customize Rating Criteria" page.
# - It uses bslib::navset_card_tab to create the three main sections.
#-------------------------------------------------------------------------------

#' @title mod_rating_criteria_ui
#'
#' @description UI for the main rating criteria page.
#' @param id The module's unique ID.
#' @noRd
mod_rating_criteria_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Customize Rating Criteria",
    value = id,
    em("Select and customize the threshold and rating criteria you will use to evaluate your CoC's projects"),
    navset_tab(
      id = "rating_criteria_subtabs",
      mod_coc_thresholds_ui(ns("coc_thresholds")),
      mod_renewal_factors_ui(ns("renewal_factors")),
      mod_new_factors_ui(ns("new_factors")) 
    )
  )
}

#' @title mod_rating_criteria_server
#'
#' @description Server logic for the main rating criteria page.
#' @param id The module's unique ID.
#' @param user_coc contains coc_version_id to capture user-selected version of the ORR
#' @noRd
mod_rating_criteria_server <- function(id, nav_control, user_coc, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Call sub-modules for each tab
    mod_coc_thresholds_server("coc_thresholds", user_coc)
    
    mod_renewal_factors_server(
      "renewal_factors", 
      user_coc
    )
    
    mod_new_factors_server(
      "new_factors", 
      user_coc
    )
  })
}


source(here("R/mod_coc_thresholds.R"))
source(here("R/mod_rating_factors.R"))
