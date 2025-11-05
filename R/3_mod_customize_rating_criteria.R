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
mod_customize_rating_criteria_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Customize Rating Criteria",
    value = id,
    icon = icon("square-check"),
    em("Select and customize the threshold and rating criteria you will use to evaluate your CoC's projects"),
    navset_tab(
      id = "rating_criteria_subtabs",
      mod_customize_coc_thresholds_ui(ns("coc_thresholds")),
      mod_customize_rating_factors_ui(ns("renewal_rating_factors"), "Renew"),
      mod_customize_rating_factors_ui(ns("new_rating_factors"), "New")
    )
  )
}

#' @title mod_rating_criteria_server
#'
#' @description Server logic for the main rating criteria page.
#' @param id The module's unique ID.
#' @param user_coc contains coc_version_id to capture user-selected version of the ORR
#' @noRd
mod_customize_rating_criteria_server <- function(id, nav_control, user_coc, parent_session, module_returns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Call sub-modules for each tab
    mod_customize_coc_thresholds_server("coc_thresholds", user_coc, nav_control)
    mod_customize_rating_factors_server("renewal_rating_factors", user_coc, "Renew", module_returns)
    mod_customize_rating_factors_server("new_rating_factors", user_coc, "New", module_returns)
  })
}
