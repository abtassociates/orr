#' @title mod_customize_criteria_ui
#'
#' @description UI for the main customize criteria page.
#' @param id The module's unique ID.
#' @noRd
mod_customize_criteria_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Customize Rating Criteria",
    value = id,
    br(),
    em("Select and customize the threshold and rating criteria you will use to evaluate your CoC's projects"),
    navset_tab(
      id = ns("rating_criteria_subtabs"),
      mod_customize_coc_thresholds_ui(ns("coc_thresholds")),
      nav_panel(
        "Rating Factors",
        value = ns("rating_factors"),
        navset_tab(
          id = ns("rating_factors_subtabs"),
          mod_customize_rating_factors_ui(ns("renewal_rating_factors"), "Renew"),
          mod_customize_rating_factors_ui(ns("new_rating_factors"), "New")
        )
      )
    )
  )
}

#' @title mod_rating_criteria_server
#'
#' @description Server logic for the main rating criteria page.
#' @param id The module's unique ID.
#' @param user_coc contains coc_version_id to capture user-selected version of the ORR
#' @noRd
mod_customize_criteria_server <- function(id, user_coc, nav_control, parent_session, module_returns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$rating_criteria_subtabs, {
      req(!is.null(user_coc$coc_version_id) & nav_control() == 'rating')
      user_coc$settings$rating_subtab <- gsub('rating-customize_criteria-', '', input$rating_criteria_subtabs)
    }, ignoreInit = TRUE)
    
    observeEvent(input$rating_factors_subtabs, {
      req(!is.null(user_coc$coc_version_id) & nav_control() == 'rating')
      user_coc$settings$rating_subsubtab <- gsub('rating-customize_criteria-', '', input$rating_factors_subtabs)
      
    }, ignoreInit = TRUE)
    
    # Call sub-modules for each tab
    mod_customize_coc_thresholds_server("coc_thresholds", user_coc, nav_control, module_returns)
    mod_customize_rating_factors_server("renewal_rating_factors", user_coc, "Renew", nav_control, module_returns)
    mod_customize_rating_factors_server("new_rating_factors", user_coc, "New", nav_control, module_returns)
  })
}