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
mod_customize_criteria_server <- function(id, user_coc, nav_control, parent_session, help_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$rating_criteria_subtabs, {
      req(!is.null(user_coc$coc_version_id) & nav_control() == 'rating')
      
      update_user_coc_setting(user_coc, "rating_subtab", input$rating_criteria_subtabs)
      
      if(input$rating_criteria_subtabs == ns("rating_factors"))
        help_id(ns("renewal_rating_factors"))
      else
        help_id(input$rating_criteria_subtabs)
      
    }, ignoreInit = TRUE)
    
    observeEvent(input$rating_factors_subtabs, {
      req(!is.null(user_coc$coc_version_id) & nav_control() == 'rating')
      
      update_user_coc_setting(user_coc, "rating_subsubtab", input$rating_factors_subtabs)
      
      help_id(input$rating_factors_subtabs)
    }, ignoreInit = TRUE)
    
    # ---------------------------------------------------------
    # 1. Define internal "Active" reactives for each sub-tab
    # ---------------------------------------------------------
    
    # Thresholds is active IF:
    # - Parent module is active AND 
    # - Level 1 tab is 'coc_thresholds'
    is_thresholds_active <- reactive({
      input$rating_criteria_subtabs == ns("coc_thresholds")
    })
    
    # Renewal Factors is active IF:
    # - Parent module is active AND 
    # - Level 1 tab is 'rating_factors' AND
    # - Level 2 tab is 'renewal_rating_factors'
    is_renewal_active <- reactive({
      input$rating_criteria_subtabs == ns("rating_factors") && 
      input$rating_factors_subtabs == ns("renewal_rating_factors")
    })
    
    # New Factors is active IF:
    # - Parent module is active AND 
    # - Level 1 tab is 'rating_factors' AND
    # - Level 2 tab is 'new_rating_factors'
    is_new_active <- reactive({
      input$rating_criteria_subtabs == ns("rating_factors") && 
      input$rating_factors_subtabs == ns("new_rating_factors")
    })
    
    # Call sub-modules for each tab
    mod_customize_coc_thresholds_server("coc_thresholds", user_coc, nav_control, active = is_thresholds_active)
    mod_customize_rating_factors_server("renewal_rating_factors", user_coc, "Renew", nav_control, active = is_renewal_active)
    mod_customize_rating_factors_server("new_rating_factors", user_coc, "New", nav_control, active =is_new_active)
  })
}