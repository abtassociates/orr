# UI modules for project rating
mod_rating_ui <- function(id) {
  ns <- NS(id)
  
  # Individual Renewal/Expansion Rating
  nav_menu(
    title = "Rate Projects",
    icon = icon("star"),
    value = id,
    nav_panel(
      title = "Renew/Expand Rating",
      value = ns("renew_rating"),
      mod_project_rating_ui(ns("renew_rating"), "Renew")
    ),
    nav_panel(
      title = "New Rating",
      value = ns("new_rating"),
      mod_project_rating_ui(ns("new_rating"), "New")
    ),
    nav_panel(
      title = "Bulk Rating",
      value = ns("alternative_rating"),
      mod_alternative_rating_ui(ns("alternative_rating"))
    ),
    nav_panel(
      title = "Rating summary",
      value = ns("rating_summary"),
      mod_rating_summary_ui(ns("rating_summary"))
    )
  )
}

mod_rating_server <- function(id, nav_control, user_coc, parent_session, module_returns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # selected_criteria <- reactive({
    #   req(user_coc$coc_version_id)
    #   
    #   factors <- get_db_query(
    #     "SELECT selected_rating_factor_id FROM selected_rating_factors WHERE coc_version_id = $1", 
    #     params = user_coc$coc_version_id
    #   )$selected_rating_factor_id
    #   
    #   thresholds <- get_db_query(
    #     "SELECT selected_threshold_id FROM selected_thresholds WHERE coc_version_id = $1", 
    #     params = user_coc$coc_version_id
    #   )$selected_threshold_id
    #   
    #   c(factors, thresholds)
    # })
    
    # selected_criteria <- module_returns$rating_criteria
    
    observe({
      req(module_returns$rating_criteria)
      
      if(length(module_returns$rating_criteria) > 0) {
        nav_show("nav", target = "renew_rating", session = parent_session)
        nav_show("nav", target = "new_rating", session = parent_session)
      } else {
        nav_hide("nav", target = "renew_rating", session = parent_session)
        nav_hide("nav", target = "new_rating", session = parent_session)
      }
    })
    
    mod_project_rating_server("renew_rating", user_coc, "Renew", module_returns)
    mod_project_rating_server("new_rating", user_coc, "New", module_returns)
    mod_rating_summary_server("rating_summary")
    mod_alternative_rating_server("alternative_rating")
  })
}