mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    'My Dashboard', 
    value = id,
    icon = icon("home"),
    mod_coc_selection_ui(ns("coc_selection")),
    mod_requests_ui(ns("requests"))
  )
}

mod_dashboard_server <- function(id, nav_control, user_coc, parent_session, module_returns) {
  moduleServer(id, function(input, output, session) {
    get_coc_versions_for_user <- function(username) {
      get_db_query(
        "SELECT v.*, u.username, u.coc_version_role, v.created_by
        FROM coc_versions v
        LEFT JOIN coc_version_users u
        ON v.coc_version_id = u.coc_version_id
        WHERE u.username = $1
        ",
        params = username
      )
    }
    
    mod_coc_selection_server("coc_selection", nav_control, user_coc, parent_session)
    mod_requests_server("requests", user_coc)
  })
}
  