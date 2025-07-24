function(input, output, session) {
  projects_data <- reactiveVal(NULL)
  selected_coc <- reactiveVal(NULL)
  nav_control <- reactiveVal("coc_selection")
  
  # Hide all panels except CoC selection initially
  observe({
    nav_hide("nav", "inventory")
    nav_hide("nav", "rating_criteria")
    nav_hide("nav", "renewal_rating")
    nav_hide("nav", "new_rating")
    nav_hide("nav", "alternative_rating")
    nav_hide("nav", "funding_priorities")
    nav_hide("nav", "ranking")

    if (!is.null(selected_coc()) && selected_coc() != "") {
      nav_show("nav", "inventory")
      nav_show("nav", "rating_criteria")
      nav_show("nav", "renewal_rating")
      nav_show("nav", "new_rating")
      nav_show("nav", "alternative_rating")
      nav_show("nav", "funding_priorities")
    }
  })
 
  
  con <-  dbConnect(
    RPostgres::Postgres(),
    host = DB_CONFIG$host,
    port = DB_CONFIG$port,
    dbname = DB_CONFIG$dbname,
    user = DB_CONFIG$username,
    password = DB_CONFIG$password,
    sslmode = "require"
  )
  
  coc_iu_val <- mod_coc_selection_server("coc_selection", nav_control, projects_data, selected_coc, con)
  mod_inventory_server("inventory", projects_data)
  mod_rating_criteria_server("rating_criteria")
  mod_renewal_rating_server("renewal_rating", projects_data)
  mod_new_rating_server("new_rating", projects_data)
  mod_alternative_rating_server("bulk_rating", projects_data)
  mod_funding_priorities_server("funding_priorities")
  mod_ranking_server("ranking")
  mod_requests_server(id="requests", coc_iu = coc_iu_val)
  # Observer to update nav panel when reactive value changes
  observeEvent(nav_control(), {
    nav_select("nav", selected = nav_control())
  })
}