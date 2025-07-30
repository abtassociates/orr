function(input, output, session) {
  projects_data <- reactiveVal(NULL)
  selected_coc <- reactiveValues(
    coc = NULL,
    coc_instance_id = NULL
  )
  nav_control <- reactiveVal("coc_selection")
  username <- reactiveVal("alex.silverman@abtglobal.com")
  
  # Hide all panels except CoC selection initially
  observe({
    nav_hide("nav", "inventory")
    nav_hide("nav", "rating_criteria")
    nav_hide("nav", "renewal_rating")
    nav_hide("nav", "new_rating")
    nav_hide("nav", "alternative_rating")
    nav_hide("nav", "funding_priorities")
    nav_hide("nav", "ranking")

    if (!is.null(selected_coc$coc) && selected_coc$coc != "") {
      nav_show("nav", "inventory")
      nav_show("nav", "rating_criteria")
      nav_show("nav", "renewal_rating")
      nav_show("nav", "new_rating")
      nav_show("nav", "alternative_rating")
      nav_show("nav", "funding_priorities")
    }
  })
  
  mod_coc_selection_server("coc_selection", nav_control, projects_data, selected_coc)
  mod_inventory_server("inventory", projects_data, selected_coc)
  mod_rating_criteria_server("rating_criteria", selected_coc)
  mod_renewal_rating_server("renewal_rating", projects_data)
  mod_new_rating_server("new_rating", projects_data)
  mod_alternative_rating_server("bulk_rating", projects_data)
  mod_funding_priorities_server("funding_priorities", selected_coc)
  mod_final_review_server("final_review", selected_coc)
  mod_ranking_server("ranking")
  
  # Observer to update nav panel when reactive value changes
  observeEvent(nav_control(), {
    nav_select("nav", selected = nav_control())
  })
}