function(input, output, session) {
  projects_data <- reactiveVal(NULL)
  user_coc <- reactiveValues(
    coc = NULL,
    coc_version_id = NULL,
    username = NULL,
    auth = FALSE, # is the user authenticated or not
    given_name = NULL, # user's given_name as stored and returned by cognito
    email = NULL  # user's email as stored and returned by cognito
  )
  nav_control <- reactiveVal("dashboard")

  toggle_tabs <- function() {
    for(tab in TABS) {
      if(tab %in% TABS_TO_SHOW) nav_show("nav", tab)
      else nav_hide("nav", tab)
    }
  }
  # Hide all panels except "account" initially, show login modal
  observe({
    showModal(
      list(
           mod_cognito_ui("cognito"),
           ## adjust background color of blurred application behind modal
           tags$script("$('.modal-backdrop').css('background-color', '#777777')")
           ),
              session = session)

    toggle_tabs()
  })

  mod_coc_selection_server("coc_selection", nav_control, projects_data, user_coc)
  mod_inventory_server("inventory", user_coc)
  mod_rating_criteria_server("rating_criteria", user_coc)
  mod_renewal_rating_server("renewal_rating", projects_data)
  mod_new_rating_server("new_rating", projects_data)
  mod_alternative_rating_server("alternative_rating", projects_data)
  mod_funding_priorities_server("funding_priorities", user_coc)
  mod_final_review_server("final_review", user_coc)
  mod_ranking_server("ranking")
  mod_requests_server(id="requests", user_coc)
  
  # get the url variables ----
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!("code" %in% names(query))){
      # no code in the url variables means the user hasn't logged in yet
      print('not logged in yet')
      showElement("login_welcome_text")
      showElement("login_link")
      showElement("signup_link")
    } else {
      current_user <- retrieve_user_data(query$code)
      
      # if an error occurred during login
      if (is.null(current_user)){
        print('user is NULL')
        hideElement("login_welcome_text")
        user_coc$auth <- FALSE
      } else {
        print('user found!')
        # check if user is in allowed user list
        if (!(str_to_lower(current_user$email) %in% users$username)){
            print("new user added to allowed list")
            
        } else {
          print("user in allowed list")
          
        }
        
          removeModal()
          
          user_coc$auth <- TRUE
          user_coc$email <- current_user$email
          user_coc$username <- current_user$email
          user_coc$given_name <- current_user$given_name
         
          toggle_tabs()
      }
    }
    
  })
  
  
  # Observer to update nav panel when reactive value changes
  observeEvent(nav_control(), {
    nav_select("nav", selected = nav_control())
  })

}
