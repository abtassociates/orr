function(input, output, session) {
  projects_data <- reactiveVal(NULL)
  user_coc <- reactiveValues(
    coc = NULL,
    coc_instance_id = NULL,
    username = NULL,
    auth = FALSE, # is the user authenticated or not
    given_name = NULL, # user's given_name as stored and returned by cognito
    email = NULL  # user's email as stored and returned by cognito
  )
  nav_control <- reactiveVal("dashboard")

  # Hide all panels except "account" initially, show login modal
  observe({
    showModal(
      list(
           mod_cognito_ui("cognito"),
           ## adjust background color of blurred application behind modal
           tags$script("$('.modal-backdrop').css('background-color', '#777777')")
           ),
              session = session)
    #nav_hide("nav", "coc_selection")
    nav_hide("nav", "inventory")
    nav_hide('nav', 'dashboard')
    nav_hide("nav", "rating_criteria")
    nav_hide("nav", "renewal_rating")
    nav_hide("nav", "new_rating")
    nav_hide("nav", "alternative_rating")
    nav_hide("nav", "funding_priorities")
    nav_hide("nav", "ranking")
    nav_hide("nav", "final_review")

  })

  mod_coc_selection_server("coc_selection", nav_control, projects_data, user_coc)
  mod_inventory_server("inventory", user_coc)
  mod_rating_criteria_server("rating_criteria", user_coc)
  mod_renewal_rating_server("renewal_rating", projects_data)
  mod_new_rating_server("new_rating", projects_data)
  mod_alternative_rating_server("bulk_rating", projects_data)
  mod_funding_priorities_server("funding_priorities", user_coc)
  mod_final_review_server("final_review", user_coc)
  mod_ranking_server("ranking")
  mod_requests_server(id="requests", user_coc)
  
  # user <- reactiveValues(auth = FALSE, # is the user authenticated or not
  #                        given_name = NULL, # user's given_name as stored and returned by cognito
  #                        email = NULL)  # user's email as stored and returned by cognito
  
  # get the url variables ----
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!("code" %in% names(query))){
      # no code in the url variables means the user hasn't logged in yet
      print('not logged in yet')
      showElement("login_welcome_text")
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
            # user not allowed, so shows a login error message
            #showElement("login_error_user")
            
        } else {
          print("user in allowed list")
          
        }
        
          hideElement("login_welcome_text")
          showElement("login_confirmed")
          showElement("enter_app")
          user_coc$auth <- TRUE
          user_coc$email <- current_user$email
          user_coc$username <- current_user$email
          user_coc$given_name <- current_user$given_name
         
          nav_show("nav", "dashboard")
          nav_show("nav", "coc_selection")
          nav_show("nav", "inventory")
          nav_show("nav", "rating_criteria")
          nav_show("nav", "renewal_rating")
          nav_show("nav", "new_rating")
          nav_show("nav", "alternative_rating")
          nav_show("nav", "funding_priorities")
          nav_show("nav", "final_review")
          
      }
    }
    
  })
  
  observeEvent(input$enter_app, {
    removeModal()
  })
  
  output$confirmed_login_name <-
    renderText({
     
      ifelse(!is.null(user_coc$given_name),
             paste0("Welcome, ",user_coc$given_name),
             paste0("Welcome, ",user_coc$email)
         )
      
    })
  
  
  # Observer to update nav panel when reactive value changes
  observeEvent(nav_control(), {
    nav_select("nav", selected = nav_control())
  })

}
