function(input, output, session) {
  projects_data <- reactiveVal(NULL)
  selected_coc <- reactiveValues(
    coc = NULL,
    coc_instance_id = NULL
  )
  nav_control <- reactiveVal("account")
  username <- reactiveVal("alex.silverman@abtglobal.com")
  
  # Hide all panels except "account" initially, show login modal
  observe({
    showModal(
      list(
           mod_cognito_ui("cognito"),
           ## adjust background color of blurred application behind modal
           tags$script("$('.modal-backdrop').css('background-color', '#777777')")
           ),
              session = session)
    nav_hide("nav", "coc_selection")
    nav_hide("nav", "inventory")
    nav_hide("nav", "rating_criteria")
    nav_hide("nav", "renewal_rating")
    nav_hide("nav", "new_rating")
    nav_hide("nav", "alternative_rating")
    nav_hide("nav", "funding_priorities")
    nav_hide("nav", "ranking")
    nav_hide("nav", "final_review")

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
  
  user <- reactiveValues(auth = FALSE, # is the user authenticated or not
                         given_name = NULL, # user's given_name as stored and returned by cognito
                         email = NULL)  # user's email as stored and returned by cognito
  
  # get the url variables ----
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!("code" %in% names(query))){
      # no code in the url variables means the user hasn't logged in yet
      print('not logged in yet')
      showElement("login")
    } else {
      current_user <- retrieve_user_data(query$code)
      
      # if an error occurred during login
      if (is.null(current_user)){
        print('user is NULL')
        hideElement("login")
        showElement("login_error_aws_flow")
        user$auth <- FALSE
      } else {
        print('user found!')
        # check if user is in allowed user list
        if (str_to_lower(current_user$email) %in% str_to_lower(allowed_users$user_email)){
          print("user in allowed list")
          
          hideElement("login")
          showElement("login_confirmed")
          showElement("enter_app")

          user$auth <- TRUE
          user$email <- current_user$email
          user$given_name <- current_user$given_name
          
          nav_show("nav", "coc_selection")
          nav_show("nav", "inventory")
          nav_show("nav", "rating_criteria")
          nav_show("nav", "renewal_rating")
          nav_show("nav", "new_rating")
          nav_show("nav", "alternative_rating")
          nav_show("nav", "funding_priorities")
          nav_show("nav", "final_review")
          
        } else {
          print("user not allowed")
          # user not allowed, so shows a login error message
          hideElement("login")
          showElement("login_error_user")

        }
      }
    }
    
  })
  
  observeEvent(input$enter_app, {
    removeModal()
  })
  
  output$confirmed_login_name <-
    renderText({
     
      ifelse(!is.null(user$given_name),
             paste0("Welcome, ",user$given_name),
             paste0("Welcome, ",user$email)
         )
      
    })
  
  # Observer to update nav panel when reactive value changes
  observeEvent(nav_control(), {
    nav_select("nav", selected = nav_control())
  })
  
 
}