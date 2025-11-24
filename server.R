function(input, output, session) {
  user_coc <- reactiveValues(
    coc = NULL,
    coc_version_id = NULL,
    username = NULL,
    auth = FALSE, # is the user authenticated or not
    given_name = NULL, # user's given_name as stored and returned by cognito
    email = NULL  # user's email as stored and returned by cognito
  )
  nav_control <- reactiveVal("about")

  module_returns <- reactiveValues(customize_rating_criteria = 0)
  
  observeEvent(user_coc$auth, {
    req(user_coc$auth)
    
    # Once user logs in, load the UI + Server functions of the desired modules
    lapply(TABS_AFTER_LOGIN, function(t) {
      print(t)
      # UI
      nav_insert("nav", get(glue::glue("mod_{t}_ui"))(t), select = t == "dashboard")
      
      # Server
      get(glue::glue("mod_{t}_server"))(t, nav_control, user_coc, session, module_returns)
    })
  })
  
  # get the url variables ----
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if(IN_DEV_MODE) {
      login_as_dev(user_coc)
      nav_control("dashboard")
      req(FALSE)
    }
    
    if (!("code" %in% names(query))){
      # no code in the url variables means the user hasn't logged in yet
      print('not logged in yet')
      #showElement("login_welcome_text")
      showElement("login_link")
      showElement("signup_link")
    } else {
      current_user <- retrieve_user_data(query$code)
      
      # if an error occurred during login
      if (is.null(current_user)){
        print('user is NULL')
        #hideElement("login_welcome_text")
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
        hideElement("login_link")
        hideElement("signup_link")
        user_coc$auth <- TRUE
        user_coc$username <- current_user$email
        user_coc$given_name <- current_user$given_name
        nav_control("dashboard")
      }
    }
    
  })
  
  
  # Observer to update nav panel when reactive value changes
  observeEvent(input$nav, {
    if(!(nav_control() == "dashboard" && input$nav == "about"))
      nav_control(input$nav)
  })
  
  observeEvent(nav_control(), {
    nav_select("nav", selected = nav_control())
  })

}
