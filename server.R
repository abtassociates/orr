function(input, output, session) {
  logger::log_shiny_input_changes(input, excluded_inputs = inputs_to_exclude)
  
  user_coc <- reactiveValues(
    coc = NULL,
    coc_version_id = NULL,
    username = NULL,
    auth = FALSE, # is the user authenticated or not
    given_name = NULL, # user's given_name as stored and returned by cognito
    email = NULL,  # user's email as stored and returned by cognito
    active_tab = NULL, # last active tab
    settings = NULL, # user settings (cols hidden from inventory, rating method, etc.)
    
    requests_updated = 0,
    projects_updated = 0,
    
    # Incrementing trigger for refreshing other views
    customized_rating_factors_updated_Renew = 0,
    customized_rating_factors_updated_New = 0,
    customized_coc_thresholds_updated = 0,
    priorities_and_ceilings_updated = 0,
    
    rating_updated = 0,
    ranking_updated = 0,
    coc_status = NULL,
    coc_status_updated = 0
  )
  nav_control <- reactiveVal("about")
  
  observeEvent(user_coc$auth, {
    req(user_coc$auth)
    
    # Once user logs in, load the UI + Server functions of the desired modules
    lapply(TABS_AFTER_LOGIN, function(t) {
      print(t)
      # UI
      nav_insert("nav", get(glue::glue("mod_{t}_ui"))(t), select = t == "dashboard")
      
      # Server
      get(glue::glue("mod_{t}_server"))(t, nav_control, user_coc, session, help_id = help_id)
    })
  })
  
  # get the url variables ----
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if(IN_DEV_MODE) {
      login_as_user(user_coc, get0("DEV_USER_LOGIN", envir=.GlobalEnv))
      nav_control("dashboard")
      req(FALSE)
    }
    
    if (!("code" %in% names(query))){
      # no code in the url variables means the user hasn't logged in yet
      message('not logged in yet')
      #showElement("login_welcome_text")
      showElement("login_link")
      showElement("signup_link")
    } else {
      current_user <- retrieve_user_data(query$code)
      
      # if an error occurred during login
      if (is.null(current_user)){
        message('user is NULL')
        
        # 2. CLEAR COOKIE: Remove the cookie so we don't get stuck in an auto-redirect loop
        shinyjs::runjs("clear_cookie();")
        
        #hideElement("login_welcome_text")
        user_coc$auth <- FALSE
      } else {
        message('user found!')
        
        # 3. SET COOKIE: Set a browser cookie to expire in 30 minutes
        shinyjs::runjs("
          let expires = new Date();
          expires.setTime(expires.getTime() + (60 * 60 * 1000)); // 60 minutes, to match Cognito
          document.cookie = 'cognito_session=active;expires=' + expires.toUTCString() + ';path=/';
        ")
        
        # check if user is in allowed user list
        if (!(str_to_lower(current_user$email) %in% str_to_lower(get_db_tbl('users')$username))){
          message("new user added to allowed list")
            
            new_user_df <- data.frame(
              username = current_user$email, 
              firstname = current_user$given_name,
              lastname = current_user$family_name,
              created_by = SERVICE_ACCOUNT, 
              updated_by = SERVICE_ACCOUNT
            )
            db_append('users', new_user_df)
        } else {
          message("user in allowed list")
          
        }
        
        removeModal()
        hideElement("login_link")
        hideElement("signup_link")
        user_coc$auth <- TRUE
        user_coc$username <- current_user$email
        user_coc$given_name <- current_user$given_name
        

        # 4. CLEAN THE URL: Strip the single-use ?code=... from the URL. 
        # If the user refreshes, they will hit the clean URL, our cookie logic 
        # will catch them, and seamlessly fetch a new code from Cognito.
        updateQueryString(session$clientData$url_pathname, mode = "replace", session = session)
        
        nav_control("dashboard")
        session$sendCustomMessage("auth_state", TRUE)
      }
    }
    
  })
  
  
  # Observer to update nav panel when reactive value changes
  observeEvent(input$nav, {
    if(!(nav_control() == "dashboard" && input$nav == "about"))
      nav_control(input$nav)
    
    db_execute("
      DELETE FROM user_presence WHERE session_id = $1", 
      params = list(session$token)
    )
    
    if(!is.null(user_coc$coc_version_id))
      update_user_coc_setting(user_coc, "active_tab", input$nav)
  })
  
  observeEvent(nav_control(), {
    nav_select("nav", selected = nav_control())
  })
  
  # Run once on app startup
  clean_presence_table()

  # --- Slide-In Sidebar ----
  help_id <- mod_slide_in_instructions_server("instructions", user_coc, nav_control)
  
  session$onSessionEnded(function() {
    p <- get_db_pool()
    if(!p$valid) {
      message("onSessionEnded, but no pool available so returning now.")
      return(NULL)
    }
    
    message("onSessionEnded, deleting from user presence.")
    db_execute(
      "DELETE FROM user_presence WHERE session_id = $1;",
      params = list(session$token)
    )
  })
}
