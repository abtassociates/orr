# UI modules for project rating
mod_rating_ui <- function(id) {
  ns <- NS(id)
  
  # Individual Renewal/Expansion Rating
    nav_panel(
      title = "Rating",
      icon = icon("star"),
      value = id,
      # CARD-METHOD SELECTION HERE
      card_body(
        # h5("Choose Your Rating Method"),
        
        tags$style(HTML(glue::glue("
          /* Remove hyperlink styling from actionLink */
          a#{ns('select_in_app')}, a#{ns('select_alternative')} {{
            text-decoration: none;
            color: inherit;
          }}
          .card-selector .card {{
            cursor: pointer;
            transition: transform 0.2s ease-in-out, box-shadow 0.2s ease-in-out;
          }}
          .card-selector .card:hover {{
            transform: translateY(-5px);
            box-shadow: 0 4px 15px rgba(0,0,0,0.1);
          }}
          .card-selector.card-selected .card {{
            border: 2px solid var(--bs-primary);
            box-shadow: 0 4px 15px rgba(0,0,0,0.15);
          }}
        "))),
            
        layout_columns(
          col_widths = c(2, 4, 4, 2),
          # Add a div wrapper with an ID for shinyjs to target
          # The 'card-selected' class makes this the default selection
          div(),
          actionLink(
            ns("select_in_app"),
            label = card(
              full_screen = FALSE,
              card_body(
                class = "text-center",
                h3("In-App"),
                p("Rate projects entirely in-app using customizable criteria")
              )
            ),
            class = "card-selector"
          ),
          
          # Add a div wrapper with an ID for shinyjs to target
          actionLink(
            ns("select_alternative"),
            label = card(
              full_screen = FALSE,
              card_body(
                class = "text-center",
                h3("Alternative"),
                p("Rate projects externally and enter final results in-app")
              )
            ),
            class = "card-selector"
          ),
          div()
        )
      ),
      
      navset_hidden(
        id = ns("method"),
        selected = ns("none"),
        
        nav_panel_hidden(
          value = ns("none"),
          p(
            class = "text-center text-muted",
            "Please select a rating method above to begin."
          )
        ),
        
        # METHOD 1: IN-APP RATING------------
        nav_panel_hidden(
          value = ns("in_app"),
          navset_tab(
            id = ns("rating_tabs"),
            mod_customize_criteria_ui(ns("customize_criteria")),
            mod_in_app_rating_ui(ns("renew"), funding_action = "Renew"),
            mod_in_app_rating_ui(ns("new"), funding_action = "New")
            # footer = card_footer(
            #   style = "display: flex; justify-content: space-between; align-items: center;",
            #   actionButton(ns("save_rating"), paste0("Save Rating"), icon = icon("save"), class = "btn-primary")
            # )
          )
        ),
        
        nav_panel_hidden(
          value = ns("alternative"),
          mod_alternative_rating_ui(ns("alternative"))
        )
      ) # End method selection
    ) #, # End rating panel
    
    # nav_panel(
    #   title = "Rating Summary",
    #   mod_rating_summary_ui(ns("summary"))
    # )
  # )
}

mod_rating_server <- function(id, nav_control, user_coc, parent_session, module_returns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## restore previous user setting for rating_method from DB
    observe({
      req(user_coc$auth)
      req(!is.null(user_coc$coc_version_id) & nav_control() == 'rating')
      
      user_previous_method <- get_db_query(
                                         "SELECT setting_value FROM user_settings WHERE coc_version_id = $1 AND coc_user = $2 AND setting_name = 'rating_method'",
                                         params = list(user_coc$coc_version_id,
                                                       user_coc$username)
      ) |> unlist(use.names = FALSE)
      
      if(length(user_previous_method) > 0){
        shinyjs::click(id = glue::glue('select_{user_previous_method}'))
      }
      
      ## set up subtabs if using in-app rating method
      if(user_previous_method == 'in_app'){
        user_previous_tab <- get_db_query(
                                           "SELECT setting_value FROM user_settings WHERE coc_version_id = $1 AND coc_user = $2 AND setting_name = 'rating_tab'",
                                           params = list(user_coc$coc_version_id,
                                                         user_coc$username)
        )|> unlist(use.names = FALSE)
        if(length(user_previous_tab) > 0){
          
          nav_select(id = 'rating_tabs', selected = glue::glue('rating-{user_previous_tab}'))
        }
      }
      
    })
    
    observeEvent(input$select_in_app, {
      nav_select(id = "method", selected = ns("in_app"))
      user_coc$settings$rating_method <- 'in_app'
      shinyjs::addClass(id = "select_in_app", class = "card-selected")
      shinyjs::removeClass(id = "select_alternative", class = "card-selected")
    })
    
    observeEvent(input$select_alternative, {
      nav_select(id = "method", selected = ns("alternative"))
      user_coc$settings$rating_method <- 'alternative'
      shinyjs::addClass(id = "select_alternative", class = "card-selected")
      shinyjs::removeClass(id = "select_in_app", class = "card-selected")
    })
    
    observeEvent(input$rating_tabs, {
      user_coc$settings$rating_tab <- gsub('rating-', '', input$rating_tabs)
      
    }, ignoreInit = TRUE)
    
    # observe({
    #   req(module_returns$rating_criteria)
    #   
    #   if(length(module_returns$rating_criteria) > 0) {
    #     nav_show("nav", target = "renew_rating", session = parent_session)
    #     nav_show("nav", target = "new_rating", session = parent_session)
    #   } else {
    #     nav_hide("nav", target = "renew_rating", session = parent_session)
    #     nav_hide("nav", target = "new_rating", session = parent_session)
    #   }
    # })
    
    mod_customize_criteria_server("customize_criteria", user_coc, nav_control, parent_session, module_returns)
    mod_in_app_rating_server("renew", user_coc, "Renew", module_returns)
    mod_in_app_rating_server("new", user_coc, "New", module_returns)
    mod_rating_summary_server("rating_summary")
    mod_alternative_rating_server("alternative", user_coc)
  })
}
