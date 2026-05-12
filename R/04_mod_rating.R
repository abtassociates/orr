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

mod_rating_server <- function(id, nav_control, user_coc, parent_session, help_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## restore previous user setting for rating_method from DB
    observeEvent(c(user_coc$coc_version_id, nav_control()), {
      req(nav_control() == "rating")
      
      user_previous_method <- get_user_setting(user_coc, "rating_method")
      
      if(length(user_previous_method) == 0) {
        shinyjs::removeClass(id = "select_in_app", class = "card-selected")
        shinyjs::removeClass(id = "select_alternative", class = "card-selected")
        nav_select(id = "method", selected = ns("none"))
        nav_select(id = "rating_tabs", selected = ns("customize_criteria"))
        
      } else {
        shinyjs::click(id = glue::glue('select_{user_previous_method}'))
      }
      
      req(user_previous_method == "in_app")
      
      # Handle in_app tab selections
      ## Customize vs. Rate Renew vs. Rate New tabs
      user_previous_tab <- get_user_setting(user_coc, "rating_tab")
      user_previous_tab <- if(length(user_previous_tab)) user_previous_tab else NULL
      
      nav_select(id = 'rating_tabs', selected = user_previous_tab)
      
      # if(length(user_previous_tab) > 0){
      #   nav_select(id = 'rating_tabs', selected = user_previous_tab)
      # }
      
      ## Subtab
      user_previous_subtab <- get_user_setting(user_coc, "rating_subtab")
      
      subtab_id <- fcase(
        user_previous_tab == ns('customize_criteria'), 'customize_criteria-rating_criteria_subtabs',
        user_previous_tab == ns('renew'), 'renew-main_contents',
        user_previous_tab == ns('new'), 'new-main_contents',
        default = 'customize_criteria-rating_criteria_subtabs'
      )
      
      nav_select(id = subtab_id, selected = user_previous_subtab)
    }, ignoreInit = TRUE)
    
    ## Rating method
    handle_method_selection <- function(id) {
      nav_select(id = "method", selected = ns(id))
      
      update_user_coc_setting(user_coc, "rating_method", id)
      
      shinyjs::addClass(id = paste0("select_", id), class = "card-selected")
      
      idToRemoveClass <- paste0("select_", ifelse(id == "in_app", "alternative", "in_app"))
      shinyjs::removeClass(id = idToRemoveClass, class = "card-selected")
      
      if(id == "in_app")
        help_id(ns("customize_criteria-coc_thresholds")) # Default to the first sub-tab of in_app
      else
        help_id(ns("alternative"))
    }
    
    observeEvent(input$select_in_app, { handle_method_selection("in_app")}, ignoreInit = TRUE)
    observeEvent(input$select_alternative, { handle_method_selection("alternative")}, ignoreInit = TRUE)
    
    ## Update rating_tab user setting
    observeEvent(input$rating_tabs, {
      req(!is.null(user_coc$coc_version_id) & nav_control() == 'rating')
      
      update_user_coc_setting(user_coc, "rating_tab", input$rating_tabs)

      # Helper slide-in text
      if(input$rating_tabs == ns("customize_criteria"))
        help_id(ns("customize_criteria-coc_thresholds"))
      else
        help_id(paste0(input$rating_tabs, "-thresholds_entry"))
    }, ignoreInit = TRUE)
    
    mod_customize_criteria_server("customize_criteria", user_coc, nav_control, parent_session, help_id)
    mod_in_app_rating_server("renew", user_coc, "Renew", nav_control, help_id)
    mod_in_app_rating_server("new", user_coc, "New", nav_control, help_id)
    mod_rating_summary_server("rating_summary")
    mod_alternative_rating_server("alternative", user_coc, nav_control)
  })
}
