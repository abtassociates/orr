
mod_in_app_rating_ui <- function(id, funding_action) {
  ns <- NS(id)
  
  ptypes <- ifelse(grepl("renew", id), "Renewal/Expansion Projects", "New Projects")
  
  nav_panel(
    title = paste0("Rate ", ptypes),
    value = id,
    br(),
    helpText(paste0("Rate your ", ptypes, " against your selected criteria.")),
    br(),
    layout_sidebar(
      style = "min-height: 500px; overflow: visible !important;",
      # the side bar will be 
      sidebar = sidebar(
        width = 250,
        open = "desktop",
        id = ns("project_selection_sidebar"),
        uiOutput(ns("project_select_css")),
        selectInput(ns("project_select"), label = "Select Project", choices = NULL),
        uiOutput(ns("project_info_sidebar"))
      ),
      navset_tab(
        id = ns("main_contents"),
        mod_thresholds_entry_ui(ns("thresholds_entry")),
        mod_rating_scores_entry_ui(ns("rating_scores_entry")),
      )
    )
  )
}

mod_in_app_rating_server <- function(id, user_coc, funding_action, nav_control, help_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive to query the database and return IDs of projects that have already been rated
    rated_projects <- reactive({
      req(user_coc$coc_version_id, nav_control() == 'rating')
      user_coc$projects_updated # Trigger a refresh if projects/ratings are updated
      
      projects <- all_projects()
      req(fnrow(projects) > 0)
      
      # Query DB for all projects that have a weighted_score
      evaluated <- get_project_rating_completion(projects$project_id)
      
      if (is.null(evaluated) || fnrow(evaluated) == 0) return(character(0))
      
      # Return only rated projects that belong to the current list
      evaluated$project_id
    })
    
    # Dynamically generate CSS to set the background color of completed options in the dropdown
    output$project_select_css <- renderUI({
      rated <- rated_projects()
      req(length(rated) > 0)
      
      input_id <- ns("project_select")
      
      # Build CSS selectors targeting the specific project options in the selectize dropdown
      # Also target raw <option> tags as a fallback
      css_selectors <- c(
        paste0("#", input_id, " option[value='", rated, "']"),
        paste0("#", input_id, " ~ .selectize-control .selectize-dropdown-content .option[data-value='", rated, "']")
      )
      
      # Inject custom background color (a Bootstrap-like success green)
      css_rule <- paste(
        paste(css_selectors, collapse = ",\n"),
        "{ background-color: #4BB543 !important; color: black !important;}"
      )
      
      tags$style(HTML(css_rule))
    })
    
    
    ## restore last selected project from user_settings DB tbl
    ## also update choices
    observeEvent(nav_control(), {
      req(nav_control() == 'rating')
      req(all_projects())
      
      user_prev_project_selected <- get_user_setting(user_coc, 'project_selected') |> as.integer()
      
      projects <- all_projects()
      
      if (fnrow(projects) > 0) {
        rated <- rated_projects()
        
        # Sort projects so that unrated (FALSE/0) come first, and rated (TRUE/1) go to the bottom.
        # seq_len(nrow(projects)) acts as a tie-breaker so original relative sorting is preserved.
        is_rated <- projects$project_id %in% rated
        sort_order <- order(is_rated, tolower(projects$project_name))
        projects <- projects[sort_order, ]
        
        # Update our flag based on the newly sorted dataframe
        is_rated_sorted <- projects$project_id %in% rated
        
        # Prepend a checkmark to the names of the completed projects
        choices_labels <- projects$project_name
        
        if (any(is_rated_sorted)) {
          choices_labels[is_rated_sorted] <- paste0("✔ ", choices_labels[is_rated_sorted])
        }
        
        choices_list <- c("Please select..." = "", setNames(projects$project_id, choices_labels))
        selected_val <- if(length(user_prev_project_selected) && user_prev_project_selected %in% projects$project_id) user_prev_project_selected else ""
      } else {
        choices_list <- character(0)
        selected_val <- ""
      }
      
      updateSelectInput(
        session, 
        'project_select', 
        selected = selected_val,
        choices = choices_list
      )
    })
    
    # Collapse sidebar when user is on Customize Rating Criteria tab, since it's
    # not useful there
    observeEvent(input$main_contents, {
      req(user_coc$coc_version_id & nav_control() == 'rating')
      
      update_user_coc_setting(user_coc, "rating_subtab", input$main_contents)
      
      toggle_sidebar(id = "project_selection_sidebar", open = input$main_contents != ns("rating_factors"))
      
      help_id(input$main_contents)
    }, ignoreInit = TRUE)
    
    # Store selected project in user setting
    observeEvent(input$project_select, {
      req(user_coc$coc_version_id & nav_control() == 'rating')
      update_user_coc_setting(user_coc, "project_selected", input$project_select)
    }, ignoreInit = TRUE)
    
    # Get all projects for the CoC and the current funding action
    all_projects <- reactive({
      req(user_coc$coc_version_id, user_coc$projects_updated)
      funding_action_ids <- get_lookup_refid(
        if(funding_action == "Renew") c("Renew","Expand") else "New",
        "funding_action"
      )
      
      get_projects_by_funding_action(user_coc$coc_version_id, funding_action_ids)
    })
    
    # Get the project to be rated from the dropdown in the sidebar
    selected_project <- reactive({
      req(user_coc$coc_version_id)
      
      if(input$project_select != "")
        all_projects() |> 
          fsubset(project_id == input$project_select)
      else
        data.table()
    })
    
    # Show project info about the selected project
    output$project_info_sidebar <- renderUI({
      req(selected_project())
      req(fnrow(selected_project()) > 0)
      
      div(
        p(strong("Organization:"), selected_project()$organization_name),
        p(strong("Project Type:"), get_lookup_label(selected_project()$project_type, "project_type")),
        p(strong("Target Population:"), get_lookup_label(selected_project()$target_population, "target_population")),
        p(strong("Grant Amount:"),
          formatC(selected_project()$coc_funding_requested, format="f",
                  digits=2, big.mark=","))
      )
    })
    
    # 1. Define a reactive for each sub-tab's visibility
    # It's active IF the main navbar is on 'rating' AND this specific nested tab is selected
    is_thresholds_active <- reactive({
      nav_control() == "rating" && input$main_contents == ns("thresholds_entry")
    })
    
    is_scores_active <- reactive({
      nav_control() == "rating" && input$main_contents == ns("rating_scores_entry")
    })
    
    # call the module servers of the subtabs
    mod_thresholds_entry_server("thresholds_entry", user_coc, selected_project, active = is_thresholds_active, funding_action, hasProjects = reactive(isTruthy(fnrow(all_projects()) > 0)))
    mod_rating_scores_entry_server("rating_scores_entry", user_coc, selected_project, funding_action, active = is_scores_active, hasProjects = reactive(isTruthy(fnrow(all_projects()) > 0)))
  })
}
