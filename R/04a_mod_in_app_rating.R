
mod_in_app_rating_ui <- function(id, funding_action) {
  ns <- NS(id)
  
  ptypes <- ifelse(grepl("renew", id), "Renewal/Expansion Projects", "New Projects")
  
  nav_panel(
    title = paste0("Rate ", ptypes),
    value = id,
    br(),
    helpText(paste0("Rate your ", ptypes, " against your selected criteria.", ifelse(id == "New", "Note that New YHDP projects do not get rated and thus will not appear in the Select Project dropdown below.", ""))),
    br(),
    layout_sidebar(
      style = "min-height: 500px;",
      # the side bar will be 
      sidebar = sidebar(
        width = 250,
        open = "desktop",
        id = ns("project_selection_sidebar"),
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

mod_in_app_rating_server <- function(id, user_coc, funding_action, nav_control) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## restore last selected project from user_settings DB tbl
    ## also update choices
    observe({
      req(!is.null(user_coc$coc_version_id) & nav_control() == 'rating')
      
      user_prev_project_selected <- get_version_setting(get_db_pool(), glue::glue('rating_{id}_project_selected'), user_coc$coc_version_id, user_coc$username)
      
      updateSelectInput(
        session, 
        'project_select', 
        selected = if(fnrow(all_projects()) > 0 && length(user_prev_project_selected)) user_prev_project_selected else "",
        choices = if(fnrow(all_projects()) > 0) c("Please select..." = "", setNames(all_projects()$project_id, all_projects()$project_name)) else character(0)
      )
    })
    
    # Collapse sidebar when user is on Customize Rating Criteria tab, since it's
    # not useful there
    observeEvent(input$main_contents, {
      req(user_coc$coc_version_id & nav_control() == 'rating')
      
      user_coc$version_settings$rating_subtab <- gsub(glue::glue('rating-{id}-'), '', input$main_contents)
      toggle_sidebar(id = "project_selection_sidebar", open = input$main_contents != ns("rating_factors"))
    }, ignoreInit = TRUE)
    
    # Store selected project in user setting
    observeEvent(input$project_select, {
      req(user_coc$coc_version_id & nav_control() == 'rating')
      user_coc$version_settings[[glue::glue('rating_{id}_project_selected')]] <- input$project_select
    }, ignoreInit = TRUE)
    
    # Get all projects for the CoC and the current funding action
    all_projects <- reactive({
      req(user_coc$coc_version_id)
      funding_action_ids <- get_lookup_refid(
        if(funding_action == "Renew") c("Renew","Expand") else "New",
        "funding_action"
      )
      
      get_projects_by_funding_action(user_coc$coc_version_id, funding_action_ids)
    })
    
    # Get the project to be rated from the dropdown in the sidebar
    selected_project <- reactive({
      req(user_coc$coc_version_id)
      
      all_projects() |> 
        fsubset(project_id == input$project_select)
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
    
    # call the module servers of the subtabs
    mod_thresholds_entry_server("thresholds_entry", user_coc, selected_project)
    mod_rating_scores_entry_server("rating_scores_entry", user_coc, selected_project)
  })
}
