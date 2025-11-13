
mod_in_app_rating_ui <- function(id, funding_action) {
  ns <- NS(id)
  
  ptypes <- ifelse(grepl("renew", id), "Renewal/Expansion Projects", "New Projects")
  
  nav_panel(
    title = paste0("Rate ", ptypes),
    value = id,
    br(),
    em(paste0("Rate your ", ptypes, " against your selected criteria")),
    layout_sidebar(
      # the side bar will be 
      sidebar = sidebar(
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

mod_in_app_rating_server <- function(id, user_coc, funding_action, module_returns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Collapse sidebar when user is on Customize Rating Criteria tab, since it's
    # not useful there
    observeEvent(input$main_contents, {
      req(user_coc$coc_version_id)
      toggle_sidebar(id = "project_selection_sidebar", open = input$main_contents != ns("rating_factors"))
    }, ignoreInit = TRUE)
    
    # Get all projects for the CoC and the current funding action
    all_projects <- reactive({
      funding_action_ids <- get_lookup_refid(
        ifelse(funding_action == "Renew", c("Renew","Expand"), "New"),
        "funding_action"
      )
      
      get_db_query(glue::glue_sql(
        "SELECT project_id, organization_name, project_name, project_type, target_population
        FROM projects 
        WHERE coc_version_id = {user_coc$coc_version_id} AND funding_action IN ({funding_action_ids*})",
        .con=DB_CON
      ))
    })
    
    # Get the project to be rated from the dropdown in the sidebar
    selected_project <- reactive({
      req(input$project_select)
      all_projects() |> 
        fsubset(project_id == input$project_select)
    })
    
    
    # Update project selection choices when CoC is selected
    observe({
      req(user_coc$coc_version_id)
      req(nrow(all_projects()) > 0)
      
      updateSelectInput(
        session, 
        "project_select",
        choices = setNames(all_projects()$project_id, all_projects()$project_name)
      )
    })
    
    # Show project info about the selected project
    output$project_info_sidebar <- renderUI({
      req(input$project_select)
      
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
    mod_thresholds_entry_server("thresholds_entry", user_coc, input$project_select, module_returns)
    mod_rating_scores_entry_server("rating_scores", user_coc, selected_project, funding_action, module_returns)
  })
}
