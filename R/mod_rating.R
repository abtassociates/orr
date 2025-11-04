# UI modules for project rating
mod_rating_ui <- function(id) {
  ns <- NS(id)
  
  # Individual Renewal/Expansion Rating
  nav_menu(
    title = "Rate Projects",
    icon = icon("star"),
    value = id,
    nav_item(
      value = ns("renewal_rating"),
      mod_project_rating_ui(ns("renewal_rating"), funding_action = "Renew")
    ),
    nav_item(
      value = ns("new_rating"),
      mod_project_rating_ui(ns("new_rating"), funding_action = "New")
    )
  )
}

mod_rating_server <- function(id, nav_control, user_coc, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # req(user_coc$coc_version_id)
    # mod_project_rating_server("renew_rating", user_coc, "Renew")
    # mod_project_rating_server("new_rating", user_coc, "New")
  })
}

mod_project_rating_ui <- function(id, funding_action) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = sidebar(
      id = ns("project_selection_sidebar"),
      selectInput(ns("project_select"), label = "Select Project", choices = NULL),
      uiOutput(ns("project_info_sidebar"))
    ),
    navset_tab(
      id = ns("main_contents"),
      nav_panel("Threshold Requirements", mod_threshold_requirements_ui(ns("threshold_requirements"))),
      nav_panel("Rating Factors", uiOutput(ns("project_rating_factors"))),
      nav_panel("Summary", uiOutput(ns("renewal_rating_summary")))
    )
  )
}

mod_project_rating_server <- function(id, user_coc, funding_action) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    req(user_coc$coc_version_id)
    
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
    
    selected_project <- reactive({
      all_projects() |> fsubset(project_name == input$project_select)
    })
    
    selected_thresholds <- reactive({
      req(user_coc$coc_version_id)
      
      get_db_query(glue::glue_sql(
        "SELECT st.selected_threshold_id, st.type, t.threshold_text
        FROM thresholds
        FULL JOIN selected_thresholds st ON st.threshold_id = t.threshold_id
        WHERE st.coc_version_id = {user_coc$coc_version_id} OR t.type = 'HUD'
      "))
    })
    
    threshold_entries <- reactive({
      req(user_coc$coc_version_id)
      req(input$project_select)
      
      e <- get_db_query(glue::glue_sql(
        "SELECT selected_threshold_id, met_threshold
        FROM threshold_entries te
        INNER JOIN selected_thresholds st ON st.selected_threshold_id = te.selected_threshold_id
        WHERE te.project_id = {input$project_select}
      ")) |>
        join(selected_thresholds(), on="selected_threshold_id")
    })
    
    selected_factors <- reactive({
      req(user_coc$coc_version_id)
      
      get_db_query(glue::glue_sql(
        "SELECT r.*
        FROM rating_factors r
        INNER JOIN selected_rating_factors sr ON sr.rating_factor_id = r.rating_factor_id
        WHERE coc_version_id = {user_coc$coc_version_id} AND funding_action = {funding_action}
      "))
    })
    
    rating_scores <- reactive({
      req(user_coc$coc_version_id)
      req(input$project_select)
      
      factors_for_project <- selected_factors() |>
        fsubset(
          (project_type == selected_project()$project_type | is.na(project_type)) &
          (target_population == selected_project()$target_population | is.na(target_population))
        )
      
      get_db_query(glue::glue_sql(
        "SELECT *
        FROM rating_scores
        WHERE sr.project_id = {input$project_select}
      ")) |>
        join(factors_for_project(), on="selected_rating_factor_id")()
    })
    
    # Update project selection choices when CoC is selected
    observe({
      req(user_coc$coc_version_id)
      req(nrow(all_projects()) > 0)
      
      updateSelectInput(session, "project_select",
                       choices = setNames(all_projects()$project_id, all_projects()$project_name))
      
      updateSelectInput(session, "filter_org",
                        choices = c("All", all_projects()$organization_name |> funique() |> sort()))
    })
    
    # Project info sidebar
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

    mod_threshold_requirements_server("threshold_requirements", input$project_select)
    mod_rating_scores_server("rating_scores", rating_scores)
    
    
    
    # Rating Summary UI
    output$renewal_rating_summary <- renderUI({
      req(input$project_select)
      
      card(
        card_header("Rating Summary"),
        div(
          style = "padding: 15px;",
          h4("Threshold Requirements"),
          tags$ul(
            tags$li(
              "HUD Requirements: ",
              span(style = "color: green;", "Pass")
            ),
            tags$li(
              "CoC Requirements: ",
              span(style = "color: red;", "Fail")
            )
          ),
          h4("Rating Scores"),
          tags$ul(
            tags$li("Performance Measures: 75/100"),
            tags$li("Serve High Needs: 85/100")
          ),
          h3("Total Score: 160/200")
        )
      )
    })
  })
}
