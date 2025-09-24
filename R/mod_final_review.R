mod_final_review_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Final Review",
    value = id,
    card(
      card_header("Final Project Review"),
      DTOutput(ns("final_review_table"))
    )
  )
}

mod_final_review_server <- function(id, user_coc) {
  moduleServer(id, function(input, output, session) {
    
    output$final_review_table <- renderDT({
      req(user_coc$coc)

      data <- get_db_query(
        "SELECT p.*, r.rating_score, t.met_threshold 
        FROM projects p 
        LEFT JOIN coc_versions c ON p.coc_version_id = c.coc_version_id
        LEFT JOIN rating_scores r ON p.project_id = r.project_id
        LEFT JOIN threshold_entries r ON p.project_id = t.project_id
        WHERE c.coc = $1", 
        params = list(input$coc_select)
      )
      
      datatable(
        data |>
          fselect(project_name, organization_name, Project_Type,
                 target_population, funding_action, rating_score),
        options = list(
          pageLength = 25,
          style = 'default',
          scrollX = TRUE
        )
      )
    })
  })
}
