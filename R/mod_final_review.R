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

mod_final_review_server <- function(id, selected_coc) {
  moduleServer(id, function(input, output, session) {
    
    output$final_review_table <- renderDT({
      req(selected_coc())
      sql_query <- "
        SELECT p.*, r.rating_score, t.met_threshold 
        FROM projects p 
        LEFT JOIN coc_instances c ON p.coc_instance_id = c.coc_instance_id
        LEFT JOIN rating_scores r ON p.project_id = r.project_id
        LEFT JOIN threshold_entries r ON p.project_id = t.project_id
        WHERE c.coc = $1"
      data <- get_db_query(sql_query, params = list(input$coc_select))
      
      datatable(
        data |>
          fselect(project_name, organization_name, Project_Type,
                 target_population, funding_action, rating_score),
        options = list(
          pageLength = 25,
          scrollX = TRUE
        )
      )
    })
  })
}
