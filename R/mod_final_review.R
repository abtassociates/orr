mod_final_review_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header("Final Project Review"),
      DTOutput(ns("final_review_table"))
    )
  )
}

mod_final_review_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    
    output$final_review_table <- renderDT({
      req(app_state()$projects)
      
      datatable(
        app_state()$projects |>
          select(Project_Name, Organization_Name, Project_Type,
                 Target_Population, Funding_Action, Rating_Score),
        options = list(
          pageLength = 25,
          scrollX = TRUE
        )
      )
    })
  })
}
