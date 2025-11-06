# UI modules for project rating
mod_rating_summary_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("rating_summary"))
}

mod_rating_summary_server <- function(id, rating_scores) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Rating Summary UI
    output$rating_summary <- renderUI({
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