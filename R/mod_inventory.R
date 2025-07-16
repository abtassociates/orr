mod_ionventory_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header("Project Review"),
      DTOutput(ns("projects_table")),
      actionButton(ns("add_project"), "Add New Project")
    )
  )
}

mod_ionventory_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    
    projects_data <- reactive({
      req(app_state()$projects)
      app_state()$projects
    })
    
    output$projects_table <- renderDT({
      datatable(
        projects_data(),
        editable = TRUE,
        options = list(
          pageLength = 25,
          scrollX = TRUE
        )
      )
    })
  })
}
