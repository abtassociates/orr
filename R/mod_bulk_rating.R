mod_bulk_rating_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header("Bulk Project Rating"),
      DTOutput(ns("bulk_rating_table")),
      actionButton(ns("save_bulk_rating"), "Save All Ratings")
    )
  )
}

mod_bulk_rating_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    
    output$bulk_rating_table <- renderDT({
      req(app_state()$projects)
      
      projects <- app_state()$projects %>%
        filter(Funding_Action %in% c("Renew", "Expand", "New"))
      
      dt <- datatable(
        data.frame(
          Project_Name = projects$Project_Name,
          Project_Type = projects$Project_Type,
          HUD_Threshold = "",
          CoC_Threshold = "",
          Rating_Score = NA
        ),
        editable = list(target = "cell", disable = list(columns = c(1, 2))),
        options = list(pageLength = 25)
      )
      
      dt
    })
  })
}