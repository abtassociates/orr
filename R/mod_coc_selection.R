mod_coc_selection_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header("Select your CoC"),
      selectInput(ns("coc_select"), "Choose your CoC:",
                 choices = NULL),
      actionButton(ns("confirm_coc"), "Continue")
    )
  )
}

mod_coc_selection_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      updateSelectInput(session, "coc_select",
                       choices = unique(test_hic$CoC_Code))
    })
    
    observeEvent(input$confirm_coc, {
      current_state <- app_state()
      current_state$coc_selected <- TRUE
      current_state$selected_coc <- input$coc_select
      current_state$projects <- test_hic %>%
        filter(CoC_Code == input$coc_select)
      app_state(current_state)
    })
  })
}