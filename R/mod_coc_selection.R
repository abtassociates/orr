mod_coc_selection_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Select CoC",
    value = id,
    card(
      card_header("Select your Continuum of Care"),
      selectInput(ns("coc_select"), "CoC Code",
                  choices = c("Please select" = "", sort(unique(hic_data$CoC_Code)))),
      actionButton(ns("next_btn"), "Next", class = "btn-primary")
    )
  )
}

mod_coc_selection_server <- function(id, nav_control, projects_data, selected_coc) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$next_btn, {
      if (input$coc_select != "") {
        selected_coc(input$coc_select)
        nav_control("inventory")
        
        # Initialize projects data
        filtered_data <- hic_data %>%
          fsubset(CoC_Code == input$coc_select) %>%
          fmutate(
            DV_Renewal = NA_character_,
            Grant_Number = NA_character_,
            CoC_Funding_Requested = NA_real_,
            Funding_Action = fifelse(McKinney_Vento == "No", "Ignore", "Renew")
          )
        
        projects_data(filtered_data)
      }
    })
  })
}