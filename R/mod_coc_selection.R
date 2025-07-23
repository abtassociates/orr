mod_coc_selection_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Select CoC",
    value = id,
    card(
      card_header("Select your Continuum of Care"),
      selectInput(ns("coc_select"), "CoC Code",
                  choices = c("Please select" = "", get_db_query("SELECT DISTINCT hudnum FROM all_hic_data ORDER BY hudnum")$hudnum)),
      actionButton(ns("next_btn"), "Next", class = "btn-primary")
    )
  )
}

mod_coc_selection_server <- function(id, nav_control, selected_coc, projects_data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$next_btn, {
      if (input$coc_select != "") {
        nav_control("inventory")
        
        data <- get_db_query(
          "SELECT p.*, c.coc FROM projects p 
             LEFT JOIN coc_instances c ON p.coc_instance_id = c.coc_instance_id 
             WHERE c.coc = $1",
          params = list(input$coc_select)
        )
        req(nrow(data) > 0)
        
        # use factors to ensure dropdowns
        user_columns <- c("dv_renewal", "grant_number", "coc_amount_awarded_last_year", "coc_amount_expended_last_year", "coc_funding_requested", "funding_action")
        data <- data %>%
          fselect(-coc_instance_id) %>%
          ftransformv(c(user_columns, "project_type", "target_population", "mckinneyvento"), forcats::as_factor)
        
        selected_coc(input$coc_select)
        projects_data(data)
      }
    })
  })
}