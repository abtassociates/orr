# UI modules for project rating
mod_threshold_requirements_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Threshold Entry",
    accordion(
      accordion_panel(
        "HUD Requirements",
        uiOutput(ns("hud_requirements"))
      ),
      accordion_panel(
        "CoC Requirements",
        uiOutput(ns("coc_requirements"))
      )
    )
  )
}

mod_threshold_requirements_server <- function(id, user_coc, selected_project, selected_criteria) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
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
      ", .con = DB_CON)) |>
        join(selected_thresholds(), on="selected_threshold_id")
    })
    
    # HUD Requirements UI
    output$hud_requirements <- renderUI({
      req(selected_project)
      
      hud_reqs <- c(
        "Project is eligible under 24 CFR part 578",
        "Project has capacity to meet regulatory requirements",
        "Project quality thresholds are met",
        "Match requirements are met"
      )
      
      lapply(hud_reqs, function(req) {
        div(
          style = "margin-bottom: 15px;",
          radioButtons(
            paste0("hud_req_", make.names(req)),
            req,
            choices = c("Yes" = "yes", "No" = "no"),
            selected = character(0),
            inline = TRUE
          )
        )
      })
    })
    
    # CoC Requirements UI
    output$coc_requirements <- renderUI({
      req(input$rate_project_select, input$select_thresholds_coc_thresholds)
      
      lapply(input$select_thresholds_coc_thresholds, function(req) {
        div(
          style = "margin-bottom: 15px;",
          radioButtons(
            paste0("coc_req_", make.names(req)),
            req,
            choices = c("Yes" = "yes", "No" = "no"),
            selected = character(0),
            inline = TRUE
          )
        )
      })
    })
    
    # Automatically set all HUD requirements to Yes when yes_to_all is checked
    observeEvent(input$yes_to_all_hud, {
      if(input$yes_to_all_hud) {
        for(i in 1:5) {
          updateSelectInput(session,
                            paste0("hud_req_", i),
                            selected = "Yes")
        }
      }
    })
  })
}