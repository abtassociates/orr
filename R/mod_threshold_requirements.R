# UI modules for project rating
mod_threshold_requirements_ui <- function(id) {
  ns <- NS(id)
  
  navset_card_tab(
    nav_panel(
      "Threshold Requirements",
      accordion(
        accordion_panel(
          "HUD Requirements",
          uiOutput(ns("hud_requirements"))
        ),
        accordion_panel(
          "CoC Requirements",
          uiOutput(ns("coc_requirements"))
        )
      ),
      card(
        card_footer(
          div(
            class = "d-grid gap-2",
            actionButton(ns("save_threshold_ratings"), "Save Threshold Ratings", 
                         class = "btn-primary")
          )
        )
      )
    )
  )
}

mod_threshold_requirements_server <- function(id, selected_project) {
  moduleServer(id, function(input, output, session) {
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