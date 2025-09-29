# UI modules for new project rating
mod_new_rating_ui <- function(id) {
  ns <- NS(id)
  
  # Individual New Project Rating
  nav_panel(
    "Rate New Projects",
    value = "new_rating",
    div(
      style = "padding: 15px; background-color: #f8f9fa; border-radius: 5px; margin-bottom: 15px;",
      div(
        style = "display: flex; align-items: center; margin-bottom: 10px;",
        div(
          style = "width: 120px;",
          tags$label("Project Name:", class = "control-label")
        ),
        div(
          style = "flex-grow: 1;",
          selectInput(ns("rate_new_project_select"), NULL, choices = NULL)
        )
      ),
      uiOutput("new_project_info_sidebar")
    ),
    navset_card_tab(
      nav_panel(
        "Threshold Requirements",
        accordion(
          accordion_panel(
            "HUD Requirements",
            uiOutput("new_hud_requirements")
          ),
          accordion_panel(
            "CoC Requirements",
            uiOutput("new_coc_requirements")
          )
        ),
        card(
          card_footer(
            div(
              class = "d-grid gap-2",
              actionButton("save_new_threshold_ratings", "Save Threshold Ratings", 
                           class = "btn-primary")
            )
          )
        )
      ),
      nav_panel(
        "Rating Factors",
        uiOutput("new_project_rating_factors")
      ),
      nav_panel(
        "Summary",
        uiOutput("new_rating_summary")
      )
    )
  )
}

mod_new_rating_server <- function(id, projects_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # New project info sidebar
    output$new_project_info_sidebar <- renderUI({
      req(projects_data())
      req(input$rate_new_project_select)
      project_data <- projects_data() |>
        fsubset(project_name == input$rate_new_project_select)
      
      div(
        p(strong("Organization:"), project_data$organization_name),
        p(strong("Project Type:"), project_data$Project_Type),
        p(strong("Target Population:"), project_data$Target_Population),
        p(strong("Requested Amount:"), 
          formatC(project_data$CoC_Funding_Requested, format="f", 
                  digits=2, big.mark=","))
      )
    })
    
    
    # Update organization filter choices when CoC is selected
    observe({
      req(projects_data())

      orgs <- unique(projects_data()$organization_name)
      updateSelectInput(session, "filter_org",
                        choices = c("All", sort(orgs)))
      
      new_projects <- projects_data()[funding_action == "New"]
      
      updateSelectInput(session, "rate_new_project_select",
                        choices = c("Select a project" = "", sort(new_projects$project_name)))
    })
  })
}
