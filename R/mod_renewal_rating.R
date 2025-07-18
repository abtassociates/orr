# UI modules for project rating
mod_renewal_rating_ui <- function(id) {
  ns <- NS(id)
  
  # Individual Renewal/Expansion Rating
  nav_panel(
    "Rate Renewal Projects",
    value = "renewal_rating",
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
          selectInput(ns("project_select"), NULL, choices = NULL)
        )
      ),
      uiOutput("project_info_sidebar")
    ),
    
    mod_threshold_requirements_ui(ns("threshold_requirements")),
    
    nav_panel(
      "Rating Factors",
      uiOutput(ns("project_rating_factors"))
    ),
    nav_panel(
      "Summary",
      uiOutput(ns("renewal_rating_summary"))
    )
  )
}

mod_renewal_rating_server <- function(id, projects_data) {
  moduleServer(id, function(input, output, session) {
    # Update project selection choices when CoC is selected
    observe({
      req(projects_data())
      renewal_projects <- projects_data()[Funding_Action %in% c("Renew", "Expand")]
      
      updateSelectInput(session, "project_select",
                       choices = renewal_projects$Project_Name)
      
      orgs <- unique(projects_data()$Organization_Name)
      updateSelectInput(session, "filter_org",
                        choices = c("All", sort(orgs)))
    })
    
    # Project info sidebar
    output$project_info_sidebar <- renderUI({
      req(input$project_select)
      project_data <- projects_data() |>
        fsubset(Project_Name == input$project_select)
      
      div(
        p(strong("Organization:"), project_data$Organization_Name),
        p(strong("Project Type:"), project_data$Project_Type),
        p(strong("Target Population:"), project_data$Target_Population),
        p(strong("Grant Amount:"), 
          formatC(project_data$CoC_Funding_Requested, format="f", 
                  digits=2, big.mark=","))
      )
    })

    mod_threshold_requirements_server("threshold_requirements", input$project_select)
    
    # Project Rating Factors UI
    output$project_rating_factors <- renderUI({
      req(input$project_select)
      project_data <- projects_data() |>
        fsubset(Project_Name == input$project_select)
      
      # Get applicable rating factors based on project type and population
      # This should match the factors defined in the Customize Rating Criteria tab
      
      accordion(
        accordion_panel(
          "Performance Measures",
          div(
            style = "padding: 15px;",
            numericInput("los_score", "Length of Stay Score",
                         value = NA, min = 0, max = 100),
            numericInput("exits_ph_score", "Exits to Permanent Housing Score",
                         value = NA, min = 0, max = 100)
          )
        ),
        accordion_panel(
          "Serve High Needs Populations",
          div(
            style = "padding: 15px;",
            numericInput("assessment_score", "Coordinated Assessment Score",
                         value = NA, min = 0, max = 100),
            numericInput("ch_score", "Chronic Homeless Focus Score",
                         value = NA, min = 0, max = 100)
          )
        )
      )
    })
    
    # Rating Summary UI
    output$renewal_rating_summary <- renderUI({
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
