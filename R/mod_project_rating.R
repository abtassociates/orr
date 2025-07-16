# UI modules for project rating
mod_renewal_rating_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header("Select Project"),
      selectInput(ns("project_select"), "Choose Project:",
                 choices = NULL),
      textOutput(ns("project_info"))
    ),
    
    navset_card_tab(
      id = ns("rating_tabs"),
      nav_panel(
        "Threshold",
        navset_pill_card(
          nav_panel(
            "HUD Requirements",
            checkboxInput(ns("yes_to_all_hud"), "Yes to All"),
            uiOutput(ns("hud_requirements")),
            actionButton(ns("save_hud"), "Save HUD Requirements")
          ),
          nav_panel(
            "CoC Requirements",
            checkboxInput(ns("yes_to_all_coc"), "Yes to All"),
            uiOutput(ns("coc_requirements")),
            actionButton(ns("save_coc"), "Save CoC Requirements")
          )
        )
      ),
      nav_panel(
        "Rate",
        card(
          card_header("Project Rating"),
          DTOutput(ns("rating_table")),
          textOutput(ns("total_score")),
          actionButton(ns("save_rating"), "Save Rating")
        )
      )
    )
  )
}

mod_renewal_rating_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    
    # Update project selection choices when CoC is selected
    observe({
      req(app_state()$projects)
      renewal_projects <- app_state()$projects %>%
        filter(Funding_Action %in% c("Renew", "Expand"))
      
      updateSelectInput(session, "project_select",
                       choices = renewal_projects$Project_Name)
    })
    
    # Display project info
    output$project_info <- renderText({
      req(input$project_select, app_state()$projects)
      project <- app_state()$projects %>%
        filter(Project_Name == input$project_select)
      
      sprintf("Organization: %s\nProject Type: %s",
              project$Organization_Name,
              project$Project_Type)
    })
    
    # Handle HUD requirements UI and logic
    output$hud_requirements <- renderUI({
      ns <- session$ns
      
      req(input$project_select)
      
      tagList(
        lapply(1:5, function(i) {
          div(class = "requirement-row",
              tags$span(paste("HUD Requirement", i)),
              selectInput(ns(paste0("hud_req_", i)),
                         label = NULL,
                         choices = c("", "Yes", "No"))
          )
        })
      )
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

# UI modules for new project rating
mod_new_rating_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header("Select New Project"),
      selectInput(ns("new_project_select"), "Choose Project:",
                 choices = NULL),
      textOutput(ns("new_project_info"))
    ),
    
    navset_card_tab(
      nav_panel(
        "Threshold",
        navset_pill_card(
          nav_panel(
            "HUD Requirements",
            checkboxInput(ns("new_yes_to_all_hud"), "Yes to All"),
            uiOutput(ns("new_hud_requirements"))
          ),
          nav_panel(
            "CoC Requirements",
            checkboxInput(ns("new_yes_to_all_coc"), "Yes to All"),
            uiOutput(ns("new_coc_requirements"))
          )
        )
      ),
      nav_panel(
        "Rate",
        card(
          card_header("Project Rating"),
          DTOutput(ns("new_rating_table")),
          textOutput(ns("new_total_score"))
        )
      )
    )
  )
}