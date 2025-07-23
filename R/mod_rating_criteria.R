mod_rating_criteria_ui <- function(id) {
  nav_panel(
    "Customize Rating Criteria",
    value = "rating_criteria",
    navset_card_tab(
      mod_thresholds_ui("select_thresholds"),
      mod_renewal_factors_ui("select_renewal_factors"),
      mod_new_factors_ui("select_new_factors")
    )
  )
}

mod_rating_criteria_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    # Reactive value for custom thresholds
    custom_thresholds <- reactiveVal(character(0))
    
    mod_thresholds_server("select_thresholds")
    
    mod_renewal_factors_server("select_renewal_factors")
  })
}


mod_thresholds_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    # Add new threshold
    observeEvent(input$add_threshold_btn, {
      showModal(modalDialog(
        title = "Add New Threshold Requirement",
        textInput(ns("new_threshold"), "Requirement Text"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_threshold", "Save", class = "btn-primary")
        )
      ))
    })
    
    # Save new threshold
    observeEvent(input$save_threshold, {
      req(input$new_threshold)
      current <- custom_thresholds()
      custom_thresholds(c(current, input$new_threshold))
      updateCheckboxGroupInput(session, "coc_thresholds",
                               choices = c(input$coc_thresholds, input$new_threshold),
                               selected = c(input$coc_thresholds, input$new_threshold))
      removeModal()
    })
  })
}

mod_thresholds_ui <- function(id) {
  ns <- NS(id)

  # Thresholds tab
  nav_panel(
    "Thresholds",
    card(
      checkboxGroupInput(ns("coc_thresholds"), "CoC Threshold Requirements",
                         choices = c(
                           "Housing First Approach",
                           "Participates in Coordinated Entry",
                           "Active Board Member",
                           "Submits APR timely",
                           "No unresolved monitoring findings"
                         ),
                         selected = c(
                           "Housing First Approach",
                           "Participates in Coordinated Entry",
                           "Active Board Member",
                           "Submits APR timely",
                           "No unresolved monitoring findings"
                         )),
      actionButton(ns("add_threshold_btn"), "Add New Threshold", class = "btn-primary")
    )
  )
}

mod_renewal_factors_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    card(
      card_header("Renewal Project Rating Factors"),
      fluidRow(
        column(4,
          selectInput(ns("project_type_filter"), "Filter by Project Type:",
                     choices = c("All", lookups$project_types$project_type),
                     multiple = TRUE)
        ),
        column(4,
          selectInput(ns("population_filter"), "Filter by Population:",
                     choices = c("All", "General", "DV"),
                     multiple = TRUE)
        )
      ),
      uiOutput(ns("rating_factors"))
    )
  )
}

mod_renewal_factors_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Generate rating factors UI
    output$rating_factors <- renderUI({
      # Define base factors
      base_factors <- list(
        "Performance Measures" = c(
          "Length of Stay",
          "Exits to Permanent Housing"
        ),
        "Serve High Needs Populations" = c(
          "Coordinated Assessment Score",
          "Project focuses on chronically homeless people"
        )
      )
      
      # Filter by project type and population
      selected_project_types <- if ("All" %in% input$rating_project_type_filter) {
        lookups$project_types$project_type
      } else {
        input$rating_project_type_filter
      }
      
      selected_populations <- if ("All" %in% input$rating_population_filter) {
        lookups$target_populations$target_population
      } else {
        input$rating_population_filter
      }
      
      # Generate UI elements
      tagList(
        accordion(
          lapply(names(base_factors), function(group) {
            accordion_panel(
              title = group,
              lapply(base_factors[[group]], function(factor) {
                lapply(selected_project_types, function(pt) {
                  lapply(selected_populations, function(pop) {
                    div(
                      style = "margin-bottom: 15px; padding: 10px; border-bottom: 1px solid #eee;",
                      fluidRow(
                        column(4, 
                               checkboxInput(
                                 paste0("factor_", make.names(paste(factor, pt, pop))),
                                 paste0(factor, " (", pt, " - ", pop, ")"),
                                 value = TRUE
                               )
                        ),
                        column(4,
                               numericInput(
                                 paste0("goal_", make.names(paste(factor, pt, pop))),
                                 "Factor/Goal",
                                 value = 80,
                                 min = 0,
                                 max = 100
                               )
                        ),
                        column(4,
                               numericInput(
                                 paste0("points_", make.names(paste(factor, pt, pop))),
                                 "Max Point Value",
                                 value = 20,
                                 min = 0,
                                 max = 100
                               )
                        )
                      )
                    )
                  })
                })
              })
            )
          })
        ),
        div(
          style = "margin-top: 15px;",
          actionButton("add_rating_factor", "Add New Rating Factor", class = "btn-primary")
        )
      )
    })
    
    # Add new rating factor
    observeEvent(input$add_rating_factor, {
      showModal(modalDialog(
        title = "Add New Rating Factor",
        textInput("new_factor_text", "Factor Text"),
        selectInput("new_factor_group", "Factor Group",
                    choices = c("Performance Measures", "Serve High Needs Populations")),
        numericInput("new_factor_goal", "Default Factor/Goal", value = 80),
        numericInput("new_factor_points", "Default Max Points", value = 20),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_rating_factor", "Save", class = "btn-primary")
        )
      ))
    })
  })
}
mod_new_factors_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "New Project Rating Factors",
    card(
      card_header("New Project Rating Factors"),
      selectInput("new_rating_population_filter", "Filter by Special Population",
                  choices = c("All", lookups$target_populations$target_population),
                  multiple = TRUE,
                  selected = "All"),
      uiOutput("new_project_rating_factors_ui")
    )
  )
}

mod_new_factors_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    # New Project Rating Factors UI
    output$new_project_rating_factors_ui <- renderUI({
      # Define base factors for new projects
      new_project_factors <- list(
        "Experience" = c(
          "Experience with proposed population",
          "Basic program design"
        ),
        "Implementation" = c(
          "Timeline feasibility",
          "Financial capacity"
        )
      )
      
      selected_populations <- if ("All" %in% input$new_rating_population_filter) {
        lookups$target_populations$target_population
      } else {
        input$new_rating_population_filter
      }
      
      tagList(
        accordion(
          lapply(names(new_project_factors), function(group) {
            accordion_panel(
              title = group,
              lapply(new_project_factors[[group]], function(factor) {
                lapply(selected_populations, function(pop) {
                  div(
                    style = "margin-bottom: 15px; padding: 10px; border-bottom: 1px solid #eee;",
                    fluidRow(
                      column(6, 
                             checkboxInput(
                               paste0("new_factor_", make.names(paste(factor, pop))),
                               paste0(factor, " (", pop, ")"),
                               value = TRUE
                             )
                      ),
                      column(6,
                             numericInput(
                               paste0("new_points_", make.names(paste(factor, pop))),
                               "Max Point Value",
                               value = 20,
                               min = 0,
                               max = 100
                             )
                      )
                    )
                  )
                })
              })
            )
          })
        ),
        div(
          style = "margin-top: 15px;",
          actionButton("add_new_project_factor", "Add New Factor", class = "btn-primary")
        )
      )
    })
    
    # Add new project rating factor
    observeEvent(input$add_new_project_factor, {
      showModal(modalDialog(
        title = "Add New Project Rating Factor",
        textInput("new_project_factor_text", "Factor Text"),
        numericInput("new_project_factor_points", "Default Max Points", value = 20),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_new_project_factor", "Save", class = "btn-primary")
        )
      ))
    })
  })
}