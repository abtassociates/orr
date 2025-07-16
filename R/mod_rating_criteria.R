mod_thresholds_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header("Threshold Requirements"),
      checkboxGroupInput(
        ns("hud_thresholds"),
        "HUD Threshold Requirements",
        choices = c(
          "Project is on CoC Registration",
          "Project meets HUD eligibility requirements",
          "Project has adequate match"
        ),
        selected = c(
          "Project is on CoC Registration",
          "Project meets HUD eligibility requirements",
          "Project has adequate match"
        )
      ),
      actionButton(ns("add_threshold"), "Add New Threshold")
    )
  )
}

mod_renewal_factors_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header("Renewal Project Rating Factors"),
      fluidRow(
        column(4,
          selectInput(ns("project_type_filter"), "Filter by Project Type:",
                     choices = c("All", PROJECT_TYPES),
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

mod_new_factors_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header("New Project Rating Factors"),
      uiOutput(ns("new_project_factors"))
    )
  )
}