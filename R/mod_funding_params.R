mod_funding_params_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header("Funding Parameters"),
      fluidRow(
        column(6,
          numericInput(ns("ard"), "Annual Renewal Demand (ARD):", value = 0),
          numericInput(ns("tier1"), "Tier 1:", value = 0),
          numericInput(ns("coc_bonus"), "CoC Bonus:", value = 0)
        ),
        column(6,
          numericInput(ns("dv_bonus"), "DV Bonus:", value = 0),
          numericInput(ns("adjusted_ard"), "Adjusted ARD:", value = 0, disabled = TRUE),
          numericInput(ns("tier2"), "Tier 2:", value = 0, disabled = TRUE)
        )
      )
    ),
    card(
      card_header("Project Type Priorities"),
      checkboxGroupInput(ns("coc_bonus_types"), "CoC Bonus Project Types:",
        choices = c(
          "RRH for individuals",
          "RRH for families",
          "TH+RRH for individuals",
          "TH+RRH for families"
        )
      )
    )
  )
}