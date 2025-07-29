pop_grp_toggles <- expand.grid(
  pop = c("All", lookups[reference_type == "target_population"]$value),
  grp = lookups[reference_type == "population_group"]$value_long
) %>%
  qDT() %>%
  fsubset(pop != "Not Applicable") %>%
  roworder(-grp, pop) %>%
  as_character_factor() %>%
  fmutate(
    pop = fifelse(pop == "Domestic Violence", "DV", pop),
    full_text = fcase(
      pop == "Youth" & grp == "Families", "Parenting Youth",
      pop == "Youth" & grp == "Individuals", "Single Youth",
      default = paste(pop, grp)
    )
  )

mod_funding_priorities_ui <- function(id) {
  ns <- NS(id)

  # Funding Ceilings + Priorities
  nav_panel(
    "Funding Ceilings + Priorities",
    value = "funding_priorities",
    card(
      min_height=300,
      card_header("General Funding Information"),
      div(
        style = "padding: 15px;",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          numericInput(ns("total_ard"), "Annual Renewal Demand (ARD)", value = "0"),
          numericInput(ns("coc_bonus"), "CoC Bonus", value = "0"),
          numericInput(ns("tier_1"), "Tier 1", value = "0"),
          numericInput(ns("adjusted_ard"), "Adjusted ARD", value = "0"),
          numericInput(ns("yhdp_ard"), "YHDP ARD", value = "0"),
          numericInput(ns("tier_2"), "Tier 2", value = "0"),
          numericInput(ns("dv_bonus"), "DV Bonus", value = "0"),
          numericInput(ns("dv_ard"), "DV ARD", value = "0")
        )
      )
    ),
    card(
      min_height=300,
      card_header("FY2024 HUD CoC Program NOFO Opportunities"),
      div(
        style = "padding: 15px;",
        layout_columns(
          col_widths = c(6, 6),
          div(
            h4("Project Types to Consider for CoC Bonus"),
            checkboxGroupInput(
              ns("coc_bonus_types"),
              NULL,
              choices = c(
                "RRH for individuals" = "rrh_ind",
                "RRH for families" = "rrh_fam",
                "TH+RRH for individuals" = "th_rrh_ind",
                "TH+RRH for families" = "th_rrh_fam"
              )
            )
          ),
          div(
            h4("Project Types to Consider for DV Bonus"),
            checkboxGroupInput(
              ns("dv_bonus_types"),
              NULL,
              choices = c(
                "RRH for individuals" = "rrh_ind",
                "RRH for families" = "rrh_fam",
                "TH+RRH for individuals" = "th_rrh_ind",
                "TH+RRH for families" = "th_rrh_fam"
              )
            )
          )
        )
      )
    ),
    card(
      card_header("Funding Ceilings and Priorities by Project Type and Population"),
      fill = FALSE,
      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = "15%",
          checkboxGroupInput(
            ns("population_toggles"),
            label = "Enable/Disable Populations",
            choices = pop_grp_toggles$full_text,
            selected = c("All Families","All Individuals", "Single Youth")
          )
        ),
        DTOutput(ns("priorities_table"))
      )
    )
  )
}

mod_funding_priorities_server <- function(id, selected_coc) {
  moduleServer(id, function(input, output, session) {
  
    ard_field_names <- c(
      "total_ard",
      "tier_1",
      "tier_2",
      "adjusted_ard",
      "yhdp_ard",
      "dv_ard",
      "coc_bonus",
      "dv_bonus"
    )
    hud_ard_coc_data <- reactive({
      hud_ard_report[coc == selected_coc()] %>%
        fmutate(
          tier_2 = estimated * 0.1 + coc_bonus + dv_bonus,
          adjusted_ard = round(tier_1/0.9, 0),
          yhdp_ard = estimated - adjusted_ard,
          dv_ard = as.numeric(NA)
        ) %>%
        frename(estimated = "total_ard")
    })
    
    observe({
      req(selected_coc())

      lapply(ard_field_names, function(id) {
        updateNumericInput(
          session, 
          id, 
          value = hud_ard_coc_data()[[id]]
        )
        if(id != "dv_ard") shinyjs::disable(id)
      })
    })
    # Loop through and create outputs
    
    
    # Priorities table
    priorities_data <- reactiveVal(
      data.table(
        Population = pop_grp_toggles,
        Enabled = TRUE,  # New column to track enabled/disabled state
        stringsAsFactors = FALSE
      )
    )
    
    output$priorities_table <- renderDT({
      data <- data.table(
        Population = input$population_toggles
      )
      
      for(pt in main_project_types) {
        data[[paste0(pt, "_Beds")]] <- NA_real_
        data[[paste0(pt, "_Funding")]] <- NA_real_
        data[[paste0(pt, "_Priority")]] <- NA_character_
      }
      
      # Create the header structure
      header <- list(
        Population = 'Population',
        PSH = c('Project Type', 'PSH'),
        RRH = c('Project Type', 'RRH'),
        TH = c('Project Type', 'TH'),
        'TH+RRH' = c('Project Type', 'TH+RRH')
      )
      
      datatable(
        data,
        selection = 'none',
        rownames = FALSE,
        container = tags$table(
          tags$thead(
            tags$tr(
              tags$th(rowspan = 2, 'Population'),
              lapply(main_project_types, function(pt) {
                tags$th(colspan = 3, pt)
              })
            ),
            tags$tr(
              lapply(rep(c("Beds", "Funding", "Priority"), length(main_project_types)), function(col) {
                tags$th(col)
              })
            )
          )
        ),
        autoHideNavigation = TRUE,
        editable = list(
          target = 'row',
          disable = list(columns = c(0))
        ),
        options = list(dom = 't', pageLength = 14)
      )
    })
    
    # Handle population toggles
    observeEvent(input$population_toggles, {
      dataTableProxy("priorities_table") |> 
        replaceData(priorities_data())
    })
    
    # Update priorities data when cell is edited
    observeEvent(input$priorities_table_cell_edit, {
      info <- input$priorities_table_cell_edit
      data <- priorities_data()
      
      # Update the value
      data[info$row + 1, info$col + 1] <- info$value
      
      priorities_data(data)
    })
  })
}
