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
          numericInput(ns("ard"), "Annual Renewal Demand (ARD)", value = "0"),
          numericInput(ns("coc_bonus"), "CoC Bonus", value = "0"),
          numericInput(ns("tier1"), "Tier 1", value = "0"),
          numericInput(ns("adjusted_ard"), "Adjusted ARD", value = "0"),
          numericInput(ns("yhdp_ard"), "YHDP ARD", value = "0"),
          numericInput(ns("tier2"), "Tier 2", value = "0"),
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
    div(
      style = "display: flex; gap: 20px;",
      div(
        style = "width: 250px;",
        card(
          card_header("Enable/Disable Populations"),
          checkboxGroupInput(
            ns("population_toggles"),
            NULL,
            choices = c(
              "All Families" = "1",
              "DV Families" = "2",
              "Chronically Homeless Families" = "3",
              "Veteran Families" = "4",
              "Parenting Youth" = "5",
              "All Individuals" = "6",
              "DV Individuals" = "7",
              "Chronically Homeless Individuals" = "8",
              "Veteran Individuals" = "9",
              "Single Youth" = "10"
            ),
            selected = 1:10
          )
        )
      ),
      div(
        style = "flex-grow: 1;",
        card(
          card_header("Funding Ceilings and Priorities by Project Type and Population"),
          div(
            style = "padding: 15px;",
            DTOutput(ns("priorities_table"))
          )
        )
      )
    )
  )
}

mod_funding_priorities_server <- function(id, projects_data) {
  moduleServer(id, function(input, output, session) {
    
    # Funding ceiling calculations and displays
    output$ard_display <- renderText({
      req(selected_coc())
      ard_value <- ard_data$Total_ARD[ard_data$CoC_Code == selected_coc()]
      paste("ARD: $", format(ard_value, big.mark=",", scientific=FALSE))
    })
    
    output$dv_bonus_display <- renderText({
      req(selected_coc())
      bonus_value <- ard_data$DV_Bonus[ard_data$CoC_Code == selected_coc()]
      paste("DV Bonus: $", format(bonus_value, big.mark=",", scientific=FALSE))
    })
    
    output$coc_bonus_display <- renderText({
      req(selected_coc())
      bonus_value <- ard_data$CoC_Bonus[ard_data$CoC_Code == selected_coc()]
      paste("CoC Bonus: $", format(bonus_value, big.mark=",", scientific=FALSE))
    })
    
    output$tier1_display <- renderText({
      req(selected_coc())
      tier1_value <- ard_data$Tier_1[ard_data$CoC_Code == selected_coc()]
      paste("Tier 1: $", format(tier1_value, big.mark=",", scientific=FALSE))
    })
    
    output$adjusted_ard_display <- renderText({
      req(selected_coc())
      tier1_value <- ard_data$Tier_1[ard_data$CoC_Code == selected_coc()]
      adjusted_ard <- tier1_value/0.9
      paste("Adjusted ARD: $", format(adjusted_ard, big.mark=",", scientific=FALSE))
    })
    
    output$yhdp_ard_display <- renderText({
      req(selected_coc())
      ard_value <- ard_data$Total_ARD[ard_data$CoC_Code == selected_coc()]
      tier1_value <- ard_data$Tier_1[ard_data$CoC_Code == selected_coc()]
      adjusted_ard <- tier1_value/0.9
      yhdp_ard <- ard_value - adjusted_ard
      paste("YHDP ARD: $", format(yhdp_ard, big.mark=",", scientific=FALSE))
    })
    
    output$tier2_display <- renderText({
      req(selected_coc())
      tier1_value <- ard_data$Tier_1[ard_data$CoC_Code == selected_coc()]
      coc_bonus <- ard_data$CoC_Bonus[ard_data$CoC_Code == selected_coc()]
      dv_bonus <- ard_data$DV_Bonus[ard_data$CoC_Code == selected_coc()]
      adjusted_ard <- tier1_value/0.9
      tier2 <- (adjusted_ard * 0.1) + coc_bonus + dv_bonus
      paste("Tier 2: $", format(tier2, big.mark=",", scientific=FALSE))
    })
    
    # Priorities table
    priorities_data <- reactiveVal(
      data.frame(
        Population = c(
          "All Families", "DV Families", "Chronically Homeless Families",
          "Veteran Families", "Parenting Youth",
          "All Individuals", "DV Individuals", "Chronically Homeless Individuals",
          "Veteran Individuals", "Single Youth"
        ),
        Enabled = TRUE,  # New column to track enabled/disabled state
        stringsAsFactors = FALSE
      )
    )
    
    output$priorities_table <- renderDT({
      data <- data.frame(
        Population = c(
          "All Families", "DV Families", "Chronically Homeless Families",
          "Veteran Families", "Parenting Youth",
          "All Individuals", "DV Individuals", "Chronically Homeless Individuals",
          "Veteran Individuals", "Single Youth"
        )
      )
      
      # Create columns for each project type
      project_types <- c("PSH", "RRH", "TH", "TH+RRH")
      
      for(pt in project_types) {
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
          class = 'display',
          tags$thead(
            tags$tr(
              tags$th(rowspan = 2, 'Population'),
              lapply(project_types, function(pt) {
                tags$th(colspan = 3, pt)
              })
            ),
            tags$tr(
              lapply(rep(c("Beds", "Funding", "Priority"), length(project_types)), function(col) {
                tags$th(col)
              })
            )
          )
        ),
        editable = list(
          target = 'cell',
          disable = list(columns = c(0))
        ),
        options = list(
          pageLength = 10,
          dom = 't'
        )
      ) %>%
        formatStyle(
          'Population',
          target = 'row' #,
          # backgroundColor = JS(sprintf(
          #   "function(data, type, row, meta) {
          #     return $('#population_toggles-%s:checked').length ? 'white' : '#f5f5f5';
          #   }",
          #   1:10
          # ))
        )
    })
    
    # Handle population toggles
    observeEvent(input$population_toggles, {
      dataTableProxy("priorities_table") %>% 
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