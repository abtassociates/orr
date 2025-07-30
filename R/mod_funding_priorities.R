pop_grp_toggles <- expand.grid(
  pop = c("All" = 0, get_labelled_lookups("target_population")),
  grp = get_labelled_lookups("population_group", lookup_col = "value_long")
) %>%
  qDT() %>%
  fmutate(
    pop_txt = ifelse(names(pop) == "Domestic Violence", "DV", names(pop)),
    grp_txt = names(grp)
  ) %>%
  fsubset(pop_txt != "Not Applicable") %>%
  setorder(-grp, pop) %>%
  fmutate(
    full_text = fcase(
      pop_txt == "Youth" & grp_txt == "Families", "Parenting Youth",
      pop_txt == "Youth" & grp_txt == "Individuals", "Single Youth",
      default = paste(pop_txt, grp_txt)
    )
  )

mod_funding_priorities_ui <- function(id) {
  ns <- NS(id)

  coc_bonus_opportunities <- coc_nofo_opportunities[
    bonus_type == "CoC Bonus", 
    setNames(coc_nofo_opportunity_id, full_text)
  ]
  
  dv_bonus_opportunities <- coc_nofo_opportunities[
    bonus_type == "DV Bonus", 
    setNames(coc_nofo_opportunity_id, full_text)
  ]
  
  # Funding Ceilings + Priorities
  nav_panel(
    "Funding Ceilings + Priorities",
    value = "funding_priorities",
    card(
      min_height=300,
      card_header("General Funding Information"),
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
    ),
    card(
      min_height=300,
      card_header("FY2024 HUD CoC Program NOFO Opportunities"),
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("Project Types to Consider for CoC Bonus"),
          layout_columns(
            col_widths = c(6, 6),
            checkboxGroupInput(
              ns("coc_bonus_types"),
              NULL,
              choices = coc_bonus_opportunities %>% head(length(.)/2)
            ),
            checkboxGroupInput(
              ns("coc_bonus_types"),
              NULL,
              choices = coc_bonus_opportunities %>% tail(length(.)/2)
            )
          )
        ),
        card(
          card_header("Project Types to Consider for DV Bonus"),
          checkboxGroupInput(
            ns("dv_bonus_types"),
            NULL,
            choices = dv_bonus_opportunities
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
          width = "20%",
          checkboxGroupInput(
            ns("population_toggles"),
            label = "Enable/Disable Populations",
            choices = pop_grp_toggles$full_text
          )
        ),
        DTOutput(ns("priorities_table"))
      )
    )
  )
}

mod_funding_priorities_server <- function(id, selected_coc) {
  moduleServer(id, function(input, output, session) {
    
    data_has_changed <- reactiveVal(FALSE)
    auto_save_timer <- reactiveTimer(5000)

    # HUD ARD Data------------------
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
      hud_ard_report[coc == selected_coc$coc] %>%
        fmutate(
          tier_2 = estimated * 0.1 + coc_bonus + dv_bonus,
          adjusted_ard = round(tier_1/0.9, 0),
          yhdp_ard = estimated - adjusted_ard,
          dv_ard = as.numeric(NA)
        ) %>%
        frename(estimated = "total_ard")
    })
    
    observe({
      req(selected_coc$coc)
      
      lapply(ard_field_names, function(id) {
        updateNumericInput(
          session, 
          id, 
          value = hud_ard_coc_data()[[id]]
        )
        if(id != "dv_ard") shinyjs::disable(id)
      })
    })
    
    # Priorities table -----------------
    get_coc_funding_priorities <- reactive({
      # set the target population + population group checkbox selections
      # for the priorities table. If they have any data so far, check the corresponding box
      coc_funding_priorities <- get_db_query(
        "SELECT * 
        FROM coc_funding_priorities 
        WHERE coc_instance_id = $1 AND (beds IS NOT NULL OR funding IS NOT NULL or priority IS NOT NULL)",
        params = list(selected_coc$coc_instance_id)
      )
      
      # default if no priorities entered
      if(nrow(coc_funding_priorities) == 0) 
        return(c("All Families", "All Individuals", "Single Youth"))
      
      coc_funding_priorities <- coc_funding_priorities %>%
        join(
          pop_grp_toggles,
          on = c("target_population" = "pop", "population_group" = "grp")
        ) %>%
        fmutate(
          pop = fifelse(pop == "Domestic Violence", "DV", pop),
          full_text = fcase(
            pop == "Youth" & grp == "Families", "Parenting Youth",
            pop == "Youth" & grp == "Individuals", "Single Youth",
            default = paste(pop, grp)
          )
        )
    
      # select the checkboxes for which there is any entry
      lapply(coc_funding_priorities, function(p) {
        if(!is.null(p$beds) || !is.null(p$priority) || !is.null(p$funding)) p$full_text
      })
    })
    
    # Update target pop + pop group priority toggle checkboxes
    observe({
      updateCheckboxGroupInput(
        session,
        "population_toggles",
        selected = get_coc_funding_priorities()
      )
    })
    
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
      datatable(
        data,
        selection = 'none',
        rownames = FALSE,
        container = tags$table(
          tags$thead(
            tags$tr(
              tags$th(rowspan = 2, 'Population'),
              lapply(main_project_types, function(pt) {
                tags$th(colspan = 3, pt, style = "border-right: 1px solid black")
              })
            ),
            tags$tr(
              lapply(rep(c("Beds", "Funding", "Priority"), length(main_project_types)), function(col) {
                tags$th(col, style=ifelse(col == "Priority", "border-right: 1px solid black", ""))
              })
            )
          )
        ),
        editable = list(
          target = 'cell',
          disable = list(columns = c(0))
        ),
        options = list(
          dom = 't', 
          pageLength = nrow(data),
          ordering = FALSE,
          searching = FALSE,
          info = FALSE
        )
      ) %>% formatStyle(
        columns = seq(4, ncol(data), by = 3),  # Priority columns (every 3rd column starting from 3)
        `border-right` = "1px solid black"
      )
    })
    
    # Toggle which target population + population group is to be prioritized
    # default is All Families, All Individuals, and Single Youth
    priorities_data <- reactiveVal(
      data.table(
        Population = pop_grp_toggles,
        Enabled = TRUE,  # New column to track enabled/disabled state
        stringsAsFactors = FALSE
      )
    )
    
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
      data_has_changed(TRUE) 
    })
    
    observe({
      # This code runs every 5 seconds (because it depends on the timer)
      auto_save_timer() 

      # Only proceed if data has actually changed
      if (data_has_changed()) {
        browser()
        # Isolate the data to prevent reactive loops
        data_to_save <- isolate(priorities_data())
        browser()
        # The "UPSERT" query
        sql_query <- "
          INSERT INTO coc_funding_priorities (coc_instance_id, project_type, target_population, beds, funding, priority, created_by)
          VALUES ($1, $2, $3, $4, $5, $6, $7)
          ON CONFLICT (coc_instance_id, project_type, target_population)
          DO UPDATE SET 
            beds = $4, 
            funding = $5, 
            priority = $6, 
            updated_by = $7;
        "
        tryCatch({
          # Execute the query for each row of the long data frame
          # Using a prepared statement with `dbExecute` and `params` is safe from SQL injection
          apply(long_data, 1, function(row) {
            dbExecute(
              DB_CON,
              sql_query,
              params = list(
                selected_coc$coc_instance_id,
                row[["population_group"]],
                row[["project_type"]],
                row[["metric"]],
                as.character(row[["value"]]) # Ensure value is character/text
              )
            )
          })
          
          # If successful, reset the flag and notify the user
          data_has_changed(FALSE)
          showNotification("Changes saved successfully!", type = "message", duration = 3)
        }, error = function(e) {
          # If an error occurs, do NOT reset the flag, so it will try again.
          # Notify the user of the failure.
          showNotification(paste("Error saving data:", e$message), type = "error", duration = 10)
          cat("Database save error:", e$message, "\n")
        })
      }
    })
  })
}
