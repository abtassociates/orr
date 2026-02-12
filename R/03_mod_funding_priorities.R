pop_grp_toggles <- expand.grid(
  pop = c("All" = 0, get_labelled_lookups("target_population", lookup_col = "value_long")),
  grp = get_labelled_lookups("population_group", lookup_col = "value_long")
) |>
  qDT() |>
  fmutate(
    pop_txt = ifelse(names(pop) == "Domestic Violence", "DV", names(pop)),
    grp_txt = names(grp)
  ) |>
  fsubset(!pop_txt %in% c("Not Applicable", "Human Immunodeficiency Virus", "General")) |>
  setorder(-grp, pop) |>
  fmutate(
    full_text = fcase(
      pop_txt == "Youth" & grp_txt == "Families", "Parenting Youth",
      pop_txt == "Youth" & grp_txt == "Individuals", "Single Youth",
      default = paste(pop_txt, grp_txt)
    )
  )

mod_funding_priorities_ui <- function(id) {
  ns <- NS(id)

  coc_bonus_opportunities <- COC_NOFO_OPPORTUNITIES[
    bonus_type == "CoC Bonus", 
    setNames(coc_nofo_opportunity_id, full_text)
  ]
  
  dv_bonus_opportunities <- COC_NOFO_OPPORTUNITIES[
    bonus_type == "DV Bonus", 
    setNames(coc_nofo_opportunity_id, full_text)
  ]
  
  # Funding Ceilings + Priorities
  nav_panel(
    "Funding Ceilings + Priorities",
    value = id,
    icon = icon("usd"),
    card(
      min_height=300,
      card_header("General Funding Information"),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        shinyWidgets::autonumericInput(ns("total_ard"), "Annual Renewal Demand (ARD)", value = "0", currencySymbol = "$", currencySymbolPlacement = "p", decimalPlaces = 0),
        shinyWidgets::autonumericInput(ns("coc_bonus"), "CoC Bonus", value = "0", currencySymbol = "$", currencySymbolPlacement = "p", decimalPlaces = 0),
        shinyWidgets::autonumericInput(ns("tier_1"), "Tier 1", value = "0", currencySymbol = "$", currencySymbolPlacement = "p", decimalPlaces = 0),
        shinyWidgets::autonumericInput(ns("adjusted_ard"), "Adjusted ARD", value = "0", currencySymbol = "$", currencySymbolPlacement = "p", decimalPlaces = 0),
        shinyWidgets::autonumericInput(ns("yhdp_ard"), "YHDP ARD", value = "0", currencySymbol = "$", currencySymbolPlacement = "p", decimalPlaces = 0),
        shinyWidgets::autonumericInput(ns("tier_2"), "Tier 2", value = "0", currencySymbol = "$", currencySymbolPlacement = "p", decimalPlaces = 0),
        shinyWidgets::autonumericInput(ns("dv_bonus"), "DV Bonus", value = "0", currencySymbol = "$", currencySymbolPlacement = "p", decimalPlaces = 0),
        shinyWidgets::autonumericInput(ns("dv_ard"), "DV ARD", value = "0", currencySymbol = "$", currencySymbolPlacement = "p", decimalPlaces = 0)
      )
    ),
    card(
      min_height=300,
      card_header("FY2026 HUD CoC Program NOFO Opportunities"),
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_title("Project Types to Consider for CoC Bonus"),
          layout_columns(
            col_widths = c(6, 6),
            checkboxGroupInput(
              ns("coc_bonus_types_1"),
              NULL,
              choices = coc_bonus_opportunities %>% head(length(.)/2) #needs to be %>% instead of |>
            ),
            checkboxGroupInput(
              ns("coc_bonus_types_2"),
              NULL,
              choices = coc_bonus_opportunities %>% tail(length(.)/2) #needs to be %>% instead of |>
            )
          )
        ),
        card(
          card_title("Project Types to Consider for DV Bonus"),
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
          width = "22%",
          checkboxGroupInput(
            ns("population_toggles"),
            label = "Enable/Disable Populations",
            choices = pop_grp_toggles$full_text
          )
        ),
        helpText("Double-click a cell to edit"),
        DTOutput(ns("priorities_table"))
      )
    )
  )
}

mod_funding_priorities_server <- function(id, nav_control, user_coc, parent_session, module_returns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
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
      HUD_ARD_REPORT[coc == user_coc$coc] |>
        fmutate(
          tier_2 = estimated * 0.1 + coc_bonus + dv_bonus,
          adjusted_ard = round(tier_1/0.9, 0),
          yhdp_ard = estimated - adjusted_ard,
          dv_ard = as.numeric(NA)
        ) |>
        frename(estimated = "total_ard")
    })
    
    # CoC Nofo Opportunities ---------------
    coc_nofo_inputs_initialized <- reactiveVal(FALSE)
    initialize_coc_nofo_opportunity_inputs <- function() {
      vals <- db_selected_coc_nofo_opportunities()[val == 1]
      
      coc_bonus_types <- vals[bonus_type == get_lookup_refid("CoC Bonus", "bonus_type")]
      updateCheckboxGroupInput(
        session, "coc_bonus_types_1", selected = coc_bonus_types[coc_nofo_opportunity_id %in% 1:4]$coc_nofo_opportunity_id
      )
      updateCheckboxGroupInput(
        session, "coc_bonus_types_2", selected = coc_bonus_types[coc_nofo_opportunity_id %in% 5:8]$coc_nofo_opportunity_id
      )
      
      dv_bonus_types <- vals[bonus_type == get_lookup_refid("DV Bonus", "bonus_type")]
      updateCheckboxGroupInput(
        session, "dv_bonus_types", selected = dv_bonus_types$coc_nofo_opportunity_id
      )
      
      coc_nofo_inputs_initialized(TRUE)
    }
    
    observe({
      req(user_coc$coc)
      lapply(ard_field_names, function(i) {
        updateAutonumericInput(
          session, 
          i, 
          value = hud_ard_coc_data()[[i]]
        )
        if(i != "dv_ard") shinyjs::disable(i)
      })
      
      # initialize selected coc_nofo_opportunities
      db_selected_coc_nofo_opportunities(
        get_db_query(
          "SELECT c.coc_nofo_opportunity_id, c.bonus_type,
            s.coc_nofo_opportunity_id IS NOT NULL AS val, 
            s.coc_nofo_opportunity_id IS NOT NULL AS new_val,
            $1 AS coc_version_id
          FROM coc_nofo_opportunities c
          LEFT JOIN selected_coc_nofo_opportunities s ON c.coc_nofo_opportunity_id = s.coc_nofo_opportunity_id AND coc_version_id = $2", 
          params = list(user_coc$coc_version_id, user_coc$coc_version_id)
        )
      )
      
      # initialize inputs
      initialize_coc_nofo_opportunity_inputs()
    })
    
    # CoC Bonus
    db_selected_coc_nofo_opportunities <- reactiveVal()
    
    observeEvent(
      c(
        input$coc_bonus_types_1,
        input$coc_bonus_types_2,
        input$dv_bonus_types
      ), {
      req(coc_nofo_inputs_initialized())
        
      newly_selected <- as.integer(c(
        input$coc_bonus_types_1,
        input$coc_bonus_types_2,
        input$dv_bonus_types
      ))

      selected_opps <- isolate(db_selected_coc_nofo_opportunities())
      req(!identical(selected_opps[val == 1]$coc_nofo_opportunity_id,  newly_selected))
      
      # Create full indicator vector
      db_selected_coc_nofo_opportunities(
        selected_opps %>%
          fmutate(new_val = as.integer(coc_nofo_opportunity_id %in% newly_selected))
      )
      
      # update database
      update_coc_nofo_opportunities_db()
    })
    
    update_coc_nofo_opportunities_db <- function() {
      vals <- db_selected_coc_nofo_opportunities()

      #to insert
      to_insert <- vals |> 
        fsubset(val == 0 & new_val == 1) |> 
        fselect(-val, -new_val, -bonus_type) |>
        add_user_stamp(user_coc, is_new = TRUE)
      
      if(fnrow(to_insert) > 0)
        dbAppendTable(DB_CON, "selected_coc_nofo_opportunities", to_insert)
      
      
      # to delete
      to_remove <- vals[val == 1 & new_val == 0]$coc_nofo_opportunity_id
      if (length(to_remove) > 0) {
        dbExecute(DB_CON, glue::glue_sql("
              DELETE FROM selected_coc_nofo_opportunities
              WHERE coc_version_id = {user_coc$coc_version_id} AND coc_nofo_opportunity_id IN ({to_remove*})
            ", .con = DB_CON))
      }
    }
    
    # Priorities table -----------------
    priorities_data <- reactiveVal(NULL)
    
    # Get db data
    coc_funding_priorities_from_db <- reactive({
      req(user_coc$coc_version_id)
      get_db_query(
        "SELECT * 
           FROM coc_funding_priorities 
           WHERE coc_version_id = $1 AND (beds IS NOT NULL OR funding IS NOT NULL or priority IS NOT NULL)",
        params = list(user_coc$coc_version_id)
      )
    })
    
    
    # initialize datatable using db data
    observeEvent(coc_funding_priorities_from_db(), {
      
       # 1. Need to get this into app-ready data structure, e.g.
        #                           Population PSH_Beds PSH_Funding PSH_Priority RRH_Beds RRH_Funding RRH_Priority TH_Beds TH_Funding TH_Priority TH+RRH_Beds TH+RRH_Funding TH+RRH_Priority
        #                               <char>    <num>       <num>       <char>    <num>       <num>       <char>   <num>      <num>      <char>       <num>          <num>          <char>
        # 1:                     All Families       NA          NA         <NA>       NA          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        # 2:                      DV Families       NA          NA         <NA>       NA          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        # 3:    Chronically Homeless Families       NA          NA         <NA>       10          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        # 4:                 Veteran Families       NA        5000         <NA>       NA          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        # 5:                  Parenting Youth       NA          NA         <NA>       NA          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        # 6:                  All Individuals       NA          NA         <NA>       NA          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        # 7:                   DV Individuals       NA          NA         <NA>       NA          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        # 8: Chronically Homeless Individuals       NA          NA         <NA>       NA          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        # 9:              Veteran Individuals       NA          NA         <NA>       NA          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        # 10:                     Single Youth      NA          NA         <NA>       NA          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
      full_data <- coc_funding_priorities_from_db() |>
        # Get full "Population" text (target_pop + pop_grp)
        join(
          pop_grp_toggles |> fselect(Population = full_text, pop, grp),
          on = c("target_population" = "pop", "population_group" = "grp"),
          how = "right"
        ) |>
        fselect(Population, project_type, beds, funding, priority) |>
        # For each Population, get all main project types, still in long format
        join(
          expand.grid(
            Population = pop_grp_toggles$full_text,
            project_type = LOOKUPS[reference_type == "project_type" & value %in% MAIN_PROJECT_TYPES]$reference_id
          ),
          how = "right"
        ) |>
        # convert project type number to label
        fmutate(
          project_type = get_lookup_label(project_type, 'project_type')
        ) |>
        # pivot to app data structure
        pivot(
          ids = "Population",
          how="wider",
          names = "project_type",
          transpose = "names"
        ) |>
        colorderv(
          paste0("^", gsub("+", "\\+", MAIN_PROJECT_TYPES, fixed = TRUE), "_"), 
          regex=TRUE, 
          pos="end"
        )
      
      # save to reactive
      priorities_data(full_data)
      
      # set initial Population toggles
      update_population_toggles(full_data)
    }, ignoreNULL = TRUE)
    
    update_population_toggles <- function(full_data) {
      selected_populations <- full_data %>%
        dplyr::filter(dplyr::if_any(-Population, ~ !is.na(.)))
      
      updateCheckboxGroupInput(
        session,
        "population_toggles",
        selected = selected_populations$Population
      )
    }
    
    
    output$priorities_table <- renderDT({
      # Require these two things to be ready before rendering
      req(input$population_toggles)
      
      # Filter the full dataset based on the selected checkboxes
      data_to_display <- isolate(priorities_data()[Population %in% input$population_toggles])

      # Create the header structure
      datatable(
        data_to_display,
        selection = 'none',
        style = 'default',
        rownames = FALSE,
        container = tags$table(
          tags$thead(
            tags$tr(
              tags$th(rowspan = 2, 'Population'),
              lapply(MAIN_PROJECT_TYPES, function(pt) {
                tags$th(colspan = 3, pt, style = "border-right: 1px solid black; text-align: center")
              })
            ),
            tags$tr(
              lapply(rep(c("Beds", "Funding", "Priority"), length(MAIN_PROJECT_TYPES)), function(col) {
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
          pageLength = nrow(data_to_display),
          #ordering = FALSE,
          searching = FALSE,
          info = FALSE
        ),
        callback = JS("$(document).on('mouseenter', 'table.dataTable tbody tr', function() {",
      paste0("$(this).css('background-color', '",USER_ENTRY_BG_COLOR,"');"),
      "});
              $(document).on('mouseleave', 'table.dataTable tbody tr', function() {
      $(this).css('background-color', 'inherit');
      });")
      ) %>% 
        formatStyle(
          columns = seq(4, ncol(data_to_display), by = 3),  # Priority columns (every 3rd column starting from 3)
          `border-right` = "1px solid black"
        ) %>%
        formatCurrency(
          columns = seq(3, ncol(data_to_display), by = 3),
          currency = "$", 
          mark = ",",
          digits = 0
        )
    }, server = FALSE)
    
    priorities_table_proxy <- dataTableProxy("priorities_table")
    
    # Update priorities data in table and db when cell is edited
    observeEvent(input$priorities_table_cell_edit, {
      info <- input$priorities_table_cell_edit
      
      # Update the full dataset 
      current_data <- priorities_data()
      
      # Get the population name from the row that was displayed
      # This is trickier because the view is filtered. We need to map the
      # viewed row index back to the full data index.
      displayed_data <- current_data[Population %in% input$population_toggles]
      population_to_update <- displayed_data[info$row, Population]
      full_data_row_index <- which(current_data$Population == population_to_update)
      
      # Update the value in the full dataset, so we can update the reactive and datatable proxy
      # The column index needs + 1 because datatable is 0 indexed
      current_data[full_data_row_index, (info$col + 1) := info$value]
      
      ## Update database -------------
      changed_data <- current_data[full_data_row_index, c(1, info$col + 1), with=FALSE] |>
        tidyr::pivot_longer(
          cols = -Population,
          names_to = c("project_type", ".value"), # ".value" tells it to keep the second part as column headers
          names_sep = "_"
        ) |>
        join(
          pop_grp_toggles,
          on = c("Population" = "full_text")
        ) |>
        frename(pop = target_population, grp = population_group) |>
        fmutate(project_type = get_lookup_refid(project_type, ref_type = "project_type"))
      
      # bed, funding, or priority
      metric_name <- names(changed_data)[3]
      
      pulled_date_updated <- changed_data |> # date_updated
        join(
          coc_funding_priorities_from_db(),
          on = c("project_type", "target_population", "population_group"),
          how = "left"
        ) |>
        fselect(date_updated)
      
      # need to drop timezone from R timestamp
      timestamp_param <- format(round(pulled_date_updated[[1]], 0), "%Y-%m-%d %H:%M:%S")
      
      sql <- glue::glue(
        "INSERT INTO coc_funding_priorities (coc_version_id, project_type, target_population, population_group, {metric_name}, created_by)
          VALUES ($1, $2, $3, $4, $5, $6)
          ON CONFLICT (coc_version_id, project_type, target_population, population_group)
          DO UPDATE SET 
            {metric_name} = EXCLUDED.{metric_name},
            updated_by = EXCLUDED.created_by, -- Use the 'created_by' value from the attempted insert
            date_updated = CURRENT_TIMESTAMP
          WHERE date_updated = $7;
        "
      )
      
      tryCatch({
        db_execute(
          sql,
          params = list(
            user_coc$coc_version_id,
            changed_data$project_type,
            changed_data$target_population,
            changed_data$population_group,
            changed_data[[metric_name]],
            user_coc$username,
            timestamp_param
          )
        )
        
        ## update reactive ----------
        priorities_data(current_data)
        
        ## update datatable --------
        replaceData(
          priorities_table_proxy, 
          current_data[Population %in% input$population_toggles], 
          resetPaging = FALSE, 
          rownames = FALSE
        )
        
        showNotification("Changes saved successfully!", type = "message", duration = 3)
      }, error = function(e) {
        # If an error occurs, do NOT reset the flag, so it will try again.
        # Notify the user of the failure.
        showNotification(paste("Error saving data:", e$message), type = "error", duration = 10)
        cat("Database save error:", e$message, "\n")
      })
    })
  })
}
