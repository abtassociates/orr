pop_grp_toggles <- expand.grid(
  pop = c("All" = 0, get_labelled_lookups("target_population", lookup_col = "value_long")),
  grp = get_labelled_lookups("population_group", lookup_col = "value_long")
) %>%
  qDT() %>%
  fmutate(
    pop_txt = ifelse(names(pop) == "Domestic Violence", "DV", names(pop)),
    grp_txt = names(grp)
  ) %>%
  fsubset(!pop_txt %in% c("Not Applicable", "Housing Inventory Count", "General")) %>%
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
    value = id,
    icon = icon("usd"),
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
          card_title("Project Types to Consider for CoC Bonus"),
          layout_columns(
            col_widths = c(6, 6),
            checkboxGroupInput(
              ns("coc_bonus_types_1"),
              NULL,
              choices = coc_bonus_opportunities %>% head(length(.)/2)
            ),
            checkboxGroupInput(
              ns("coc_bonus_types_2"),
              NULL,
              choices = coc_bonus_opportunities %>% tail(length(.)/2)
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

mod_funding_priorities_server <- function(id, user_coc) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
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
      hud_ard_report[coc == user_coc$coc] %>%
        fmutate(
          tier_2 = estimated * 0.1 + coc_bonus + dv_bonus,
          adjusted_ard = round(tier_1/0.9, 0),
          yhdp_ard = estimated - adjusted_ard,
          dv_ard = as.numeric(NA)
        ) %>%
        frename(estimated = "total_ard")
    })
    
    observe({
      req(user_coc$coc)
      lapply(ard_field_names, function(i) {
        updateNumericInput(
          session, 
          i, 
          value = hud_ard_coc_data()[[i]]
        )
        if(i != "dv_ard") shinyjs::disable(i)
      })
    })
    
    # Priorities table -----------------
    priorities_data <- reactiveVal(NULL)
    
    observe({
      req(user_coc$coc_version_id)
      
      # 1. Create the full, empty data structure for ALL possible populations.
      full_data <- pop_grp_toggles[, .(Population = full_text)]
      for(pt in main_project_types) {
        full_data[[paste0(pt, "_Beds")]] <- NA_real_
        full_data[[paste0(pt, "_Funding")]] <- NA_real_
        full_data[[paste0(pt, "_Priority")]] <- NA_character_
      }
      
      # 2. Fetch existing priorities from the DB
      coc_funding_priorities_from_db <- get_db_query(
        "SELECT * 
         FROM coc_funding_priorities 
         WHERE coc_version_id = $1 AND (beds IS NOT NULL OR funding IS NOT NULL or priority IS NOT NULL)",
        params = list(user_coc$coc_version_id)
      )
      
      # 3. If data exists in the DB, merge it into our full data template.
      if (nrow(coc_funding_priorities_from_db) > 0) {
        # You'll need to reshape your DB data from long to wide to match `full_data`.
        # This is a conceptual example using dcast from data.table.
        # Your column names (`project_type`, `beds`, etc.) might differ.
        
        # First, map the DB codes back to the `full_text` population name
        wide_db_data <- coc_funding_priorities_from_db %>%
          join(pop_grp_toggles, on = c("target_population" = "pop", "population_group" = "grp")) %>%
          # Reshape logic here... for example:
          # dcast(. ~ project_type, value.var = c("beds", "funding", "priority"))
          # This step is highly dependent on your DB schema and `main_project_types`
          
          # For now, let's assume `wide_db_data` has columns like "Population", "PH_Beds", etc.
          # We can then update the `full_data` table.
          # This is a robust way to update a data.table by joining.
          full_data[wide_db_data, on = "Population", names(wide_db_data) := mget(paste0("i.", names(wide_db_data)))]
      }

      # 4. Store the final, merged table in our reactiveVal.
      #    If no data was in the DB, this is just the empty template.
      priorities_data(full_data)
      
    }) # This observer only needs to run once. Consider adding `once = TRUE`.
    
    observe({
      # Wait until priorities_data() is populated.
      req(priorities_data())
      
      data <- priorities_data()
      
      # Check if any data exists across any of the editable columns
      # This checks if we started from a truly blank slate
      has_existing_data <- any(
        !is.na(data[, .SD, .SDcols = patterns("_Beds$|_Funding$|_Priority$")])
      )
      
      selected_populations <- if (has_existing_data) {
        # If data exists, select the populations (rows) that have any value
        rows_to_keep <- data[, rowSums(!is.na(as.data.frame(.SD))) > 0, 
                             .SDcols = patterns("_Beds$|_Funding$|_Priority$")]
        data[rows_to_keep, Population]
      } else {
        # <--- THIS IS THE KEY LOGIC FOR THE EMPTY CASE
        # If no data exists, apply the hardcoded default
        c("All Families", "All Individuals", "Single Youth")
      }
      
      updateCheckboxGroupInput(
        session,
        "population_toggles",
        selected = selected_populations
      )
    })
    
    output$priorities_table <- renderDT({
      # Require these two things to be ready before rendering
      req(priorities_data(), input$population_toggles)
      
      # Filter the full dataset based on the selected checkboxes
      data_to_display <- priorities_data()[Population %in% input$population_toggles]

      # Create the header structure
      datatable(
        data_to_display,
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
          pageLength = nrow(data_to_display),
          ordering = FALSE,
          searching = FALSE,
          info = FALSE
        )
      ) %>% formatStyle(
        columns = seq(4, ncol(data_to_display), by = 3),  # Priority columns (every 3rd column starting from 3)
        `border-right` = "1px solid black"
      )
    }, server = FALSE)
    
    # Toggle which target population + population group is to be prioritized
    # default is All Families, All Individuals, and Single Youth
    # priorities_data <- reactiveVal(
    #   data.table(
    #     Population = pop_grp_toggles,
    #     Enabled = TRUE,  # New column to track enabled/disabled state
    #     stringsAsFactors = FALSE
    #   )
    # )
    
    # priorities_table_proxy <- dataTableProxy(ns("priorities_table"))
    # observeEvent(input$population_toggles, {
    #   req(user_coc$coc)
    #   browser()
    #   replaceData(priorities_table_proxy, priorities_data())
    # }, ignoreInit = TRUE)
    
    # Update priorities data when cell is edited
    observeEvent(input$priorities_table_cell_edit, {
      info <- input$priorities_table_cell_edit
      current_data <- priorities_data()
      
      # Update the value
      # Get the population name from the row that was displayed
      # This is trickier because the view is filtered. We need to map the
      # viewed row index back to the full data index.
      displayed_data <- current_data[Population %in% isolate(input$population_toggles)]
      population_to_update <- displayed_data[info$row, Population]
      full_data_row_index <- which(current_data$Population == population_to_update)
      
      # Update the value in the full dataset
      # The column index is correct as is.
      current_data[full_data_row_index, info$col + 1] <- info$value

      # Save the updated data back to the reactiveVal
      priorities_data(current_data)
      data_has_changed(TRUE)
    })
    
    observe({
      # This code runs every 5 seconds (because it depends on the timer)
      auto_save_timer() 

      # Only proceed if data has actually changed
      if (data_has_changed()) {
        # Isolate the data to prevent reactive loops
        data_to_save <- isolate(priorities_data())
        long_data <- melt(
          copy(data_to_save),
          id.vars = "Population",
          measure.vars = patterns("_Beds$|_Funding$|_Priority$"),
          variable.name = "metric", # This will be an integer (1, 2, 3...)
          na.rm = TRUE                        # Still the most important optimization
        )
        
        # If melting results in an empty table (no values entered), stop.
        if (nrow(long_data) == 0) {
          data_has_changed(FALSE)
          return()
        }
        
        # Step 2: SPLIT the 'variable' column into project_type and metric.
        long_data[, c("project_type", "metric") := tstrsplit(metric, "_", fixed = TRUE)]
        
        # Step 3: DCAST to pivot the 'metric' values into new columns.
        # This is the key step you were missing. It creates the 'beds', 'funding',
        # and 'priority' columns.
        db_ready_data <- dcast(
          long_data,
          Population + project_type ~ tolower(metric), # formula: rows ~ columns_to_create
          value.var = "value"
        )
        
        # Step 4: JOIN and ADD METADATA using a final chain.
        # This part is cleaner when chained after the main reshaping is done.
        db_ready_data <- db_ready_data[
          pop_grp_toggles, on = c(Population = "full_text"), # Join to get DB population codes
          `:=`(target_population = i.pop, population_group = i.grp)
        ][, `:=`( # Add metadata columns for the query
          coc_version_id = user_coc$coc_version_id,
          created_by = user_coc$username,
          updated_by = user_coc$username,
          project_type = get_lookup_refid(project_type, "project_type")
        )]

        # Ensure final data types are correct before sending to the database.
        db_ready_data[, beds := ifelse("beds" %in% names(db_ready_data), as.integer(beds), NA)]
        db_ready_data[, funding := ifelse("funding" %in% names(db_ready_data), as.integer(funding), NA)]
        db_ready_data[, priority := ifelse("priority" %in% names(db_ready_data), as.integer(priority), NA)]

        # The "UPSERT" query
        sql_query <- "
          INSERT INTO coc_funding_priorities (coc_version_id, project_type, target_population, population_group, beds, funding, priority, created_by)
          VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
          ON CONFLICT (coc_version_id, project_type, target_population, population_group)
          DO UPDATE SET 
            beds = EXCLUDED.beds,
            funding = EXCLUDED.funding,
            priority = EXCLUDED.priority,
            updated_by = EXCLUDED.created_by, -- Use the 'created_by' value from the attempted insert
            updated_at = CURRENT_TIMESTAMP;
        "
        tryCatch({
          # Execute the query for each row of the long data frame
          # Using a prepared statement with `dbExecute` and `params` is safe from SQL injection
          apply(db_ready_data, 1, function(row) {
            dbExecute(
              DB_CON,
              sql_query,
              params = list(
                user_coc$coc_version_id,
                row[["project_type"]],
                row[["target_population"]],
                row[["population_group"]],
                row[["beds"]],
                row[["funding"]],
                row[["priority"]],
                user_coc$username
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
