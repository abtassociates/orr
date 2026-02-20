#-------------------------------------------------------------------------------
# 2. CoC Thresholds Sub-Module
# - This module allows users to select which CoC-specific thresholds apply.
# - Selections are saved to the `selected_thresholds` table.
#-------------------------------------------------------------------------------

#' @title mod_coc_thresholds_ui
#' @noRd
mod_customize_coc_thresholds_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "CoC Thresholds Requirements",
    value = id,
    card(
      em("Select the CoC Thresholds that all projects must meet to be considered for funding. HUD Thresholds are mandatory and not shown here."),
      uiOutput(ns("threshold_checkboxes_ui")),
      card_footer(
        style = "display: flex; justify-content: space-between; align-items: center;",
        actionButton(ns("add_threshold_btn"), "Add Custom Threshold", icon = icon("plus")),
        actionButton(ns("save_thresholds"), "Save CoC Threshold Selections", icon = icon("save"), class = "btn-primary")
      )
    )
  )
}

#' @title mod_coc_thresholds_server
#' @noRd
mod_customize_coc_thresholds_server <- function(id, user_coc, nav_control, module_returns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pull_thresholds_trigger <- reactiveVal(0)
    all_coc_thresholds <- reactiveVal()
    updating_from_db <- reactiveVal(FALSE)
    
    # Fetch currently selected thresholds for the active profile
    observe({
      req(user_coc$coc_version_id)
      req(pull_thresholds_trigger())

      data <- get_db_query(
        "SELECT t.threshold_id, t.threshold_text, st.selected, t.date_updated AS threshold_date_updated, st.date_updated AS selected_threshold_date_updated
        FROM thresholds t
        LEFT JOIN selected_thresholds st ON t.threshold_id = st.threshold_id
        WHERE type = 'CoC' AND (t.coc_version_id = $1 OR t.coc_version_id IS NULL)
        ORDER BY t.threshold_id",
        params = list(user_coc$coc_version_id)
      )
      
      updating_from_db(TRUE)
      all_coc_thresholds(data)
      
      updateCheckboxGroupInput(
        session,
        inputId = "threshold_checkboxes",
        choices = setNames(data$threshold_id, data$threshold_text),
        selected = data[selected == 1]$threshold_id
      )
      
      shiny::isolate(updating_from_db(FALSE))
    })
    
    # Dynamically render the checkboxes based on available and selected data
    output$threshold_checkboxes_ui <- renderUI({
      req(user_coc$coc_version_id)
      req(fnrow(all_coc_thresholds()) > 0)
      
      checkboxGroupInput(
        inputId = ns("threshold_checkboxes"),
        label = "CoC Threshold Requirements",
        choices = setNames(
          all_coc_thresholds()$threshold_id, 
          all_coc_thresholds()$threshold_text
        ),
        width = "100%",
        selected = all_coc_thresholds()[selected == TRUE]$threshold_id
      )
    })
    
    observeEvent(input$threshold_checkboxes, {
      req(!isolate(updating_from_db()))
      req(!is.null(isolate(all_coc_thresholds())))
    
      all_coc_thresholds(
        isolate(all_coc_thresholds()) |>
          fmutate(
            selected = fifelse(
              threshold_id %in% as.integer(input$threshold_checkboxes), 1, 0
            )
          )
      )
    }, ignoreNULL = FALSE)
    
    # Save logic for threshold selections
    save_selected_thresholds <- function() {
      sql_query <- "
          INSERT INTO selected_thresholds (threshold_id, coc_version_id, selected, created_by)
          VALUES ($1, $2, $3, $4)
          ON CONFLICT (coc_version_id, threshold_id) DO UPDATE SET 
            selected = EXCLUDED.selected,
            updated_by = EXCLUDED.created_by, -- Use the 'created_by' value from the attempted insert
            date_updated = $5
          WHERE date_updated = $6
        "

      timestamp <- get_db_timestamp()
      params_list <- all_coc_thresholds() |>
        fmutate(
          coc_version_id = user_coc$coc_version_id,
          username = user_coc$username,
          new_date_updated = timestamp
        ) |>
        fselect(threshold_id, coc_version_id, selected, username, new_date_updated, selected_threshold_date_updated) |>
        as.list() |>
        unname()
      
      tryCatch({
        z <- db_execute(sql_query, params = params_list)

        if(z == 0) {
          showNotification("Someone is editing these thresholds!", type = "error", duration = 3)
          pull_thresholds_trigger(pull_thresholds_trigger() + 1)
        } else if(z < fnrow(all_coc_thresholds())) {
          showNotification("Someone was editing one or more of these thresholds!", type = "error", duration = 3)
          pull_thresholds_trigger(pull_thresholds_trigger() + 1)
        } else {
          showNotification("Saved thresholds successfully!", type = "message", duration = 3)

          all_coc_thresholds(
            all_coc_thresholds() |> fmutate(selected_threshold_date_updated = timestamp)
          )
        }
      }, error = function(e) {
        browser()
        showNotification(paste("Error saving data:", e$message), type = "error", duration = 10)
        cat("Select thresholds save error:", e$message, "\n")
      })
    }
    observeEvent(input$save_thresholds, {
      req(user_coc$coc_version_id) 
      save_selected_thresholds()
    }) # end save_thresholds observeEvent
    
    observeEvent(input$add_threshold_btn, {
      showModal(
        modalDialog(
          title = "Additional Threshold Requirement",
          textInput(ns("custom_threshold_text"), "Please enter the new requirement text:", ""),
          footer = tagList(
            actionButton(ns("submit_custom_threshold"), "Submit"),
            modalButton("Cancel")
          )
        )
      )
    })
    
    observeEvent(input$submit_custom_threshold, {
      removeModal()
      req(nchar(trimws(input$custom_threshold_text)) > 0)
      
      # update thresholds table with new custom factor
      sql_query <- "
          INSERT INTO thresholds (type, coc_version_id, threshold_text, created_by)
          VALUES ('CoC', $1, $2, $3)
          ON CONFLICT (coc_version_id, threshold_text) DO UPDATE SET
            updated_by = EXCLUDED.created_by,
            date_updated = $4
          WHERE (thresholds.date_updated = $5 OR ($5 IS NULL AND thresholds.date_updated IS NULL))
          RETURNING threshold_id, date_updated;
        "

      current_date_updated_for_threshold <- all_coc_thresholds()[
        threshold_text == input$custom_threshold_text
      ]$threshold_date_updated

      new_timestamp <- get_db_timestamp()
      params_list <- list(
        user_coc$coc_version_id, 
        input$custom_threshold_text,
        user_coc$username,
        new_timestamp,
        if (length(current_date_updated_for_threshold) > 0) current_date_updated_for_threshold else NA
      )
      
      new_threshold <- tryCatch(
        DBI::dbGetQuery(DB_POOL, sql_query, params = params_list),
        error = function(e) {
          showNotification(paste("Error adding threshold:", e$message), type = "error", duration = 10)
          cat("Save custom threshold error:", e$message, "\n")
          NULL
        }
      )
      
      req(!is.null(new_threshold))
      
      if(nrow(new_threshold) == 0) {
        showNotification("Someone is editing these thresholds!", type = "error", duration = 3)
        pull_thresholds_trigger(isolate(pull_thresholds_trigger()) + 1)
        return()
      } 
      
      new_row <- data.table(
        threshold_id = new_threshold$threshold_id,
        threshold_text = trimws(input$custom_threshold_text),
        selected = 1,
        threshold_date_updated = new_threshold$date_updated,
        selected_threshold_date_updated = new_threshold$date_updated
      )
      
      
      updated <- rbindlist(list(isolate(all_coc_thresholds()), new_row))
      all_coc_thresholds(updated)
      
      # Update checkbox UI to include new option (pre-checked)
      updating_from_db(TRUE)
      updateCheckboxGroupInput(
        session,
        inputId = "threshold_checkboxes",
        choices = setNames(updated$threshold_id, updated$threshold_text),
        selected = updated[selected == 1L]$threshold_id
      )
      isolate(updating_from_db(FALSE))
      
      # Auto-save selected_thresholds (includes new threshold + any pending checkbox changes)
      save_selected_thresholds()
    }) # end submit custom threhsold
    module_returns$selected_thresholds <- input$threshold_checkboxes
  })
}
