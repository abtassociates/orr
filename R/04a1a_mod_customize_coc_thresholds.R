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
      uiOutput(ns("threshold_checkboxes_ui")) |> withSpinner(),
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
mod_customize_coc_thresholds_server <- function(id, user_coc, nav_control) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pull_thresholds_trigger <- reactiveVal(0)
    all_coc_thresholds <- reactiveVal()
    
    # Fetch currently selected thresholds for the active profile
    observe({
      req(user_coc$coc_version_id)
      req(pull_thresholds_trigger())

      all_coc_thresholds(get_db_query(
        "SELECT t.threshold_id, t.threshold_text, st.threshold_id IS NOT NULL AS selected, t.date_updated AS threshold_date_updated, st.date_updated AS selected_threshold_date_updated
        FROM thresholds t
        LEFT JOIN selected_coc_thresholds st ON t.threshold_id = st.threshold_id
        WHERE type = 'CoC' AND (t.coc_version_id = $1 OR t.coc_version_id IS NULL)",
        params = list(user_coc$coc_version_id)
      ))
    })
    
    # Dynamically render the checkboxes based on available and selected data
    output$threshold_checkboxes_ui <- renderUI({
      req(user_coc$coc_version_id)
      req(fnrow(all_coc_thresholds()) > 0)
      
      checkboxGroupInput(
        inputId = ns("threshold_selection"),
        label = "CoC Threshold Requirements",
        choices = setNames(
          all_coc_thresholds()$threshold_id, 
          all_coc_thresholds()$threshold_text
        ),
        width = "100%",
        selected = all_coc_thresholds()[selected == TRUE]$threshold_id
      )
    })
    
    # Save logic for threshold selections
    observe({
      req(input$save_thresholds)
      req(user_coc$coc_version_id, user_coc$username)
      
      sql_query <- "
          INSERT INTO selected_thresholds (threshold_id, coc_version_id, selected, created_by)
          VALUES ($1, $2, $3, $4)
          ON CONFLICT (coc_version_id, threshold_id)
          DO UPDATE SET 
            selected = EXCLUDED.selected,
            updated_by = EXCLUDED.created_by, -- Use the 'created_by' value from the attempted insert
            date_updated = CURRENT_TIMESTAMP
          WHERE date_updated = $5
        "
      params_list <- all_coc_thresholds() |>
        fmutate(username = user_coc$username) |>
        fselect(threshold_id, coc_version_id, selected, username) |>
        as.list() |>
        unname()
      
      tryCatch({
        z <- db_execute(sql_query, params = params_list)
        if(z == 0) {
          showNotification("Someone is editing these thresholds!", type = "error", duration = 3)
          pull_thresholds_trigger(pull_thresholds_trigger() + 1)
        } else if(z < fnrow(rv$selected_coc_thresholds)) {
          showNotification("Someone was editing one or more of these thresholds!", type = "error", duration = 3)
          pull_thresholds_trigger(pull_thresholds_trigger() + 1)
        } else {
          showNotification("Saved thresholds successfully!", type = "message", duration = 3)
        }
      }, error = function(e) {
        showNotification(paste("Error saving data:", e$message), type = "error", duration = 10)
        cat("Database save error:", e$message, "\n")
      })
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
      
      # 4189f53015d2fbf37310a3b47b8a764f79f1512d
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
      
      
    }) # end submit custom threhsold
    
  })
}
