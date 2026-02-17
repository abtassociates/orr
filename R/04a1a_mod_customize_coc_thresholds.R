#-------------------------------------------------------------------------------
# 2. CoC Thresholds Sub-Module
# - This module allows users to select which CoC-specific thresholds apply.
# - Selections are saved to the `selected_coc_thresholds` table.
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
    
    rv <- reactiveValues(
      all_thresholds = data.table(),
      selected_coc_thresholds = c()
    )
    
    # Fetch all available CoC thresholds
    observe({
      req(DB_POOL)

      rv$all_thresholds <- get_db_query(
        "SELECT threshold_id, threshold_text 
        FROM thresholds
        WHERE type = 'CoC' ORDER BY threshold_id"
      )
    })
    
    # Fetch currently selected thresholds for the active profile
    observe({
      req(user_coc$coc_version_id)

      selected_q <- "SELECT threshold_id FROM selected_coc_thresholds WHERE coc_version_id = $1"
      selected_data <- get_db_query(selected_q, params = list(user_coc$coc_version_id))

      rv$selected_coc_thresholds <- selected_data$threshold_id
    })
    
    # Dynamically render the checkboxes based on available and selected data
    output$threshold_checkboxes_ui <- renderUI({
      req(nrow(rv$all_thresholds) > 0)
      
      checkboxGroupInput(
        inputId = ns("threshold_selection"),
        label = "CoC Threshold Requirements",
        choices = setNames(rv$all_thresholds$threshold_id, rv$all_thresholds$threshold_text),
        width = "100%",
        selected = rv$selected_coc_thresholds
      )
    })
    
    # Save logic for threshold selections
    observeEvent(input$save_thresholds, {
      req(user_coc$coc_version_id, user_coc$username)
      
      current_selection <- as.integer(input$threshold_selection)
      previous_selection <- rv$selected_coc_thresholds
      
      to_add <- setdiff(current_selection, previous_selection)
      to_remove <- setdiff(previous_selection, current_selection)
      
      tryCatch({
        # Add new selections
        if (length(to_add) > 0) {
          add_df <- data.frame(
            threshold_id = to_add,
            coc_version_id = user_coc$coc_version_id
          ) %>% 
            add_user_stamp(user_coc, is_new = TRUE)
          db_append("selected_coc_thresholds", add_df)
        }
        
        # Remove deselected items
        if (length(to_remove) > 0) {
          remove_q <- "DELETE FROM selected_coc_thresholds WHERE coc_version_id = $1 AND threshold_id = ANY($2)"
          db_execute(remove_q, params = list(user_cic$coc_version_id, to_remove))
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
          ON CONFLICT (coc_version_id, threshold_text)
          DO UPDATE SET
            type = EXCLUDED.type,
            coc_version_id = EXCLUDED.coc_version_id,
            threshold_text = EXCLUDED.threshold_text,
            updated_by = EXCLUDED.created_by, -- Use the 'created_by' value from the attempted insert
            date_updated = CURRENT_TIMESTAMP
          WHERE date_updated = $4
          RETURNING threshold_id;
        "

      current_date_updated_for_threshold <- all_coc_thresholds()[
        threshold_text == input$custom_threshold_text
      ]$threshold_date_updated
      
      # 4189f53015d2fbf37310a3b47b8a764f79f1512d
      params_list <- list(
        input$custom_threshold_text,
        user_coc$coc_version_id, 
        user_coc$username,
        if(length(current_date_updated_for_threshold) > 0) current_date_updated_for_threshold else NA
      ) |> unname()
      
      tryCatch({
        new_threshold_id <- DBI::dbGetQuery(DB_POOL, sql_query, params = params_list)$threshold_id
        if(length(new_threshold_id) == 0) {
          showNotification("Someone is editing these thresholds!", type = "error", duration = 3)
          pull_thresholds_trigger(pull_thresholds_trigger() + 1)
        } else {
          # update all_coc_thresholds
          all_coc_thresholds(
            rbindlist(list(
              all_coc_thresholds(), 
              data.table(
                threshold_id = new_threshold_id,
                threshold_text = input$custom_threshold_text,
                selected = TRUE,
                threshold_date_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                selected_threshold_date_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
              )
            ))
          )

          showNotification("Saved thresholds successfully!", type = "message", duration = 3)
        }
      }, error = function(e) {
        showNotification(paste("Error saving data:", e$message), type = "error", duration = 10)
        cat("Database save error:", e$message, "\n")
      })
      
      
    }) # end submit custom threhsold
    
  })
}
