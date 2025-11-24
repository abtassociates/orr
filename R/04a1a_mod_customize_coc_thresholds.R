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
      req(DB_CON)

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
            coc_version_id = user_coc$coc_version_id,
          ) %>% 
            add_user_stamp(user_coc, is_new = TRUE)
          DBI::dbAppendTable(DB_CON, "selected_coc_thresholds", add_df)
        }
        
        # Remove deselected items
        if (length(to_remove) > 0) {
          remove_q <- "DELETE FROM selected_coc_thresholds WHERE coc_version_id = $1 AND threshold_id = ANY($2)"
          dbExecute(DB_CON, remove_q, params = list(user_coc$coc_version_id, to_remove))
        }
        
        # Update reactive value to reflect saved state
        rv$selected_coc_thresholds <- current_selection
        
        shiny::showNotification("CoC Thresholds saved successfully.", type = "message")
      }, error = function(e) {
        shiny::showNotification(paste("Error saving thresholds:", e$message), type = "error")
      })
    }, ignoreInit = TRUE)
  })
}
