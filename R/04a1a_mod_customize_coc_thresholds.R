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
    
    refresh_trigger <- reactiveVal(0)
    all_coc_thresholds <- reactiveVal()
    updating_from_db <- reactiveVal(FALSE)
    
    get_all_coc_thresholds <- function(coc_version_id) {
      get_db_query(
        "SELECT t.threshold_id, t.threshold_text, st.selected, t.date_updated AS threshold_date_updated, st.date_updated AS selected_threshold_date_updated
        FROM thresholds t
        LEFT JOIN selected_thresholds st ON t.threshold_id = st.threshold_id AND st.coc_version_id = $1
        WHERE t.type = 'CoC' AND 
          (t.coc_version_id = $1 OR t.coc_version_id IS NULL)
        ORDER BY t.threshold_id",
        params = list(coc_version_id)
      ) |>
        fmutate(
          selected = allNA(selected) | selected
        )
    }
    
    # Fetch currently selected thresholds for the active profile
    observe({
      req(user_coc$coc_version_id)
      req(refresh_trigger())
      
      updating_from_db(TRUE)
      
      data <- get_all_coc_thresholds(user_coc$coc_version_id)
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
    
    # Handle changes to the thresholds shown to the user
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
    update_selected_thresholds_db <- function(p, updated_selected_thresholds) {
      save_to_db(
        p,
        "INSERT INTO selected_thresholds (threshold_id, coc_version_id, selected, created_by)
          VALUES ($1, $2, $3, $4)
          ON CONFLICT (coc_version_id, threshold_id) DO UPDATE SET 
            selected = EXCLUDED.selected,
            updated_by = EXCLUDED.created_by, -- Use the 'created_by' value from the attempted insert
        " |> add_optimistic_locking(),
        updated_selected_thresholds,
        "selected_thresholds"
      )
    }
    
    get_updated_selected_thresholds <- function(params) {
      all_coc_thresholds() |>
        fmutate(
          coc_version_id = params$coc_version_id,
          username = params$username
        ) |>
        fselect(threshold_id, coc_version_id, selected, username, selected_threshold_date_updated)
    }
    
    observeEvent(input$save_thresholds, {
      req(user_coc$coc_version_id) 
      
      updated_selected_thresholds <- get_updated_selected_thresholds(
        list(
          thresholds = all_coc_thresholds(),
          coc_version_id = user_coc$coc_version_id,
          username = user_coc$username
        )
      )
      
      needs_refresh1 <- update_selected_thresholds_db(DB_POOL, updated_selected_thresholds)
      
      if(needs_refresh1)
        refresh_trigger(\(x) x + 1)
      
    }, ignoreInit = TRUE) # end save_thresholds observeEvent
    
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
    
    update_thresholds_db <- function(p, updated_thresholds) {
      save_to_db(
        p,
        "INSERT INTO thresholds (type, coc_version_id, threshold_text, created_by)
          VALUES ('CoC', $1, $2, $3)
          ON CONFLICT (coc_version_id, threshold_text) DO UPDATE SET
            updated_by = EXCLUDED.created_by
        " |> 
          add_optimistic_locking() |>
          cat("\nRETURNING threshold_id, coc_version_id"),
        updated_thresholds(),
        "thresholds"
      )
    }
    
    observeEvent(input$submit_custom_threshold, {
      removeModal()
      req(nchar(trimws(input$custom_threshold_text)) > 0)
      
      current_date_updated_for_threshold <- all_coc_thresholds()[ 
        threshold_text == input$custom_threshold_text
      ]$threshold_date_updated
      
      updated_custom_threshold <- list(
        user_coc$coc_version_id, 
        input$custom_threshold_text,
        user_coc$username,
        if (length(current_date_updated_for_threshold) > 0) current_date_updated_for_threshold else NA
      )
      
      # These don't need to be tied together, but it avoids dealing with 2 different conflicts
      updated_custom_threshold_info <- NULL
      pool::poolWithTransaction(DB_POOL, function(p) {
        updated_custom_threshold_info <- update_thresholds_db(p, updated_custom_threshold)
        
        updated_selected_thresholds <- updated_custom_threshold_info |>
          fmutate(
            selected = TRUE,
            created_by = user_coc$username
          ) |>
          fselect(threshold_id, coc_version_id, selected, created_by, date_updated)

        needs_refresh2 <- update_selected_thresholds_db(p, updated_selected_thresholds)
      })
      
      refresh_trigger(\(x) x + 1)
      
    }) # end submit custom threhsold
    module_returns$selected_thresholds <- input$threshold_checkboxes
  })
}
