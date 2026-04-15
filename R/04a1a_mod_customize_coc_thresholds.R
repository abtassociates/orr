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
      checkboxGroupInput(
        inputId = ns("threshold_checkboxes"),
        label = "CoC Threshold Requirements",
        choices = NULL,
        width = "100%",
        selected = NULL
      ),
      card_footer(
        style = "display: flex; justify-content: space-between; align-items: center;",
        actionButton(ns("add_threshold_btn"), "Add Custom Threshold", icon = icon("plus"))#,
        # actionButton(ns("save_thresholds"), "Save CoC Threshold Selections", icon = icon("save"), class = "btn-primary")
      )
    )
  )
}

#' @title mod_coc_thresholds_server
#' @noRd
mod_customize_coc_thresholds_server <- function(id, user_coc, nav_control) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    refresh_trigger <- reactiveVal(0)
    all_coc_thresholds <- reactiveVal()
    
    # Fetch currently selected thresholds for the active profile
    observe({
      req(user_coc$coc_version_id)
      req(refresh_trigger())
      
      data <- get_all_coc_thresholds(user_coc$coc_version_id)
      all_coc_thresholds(data)
      
      updateCheckboxGroupInput(
        session,
        inputId = "threshold_checkboxes",
        choices = setNames(data$threshold_id, data$threshold_text),
        selected = data[selected == 1]$threshold_id
      )
    })
    
    # Save logic for threshold selections
    get_updated_selected_thresholds <- function(params) {
      all_coc_thresholds() |>
        fmutate(
          coc_version_id = params$coc_version_id,
          username = params$username,
          selected_new = threshold_id %in% params$selected_threshold_ids
        ) |>
        fsubset(selected_new != fcoalesce(as.logical(selected), FALSE)) |>
        fselect(threshold_id, coc_version_id, selected_new, username, selected_threshold_version_id)
    }
    
    save_thresholds <- function() {
      updated_selected_thresholds <- get_updated_selected_thresholds(
        list(
          thresholds = all_coc_thresholds(),
          selected_threshold_ids = input$threshold_checkboxes,
          coc_version_id = user_coc$coc_version_id,
          username = user_coc$username
        )
      )
      
      needs_refresh1 <- update_selected_thresholds_db(get_db_pool(), updated_selected_thresholds)
      
      # if(needs_refresh1)
      refresh_trigger(refresh_trigger() + 1)
      
      if(!needs_refresh1)
        user_coc$customized_coc_thresholds_updated <- user_coc$customized_coc_thresholds_updated + 1
    }
    
    
    # Handle changes to the thresholds shown to the user
    selected_thresholds <- reactive({
      req(user_coc$coc_version_id)
      input$threshold_checkboxes}) %>% debounce(1000)
    
    observeEvent(selected_thresholds(), {
      req(!identical(selected_thresholds(), as.character(all_coc_thresholds()[selected == T]$threshold_id)))
      
      save_thresholds()
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    
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
    
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("custom_threshold_text", sv_required())
    iv$add_rule("custom_threshold_text", ~ if(. %in% all_coc_thresholds()$threshold_text) "Threshold text must be unique.")
    
    observeEvent(input$submit_custom_threshold, {
      iv$enable()
      req(iv$is_valid())
      removeModal()
      req(nchar(trimws(input$custom_threshold_text)) > 0)
      
      current_version_id_for_threshold <- all_coc_thresholds()[ 
        threshold_text == input$custom_threshold_text
      ]$threshold_version_id
      
      updated_custom_threshold <- list(
        user_coc$coc_version_id, 
        input$custom_threshold_text,
        user_coc$username,
        if (length(current_version_id_for_threshold) > 0) current_version_id_for_threshold else NA
      )
      
      # These don't need to be tied together, but it avoids dealing with 2 different conflicts
      updated_custom_threshold_info <- NULL
      needs_refresh2 <- FALSE
      pool::poolWithTransaction(get_db_pool(), function(p) {
        updated_custom_threshold_info <- update_thresholds_db(p, updated_custom_threshold)
        
        updated_selected_thresholds <- updated_custom_threshold_info |>
          fmutate(
            selected = TRUE,
            created_by = user_coc$username
          ) |>
          fselect(threshold_id, coc_version_id, selected, created_by, version_id)

        needs_refresh2 <- update_selected_thresholds_db(p, updated_selected_thresholds)
      })
      
      refresh_trigger(refresh_trigger() + 1)
      
      if(!needs_refresh2)
        user_coc$customized_coc_thresholds_updated <- user_coc$customized_coc_thresholds_updated + 1
    }) # end submit custom threhsold
  })
}
