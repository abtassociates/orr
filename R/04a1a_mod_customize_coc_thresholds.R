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
      mod_user_presence_ui(ns("presence")),
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
mod_customize_coc_thresholds_server <- function(id, user_coc, nav_control, active) {
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
    
    # ------ Auto-Save ------------
    ## Get thresholds tha thave changes ---
    thresholds_to_save <- reactive({
      req(user_coc$coc_version_id, fnrow(all_coc_thresholds()) > 0)
      
      all_coc_thresholds() |>
        fmutate(
          coc_version_id = user_coc$coc_version_id,
          username = user_coc$username,
          selected_to_save = threshold_id %in% input$threshold_checkboxes
        ) |>
        fsubset(selected_to_save != selected) |>
        fselect(threshold_id, coc_version_id, selected_to_save, username, selected_threshold_version_id)
    }) |> debounce(2000)
    
    observeEvent(thresholds_to_save(), {
      to_save <- thresholds_to_save()
      req(to_save)
      req(fnrow(to_save) > 0)
      
      needs_refresh1 <- update_selected_thresholds_db(get_db_pool(), to_save)
      
      if(!needs_refresh1) {
        all_coc_thresholds()[
          to_save, 
          on = "threshold_id", 
          `:=`(
            selected = as.integer(i.selected_to_save),
            version_id = i.selected_threshold_version_id + 1
          )
        ]
        
        user_coc$customized_coc_thresholds_updated <- user_coc$customized_coc_thresholds_updated + 1
      } else {
        refresh_trigger(refresh_trigger() + 1)
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE, label = "CUSTOM_observe_threshold_to_save")
    
    
    # ---- Custom Threshold------------
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
    
    # -- User Presence ---
    mod_user_presence_server(
      id = "presence",
      user_coc = user_coc,
      # Record is the CoC Version
      record_id = reactive({ user_coc$coc_version_id }),
      active = active
    )
  })
}
