# UI modules for project rating
mod_thresholds_entry_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Threshold Entry",
    value = id,
    card(
      div(
        id=ns("empty"),
        helpText("Select a project in the left-hand sidebar to begin rating")
      ),
      accordion(
        id = ns("reqs"),
        accordion_panel(
          "HUD Requirements",
          checkboxInput(
            ns("yes_to_all_HUD"),
            "Met all HUD requirements"
          ),
          checkboxGroupInput(
            ns("HUD_requirements"),
            label = NULL,
            choices = setNames(
              HUD_THRESHOLD_REQUIREMENTS$threshold_id, 
              HUD_THRESHOLD_REQUIREMENTS$threshold_text
            ),
            width = "100%"
          )
        ),
        accordion_panel(
          "CoC Requirements",
          checkboxInput(
            ns("yes_to_all_CoC"),
            "Met all CoC requirements"
          ),
          checkboxGroupInput(
            ns("CoC_requirements"),
            label = NULL,
            width = "100%"
          )
        ),
        open = FALSE
      ), # end accordion
      card_footer(
        style = "display: flex; justify-content: space-between; align-items: center;",
        div(),
        actionButton(ns("save_requirements"), "Save Thresholds", icon = icon("save"), class="btn-primary")
      )
    ) # end card
  ) # end nav_panel
}

mod_thresholds_entry_server <- function(id, user_coc, selected_project) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # updating_from_db <- reactiveVal(NA)
    thresholds_to_enter <- reactiveVal(NULL)
    project_evaluation <- reactiveVal(NULL)
    refresh_trigger <- reactiveVal(0)
    
    coc_thresholds_to_enter <- reactive({
      req(thresholds_to_enter())
      thresholds_to_enter() |>
        fsubset(type == "CoC" & selected)
    })
    
    # UI updates based on project selection
    observeEvent(selected_project(), {
      project_is_selected <- isTruthy(fnrow(selected_project()) > 0)
      
      shinyjs::toggleState("HUD_requirements", condition = project_is_selected)
      shinyjs::toggleState("CoC_requirements", condition = project_is_selected)
      
      shinyjs::toggleState("yes_to_all_HUD", condition = project_is_selected)
      shinyjs::toggleState("yes_to_all_CoC", condition = project_is_selected)
      
      shinyjs::toggle("empty", condition = !project_is_selected)
      # shinyjs::toggleState(selector = glue::glue("#{ns('reqs')} .accordion-button"), condition = project_is_selected)
      shinyjs::toggleState("save_requirements", condition = project_is_selected)
      
      if(project_is_selected) {
        bslib::accordion_panel_open("reqs", "HUD Requirements", session = session)
        bslib::accordion_panel_open("reqs", "CoC Requirements", session = session)
      }
    }, ignoreNULL = FALSE)
    
    # Updating main data
    observeEvent(c(selected_project(), refresh_trigger(), user_coc$customized_coc_thresholds_updated), {
      req(selected_project())
      
      project_is_selected <- isTruthy(fnrow(selected_project()) > 0)
      
      thresholds_to_enter(
        get_all_thresholds_to_enter(
          user_coc$coc_version_id, 
          if(!project_is_selected) NA else selected_project()$project_id
        )
      )
      
      project_evaluation(
        get_project_evaluation(
          user_coc$coc_version_id, 
          if(!project_is_selected) NA else selected_project()$project_id
        )
      )
      
      hud_reqs_met <- thresholds_to_enter()[type == "HUD" & met_threshold]
      updateCheckboxGroupInput(
        session,
        "HUD_requirements",
        selected = if(fnrow(hud_reqs_met) > 0) hud_reqs_met$threshold_id else character(0)
      )
      
      coc_reqs_met <- coc_thresholds_to_enter()[met_threshold == TRUE]
      updateCheckboxGroupInput(
        session,
        "CoC_requirements",
        choices = if(is.null(coc_thresholds_to_enter())) {
          character(0)
        } else {
          setNames(
            coc_thresholds_to_enter()$threshold_id,
            coc_thresholds_to_enter()$threshold_text
          )
        },
        selected = if(fnrow(coc_reqs_met) > 0) coc_reqs_met$threshold_id else character(0)
      )
      
      # Need a delay to allow choices to re-render
      shinyjs::delay(100, {
        shinyjs::toggleState("CoC_requirements", condition = project_is_selected)
      })
      
      shinyjs::toggle("yes_to_all_CoC", condition = isTruthy(fnrow(coc_thresholds_to_enter()) > 0))
    }, ignoreNULL = FALSE, priority = 1)
    
    lapply(c("HUD", "CoC"), function(ttype) {
      
      # 1. When individual checkboxes are clicked...
      observeEvent(input[[paste0(ttype, "_requirements")]], {
        req(user_coc$coc_version_id, selected_project(), thresholds_to_enter())
        
        all_options <- thresholds_to_enter()[type == ttype, threshold_id]
        current_selected <- input[[paste0(ttype, "_requirements")]]
        
        should_be_yes_to_all <- setequal(all_options, current_selected)
        current_yes_to_all_state <- isTRUE(input[[paste0("yes_to_all_", ttype)]])
        
        # Update "Yes to All" only if it doesn't match what it should be
        if (should_be_yes_to_all != current_yes_to_all_state) {
          updateCheckboxInput(
            session,
            paste0("yes_to_all_", ttype),
            value = should_be_yes_to_all
          )
        }
      }, ignoreNULL = FALSE, ignoreInit = TRUE) 
      
      
      # 2. When the "Yes to All" checkbox is clicked...
      observeEvent(input[[paste0("yes_to_all_", ttype)]], {
        req(user_coc$coc_version_id, selected_project())
        
        all_options <- thresholds_to_enter()[type == ttype, threshold_id]
        current_selected <- input[[paste0(ttype, "_requirements")]]
        is_yes_to_all_checked <- isTRUE(input[[paste0("yes_to_all_", ttype)]])
        
        # Determine what the group SHOULD look like
        if (is_yes_to_all_checked) {
          
          target_group_state <- all_options
          
        } else {
          # If Yes-to-all is unchecked, we must figure out WHY:
          # Did the user click "Yes to all" to uncheck it? 
          # Or did the system uncheck it because the user unchecked a single box?
          
          if (setequal(current_selected, all_options)) {
            # The group is completely full. This means the user explicitly 
            # clicked the "Yes to all" box to wipe the list.
            target_group_state <- character(0)
          } else {
            # The group is ALREADY missing items. This means the system triggered this 
            # event to sync the UI. We should leave the group exactly as it is.
            target_group_state <- current_selected 
          }
        }
        
        # Update the group only if it doesn't already match the target
        if (!setequal(current_selected, target_group_state)) {
          updateCheckboxGroupInput(
            session,
            paste0(ttype, "_requirements"),
            selected = target_group_state
          )
        }
      }, ignoreNULL = FALSE, ignoreInit = TRUE)
      
    })
    
    # --- Saving to db ---------------
    get_thresholds_to_enter <- function(params) {
      params$thresholds |>
        fmutate(
          created_by = params$username,
          met_threshold_new = threshold_id %in% c(params$HUD_requirements, params$CoC_requirements),
          project_id = params$project_id
        ) |>
        fsubset(met_threshold_new != fcoalesce(as.logical(met_threshold), FALSE)) |>
        fselect(project_id, threshold_id, met_threshold_new, created_by, version_id)
    }
    get_updated_project_evaluation <- function(params) {
      data.table(
        project_id = params$project_id,
        met_HUD_thresholds = params$met_all_HUD_requirements,
        met_CoC_thresholds = params$met_all_CoC_requirements,
        created_by = params$username,
        version_id = params$version_id
      ) |>
        fselect(project_id, met_HUD_thresholds, met_CoC_thresholds, created_by, version_id)
    }
    
    observeEvent(input$save_requirements, {
      req(user_coc$coc_version_id, user_coc$username)
      
      updated_thresholds <- get_thresholds_to_enter(
        list(
          thresholds = thresholds_to_enter(),
          username = user_coc$username,
          project_id = selected_project()$project_id,
          HUD_requirements = input$HUD_requirements,
          CoC_requirements = input$CoC_requirements
        )
      )
      
      updated_project_evaluation <- get_updated_project_evaluation(
        list(
          project_id =  selected_project()$project_id,
          met_all_HUD_requirements = input$yes_to_all_HUD,
          met_all_CoC_requirements = input$yes_to_all_CoC,
          username = user_coc$username,
          version_id = ifelse(
            is_empty(project_evaluation()$version_id), 
            NA, 
            project_evaluation()$version_id
          )
        )
      )
      
      needs_refresh1 <- FALSE
      needs_refresh2 <- FALSE

      pool::poolWithTransaction(get_db_pool(), function(p) {
        needs_refresh1 <- update_threshold_entries_db(p, updated_thresholds)
        needs_refresh2 <- update_threshold_project_evaluation_db(p, updated_project_evaluation)
      })
      
      # if(needs_refresh1 || needs_refresh2)
        refresh_trigger(\(x) x + 1)
      
    }, ignoreInit = TRUE) # end save_requirements
  }) # end moduleServer
}
