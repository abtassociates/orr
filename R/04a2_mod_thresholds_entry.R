# UI modules for project rating
mod_thresholds_entry_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Threshold Entry",
    value = id,
    card(
      textOutput(ns("empty")),
      style = "overflow: visible !important;",
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
            choiceValues = HUD_THRESHOLD_REQUIREMENTS$threshold_id, 
            choiceNames = lapply(HUD_THRESHOLD_REQUIREMENTS$threshold_text, HTML),
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
        class = "sticky-footer",
        style = "display: flex; justify-content: space-between; align-items: center;",
        prettySwitch(ns("threshold_complete"), label = "Threshold Complete?")
      )
    ) # end card
  ) # end nav_panel
}

mod_thresholds_entry_server <- function(id, user_coc, selected_project, active) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # updating_from_db <- reactiveVal(NA)
    thresholds_to_enter <- reactiveVal(NULL)
    project_evaluation <- reactiveVal(NULL)
    refresh_trigger <- reactiveVal(0)
    made_a_change <- reactiveVal(FALSE)
    
    coc_thresholds_to_enter <- reactive({
      req(thresholds_to_enter())
      thresholds_to_enter() |>
        fsubset(type == "CoC" & selected)
    })
    
    output$empty <- renderText({
      project_is_selected <- isTruthy(fnrow(selected_project()) > 0)
      
      shiny::validate(need(
        project_is_selected,
        "Select a project in the left-hand sidebar to begin rating"
      ))
    })
    
    # UI updates based on project selection
    observeEvent(selected_project(), {
      project_is_selected <- isTruthy(fnrow(selected_project()) > 0)
      
      shinyjs::toggleState("HUD_requirements", condition = project_is_selected)
      shinyjs::toggleState("CoC_requirements", condition = project_is_selected)
      
      shinyjs::toggleState("yes_to_all_HUD", condition = project_is_selected)
      shinyjs::toggleState("yes_to_all_CoC", condition = project_is_selected)
      
      # shinyjs::toggleState(selector = glue::glue("#{ns('reqs')} .accordion-button"), condition = project_is_selected)
      shinyjs::toggleState("save_requirements", condition = project_is_selected)
      
      # if(project_is_selected) {
      #   bslib::accordion_panel_open("reqs", "HUD Requirements", session = session)
      #   bslib::accordion_panel_open("reqs", "CoC Requirements", session = session)
      # } else {
      #   bslib::accordion_panel_close("reqs", "HUD Requirements", session = session)
      #   bslib::accordion_panel_close("reqs", "CoC Requirements", session = session)
      # }
    }, ignoreNULL = FALSE)
    
    # Updating main data
    observeEvent(c(selected_project(), refresh_trigger(), user_coc$customized_coc_thresholds_updated), {
      project_is_selected <- isTruthy(fnrow(selected_project()) > 0)
      project_id <- selected_project()$project_id
      
      thresholds_to_enter(
        get_all_thresholds_to_enter(user_coc$coc_version_id, if(!project_is_selected) NA else project_id)
      )
      
      if(project_is_selected)
        project_evaluation(
          get_project_evaluation(user_coc$coc_version_id, project_id)
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
      
      updatePrettySwitch(
        session,
        "threshold_complete", 
        value = project_evaluation()$threshold_complete == 1
      )
      shinyjs::toggleState("threshold_complete", condition = !allNA(thresholds_to_enter()$met_threshold))
      
      made_a_change(FALSE)
    }, ignoreNULL = FALSE, priority = 11)
    
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
        
        made_a_change(TRUE)
      }, ignoreNULL = FALSE, ignoreInit = TRUE, priority = 10) 
      
      
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
        
        made_a_change(TRUE)
      }, ignoreNULL = FALSE, ignoreInit = TRUE, priority = 9)
      
    }) # end back-and-forth ind. and yes-to-all checkbox observeEvents
    
    # --- Saving to db ---------------
    # runs whenever user (un)checks a requirement
    threshold_entries_to_save <- reactive({
      req(c(input$HUD_requirements, input$CoC_requirements, input$yes_to_all_HUD, input$yes_to_all_CoC, made_a_change()))
      req(thresholds_to_enter(), fnrow(selected_project()) > 0)
      
      # 1. Diff the checkboxes against the baseline
      updated_thresholds <- get_threshold_data_to_save(thresholds_to_enter(), "threshold_id", "met_threshold", c(input$HUD_requirements, input$CoC_requirements))
      if(is.null(updated_thresholds)) return(NULL)
      
      updated_thresholds <- updated_thresholds |>
        fmutate(
          created_by = user_coc$username,
          project_id = selected_project()$project_id
        ) |>
        fselect(project_id, threshold_id, met_threshold, created_by, version_id)
      
      # 2. Diff the project evaluations against the baseline
      eval_changed <- is_empty(project_evaluation()) || 
        !identical(isTRUE(project_evaluation()$met_HUD_thresholds), isTRUE(input$yes_to_all_HUD)) ||
        !identical(isTRUE(project_evaluation()$met_CoC_thresholds), isTRUE(input$yes_to_all_CoC))
     
      updated_project_evaluation <-  if (eval_changed) {
        data.table(
          project_id = selected_project()$project_id,
          met_HUD_thresholds = isTRUE(input$yes_to_all_HUD),
          met_CoC_thresholds = isTRUE(input$yes_to_all_CoC),
          created_by = user_coc$username,
          version_id = if(is_empty(project_evaluation()$version_id)) NA_integer_ else project_evaluation()$version_id
        )
      } 
      
      list(thresholds = updated_thresholds, project_evaluation = updated_project_evaluation)
    }, label = "te_changes_to_save") |> debounce(2000)
    
    observeEvent(threshold_entries_to_save(), {
      to_save <- threshold_entries_to_save()
      req(to_save)
      
      # Execute transaction and correctly extract flags out of the local scope
      refresh_flags <- pool::poolWithTransaction(get_db_pool(), function(p) {
        needs_ref1 <- update_threshold_entries_db(p, to_save$thresholds)
        
        needs_ref2 <- FALSE
        if (!is.null(to_save$project_evaluation))
          needs_ref2 <- update_threshold_project_evaluation_db(p, to_save$project_evaluation)
        
        return(c(needs_ref1, needs_ref2))
      })
      
      needs_refresh <- any(unlist(refresh_flags))
      
      if (!needs_refresh) {
        # SUCCESS: The "Silent Update" to the baselines
        # 1. Update thresholds_to_enter baseline
        thresholds_to_enter()[
          to_save$thresholds, 
          on = "threshold_id", 
          `:=`(
            met_threshold = as.integer(met_threshold),
            version_id = fcoalesce(version_id, 0L) + 1
          )
        ]
        
        # 2. Update project_evaluation baseline
        if (!is.null(to_save$project_evaluation))
          project_evaluation(
            to_save$project_evaluation |>
              fmutate(version_id = version_id + 1)
          )
        
        shinyjs::toggleState("threshold_complete", condition = !allNA(thresholds_to_enter()$met_threshold))
      } else {
        # COLLISION: Trigger full refresh
        refresh_trigger(refresh_trigger() + 1)
      }
    }, label = "te_debounced_observe") # end save requirements
    
    observeEvent(input$threshold_complete, {
      # only proceed if all thresholds are non-null
      req(isTruthy(fnrow(thresholds_to_enter()) > 0))
      req(!anyNA(thresholds_to_enter()$met_threshold))
      req(made_a_change())
      req(fnrow(selected_project()) > 0)
      
      shinyjs::toggleState("yes_to_all_HUD", condition = !input$threshold_complete)
      shinyjs::toggleState("yes_to_all_CoC", condition = !input$threshold_complete)
      shinyjs::toggleState("HUD_requirements", condition = !input$threshold_complete)
      shinyjs::toggleState("CoC_requirements", condition = !input$threshold_complete)
      
      # pull latest Project Evaluation in case they just updated Threshold
      project_evaluation(
        get_project_evaluation(user_coc$coc_version_id, selected_project()$project_id)
      )
      
      # Update db
      data <- thresholds_to_enter() |>
        fmutate(
          threshold_complete = input$threshold_complete,
          updated_by = user_coc$username,
          project_id = selected_project()$project_id,
          version_id = project_evaluation()$version_id
        ) |>
        fselect(threshold_complete, updated_by, project_id, version_id) |>
        funique()
      
      update_threshold_complete(get_db_pool(), data)
    }, ignoreInit = TRUE)

    # -- USer PResence ---
    mod_user_presence_server(
      id = "presence",
      user_coc = user_coc,
      # We use the project ID because we are rating a specific project
      record_id = reactive({ selected_project()$project_id }), 
      active = active
    )
  }) # end moduleServer
}
