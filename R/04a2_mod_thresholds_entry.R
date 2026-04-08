# UI modules for project rating
mod_thresholds_entry_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Threshold Entry",
    value = id,
    card(
      div(
        id=ns("empty"),
        class="shiny-output-error shiny-output-error-shiny.silent.error shiny-output-error-validation",
        "Select a project in the left-hand sidebar to begin rating"
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
    
    # These are the individual threshold entries
    # These will be aggregated up to the project-level project-evaluations table
    
    # Disable accordion interaction until a project is selected
    observe({
      project_is_selected <- !is.null(selected_project())
      shinyjs::toggle(ns("empty"), condition = !project_is_selected)
      shinyjs::toggleState(selector = glue::glue("#{ns('reqs')} .accordion-button"), condition = project_is_selected)
      shinyjs::toggleState(ns("save_requirements"), condition = project_is_selected)
      
      if(project_is_selected) {
        bslib::accordion_panel_open("reqs", "HUD Requirements", session = session)
        bslib::accordion_panel_open("reqs", "CoC Requirements", session = session)
      }
    })
    
    # updating_from_db <- reactiveVal(NA)
    thresholds_to_enter <- reactiveVal(NULL)
    project_evaluation <- reactiveVal(NULL)
    refresh_trigger <- reactiveVal(0)
    
    coc_thresholds_to_enter <- reactive({
      thresholds_to_enter() |>
        fsubset(type == "CoC" & selected)
    })
    
    observeEvent(c(selected_project(), refresh_trigger(), user_coc$customized_coc_thresholds_updated), {
      req(user_coc$coc_version_id)
      req(selected_project())
      req(fnrow(selected_project()) > 0)

      # individual threshold entries
      thresholds_to_enter(
        get_all_thresholds_to_enter(
          user_coc$coc_version_id, 
          selected_project()$project_id
        )
      )
      
      # project-level evaluations
      project_evaluation(
        get_project_evaluation(
          user_coc$coc_version_id, 
          selected_project()$project_id
        )
      )

      # Initialize threshold selections
      updateCheckboxGroupInput(
        session,
        "HUD_requirements",
        selected = thresholds_to_enter()[type == "HUD" & met_threshold]$threshold_id
      )
      
      updateCheckboxGroupInput(
        session,
        "CoC_requirements",
        choices = setNames(
          coc_thresholds_to_enter()$threshold_id,
          coc_thresholds_to_enter()$threshold_text
        ),
        selected = coc_thresholds_to_enter()[met_threshold == TRUE]$threshold_id
      )
      
      has_coc_thresholds <- fnrow(thresholds_to_enter()[type == "CoC"]) > 0
      if(has_coc_thresholds) {
        updateCheckboxInput(
          session,
          "yes_to_all_CoC",
          value = allv(thresholds_to_enter()[type == "CoC"]$met_threshold, TRUE)
        )
      } else {
        shinyjs::toggle("yes_to_all_CoC", condition = has_coc_thresholds)
      }
    })
    
    stop_yes_to_all_cascade <- reactiveValues()
    stop_yes_to_all_cascade$HUD <- FALSE
    stop_yes_to_all_cascade$CoC <- FALSE
    
    toggle_yes_to_all <- function(ttype) {
      req(user_coc$coc_version_id, selected_project())

      if(stop_yes_to_all_cascade[[ttype]]) {
        stop_yes_to_all_cascade[[ttype]] <- FALSE
        return()
      }
      
      thresholds_to_select <- thresholds_to_enter()[type == ttype, threshold_id]
      thresholds_selected <- input[[paste0(ttype, "_requirements")]]

      stop_yes_to_all_cascade[[ttype]] <- TRUE
      updateCheckboxInput(
        session,
        paste0("yes_to_all_", ttype),
        value = setequal(thresholds_to_select, thresholds_selected)
      )
    }
    
    observeEvent(input$HUD_requirements, toggle_yes_to_all("HUD"), ignoreNULL = FALSE)
    observeEvent(input$CoC_requirements, toggle_yes_to_all("CoC"), ignoreNULL = FALSE)
    
    # Toggle HUD/CoC requirements when yes-to-all box is checked/unchecked
    yes_to_all <- reactiveValues()
    lapply(c("HUD","CoC"), function(ttype) {
      observeEvent(input[[paste0("yes_to_all_", ttype)]], {
        req(user_coc$coc_version_id, selected_project())

        if(stop_yes_to_all_cascade[[ttype]]) {
          stop_yes_to_all_cascade[[ttype]] <- FALSE
          req(FALSE)
        }
        
        new_val <- input[[paste0("yes_to_all_", ttype)]]
        
        stored_val <- isolate(yes_to_all[[ttype]])
        # is_initialized <- !is.null(stored_val)
        
        # Only update children if the user clicked (value changed from what we last recorded)
        if(!identical(new_val, stored_val)) {
          yes_to_all[[ttype]] <- new_val
          
          stop_yes_to_all_cascade[[ttype]] <- TRUE
          
          updateCheckboxGroupInput(
            session,
            paste0(ttype, "_requirements"),
            selected = if(new_val) thresholds_to_enter()[type == ttype]$threshold_id else character(0)
          )
        }
      }, ignoreInit = TRUE)
    }) # end yes_to_all handler
    
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
