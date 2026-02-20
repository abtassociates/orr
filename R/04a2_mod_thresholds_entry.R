# UI modules for project rating
mod_thresholds_entry_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Threshold Entry",
    value = id,
    card(
      accordion(
        accordion_panel(
          "HUD Requirements",
          checkboxInput(
            ns("yes_to_all_hud"),
            "Met all HUD requirements"
          ),
          checkboxGroupInput(
            ns("hud_requirements"),
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
            ns("yes_to_all_coc"),
            "Met all CoC requirements"
          ),
          uiOutput(ns("coc_requirements_ui"))
        ),
        open = TRUE
      ),
      card_footer(
        style = "display: flex; justify-content: space-between; align-items: center;",
        actionButton(ns("save_requirements"), "Save Thresholds", icon = icon("save"), class="btn-primary")
      )
    )
  )
}

mod_thresholds_entry_server <- function(id, user_coc, selected_project, selected_criteria) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # These are the individual threshold entries
    # These will be aggregated up to the project-level project-evaluations table
    
    updating_from_db <- reactiveVal(NA)
    thresholds_to_enter <- reactiveVal(NULL)
    project_evaluations <- reactiveVal(NULL)
    
    coc_thresholds_to_enter <- reactive({
      thresholds_to_enter() |>
        fsubset(type == "CoC" & selected)
    })
    
    observe({
      req(user_coc$coc_version_id)
      req(is.na(updating_from_db() || updating_from_db()))
      req(selected_project())

      # individual threshold entries
      thresholds_to_enter(
        get_db_query(
          "SELECT st.selected_threshold_id, st.selected, t.type, t.threshold_text, t.threshold_id, met_threshold, threshold_entry_id, te.date_updated
          FROM thresholds t
          LEFT JOIN selected_thresholds st ON st.threshold_id = t.threshold_id
          LEFT JOIN threshold_entries te ON te.threshold_id = t.threshold_id
          WHERE (st.coc_version_id = $1 OR t.type = 'HUD') AND (te.project_id = $2 OR te.project_id IS NULL)",
          params = list(
            user_coc$coc_version_id,
            selected_project()$project_id
          )
        ) |>
          fmutate(met_threshold = fcoalesce(as.logical(met_threshold), FALSE))
      )
      
      # project-level evaluations
      project_evaluations(
        get_db_query(
          "SELECT p.coc_version_id, pe.project_id, method, met_hud_thresholds, met_coc_thresholds, pe.date_updated 
          FROM project_evaluations pe
          LEFT JOIN projects p ON pe.project_id = p.project_id
          WHERE coc_version_id = $1",
          params = list(user_coc$coc_version_id)
        )
      )
      
      updateCheckboxGroupInput(
        session,
        "hud_requirements",
        selected = thresholds_to_enter()[type == "HUD" & met_threshold]
      )
      
      updateCheckboxGroupInput(
        session,
        "coc_requirements",
        choices = setNames(
          coc_thresholds_to_enter()$threshold_id, 
          coc_thresholds_to_enter()$threshold_text
        ),
        selected = coc_thresholds_to_enter()[met_threshold == TRUE]
      )
    })

    # CoC Requirements UI
    output$coc_requirements_ui <- renderUI({
      req(selected_project())
      
      checkboxGroupInput(
        ns("coc_requirements"),
        label = NULL,
        choices = setNames(
          coc_thresholds_to_enter()$threshold_id, 
          coc_thresholds_to_enter()$threshold_text
        ),
        selected = coc_thresholds_to_enter()[met_threshold == TRUE]$threshold_id,
        width = "100%"
      )
    })
    
    observeEvent(input$save_requirements, {
      req(user_coc$coc_version_id, user_coc$username)
      
      update_data <- thresholds_to_enter() |>
        add_user_stamp(user_coc, is_new = TRUE) |>
        fmutate(
          met_threshold_new = fifelse(type == "CoC", threshold_id %in% input$coc_requirements, threshold_id %in% input$hud_requirements),
          project_id = selected_project()$project_id, new_date_updated = get_db_timestamp()
        )
      
      if (nrow(update_data) > 0) {
        params_list <- update_data |>
          fsubset(met_threshold_new != met_threshold) |>
          fselect(project_id, threshold_id, created_by, new_date_updated, date_updated) |> 
          as.list() |> 
          unname()
        
        tryCatch({
          
          db_execute(
            "INSERT INTO threshold_entries (project_id, threshold_id, met_threshold, created_by)
            VALUES ($1, $2, 1, $3)
            ON CONFLICT (project_id, threshold_id) DO UPDATE SET 
              met_threshold = EXCLUDED.met_threshold, 
              date_updated = $4, 
              updated_by = EXCLUDED.created_by
            WHERE date_updated = $5;",
            params = params_list
          )
          
          ## update reactive ----------
          thresholds_to_enter(
            update_data |> 
              fmutate(met_threshold = met_threshold_new) |>
              fselect(-met_threshold_new)
          )
          
          project_evaluations(
            thresholds_to_enter() |> 
              pivot(
                ids = "project_id",
                values = "met_threshold", 
                names = "type",
                how = "wider", 
                FUN = all
              ) |>
              frename(\(x) ifelse(x == "project_id", x, paste0("met_", x, "_thresholds"))),
          )
          
          showNotification("Threshold entires saved successfully!", type = "message", duration = 3)
        }, error = function(e) {
          # If an error occurs, do NOT reset the flag, so it will try again.
          # Notify the user of the failure.
          showNotification(paste("Error saving data:", e$message), type = "error", duration = 10)
          cat("Database save error:", e$message, "\n")
        })
      }
    }, ignoreInit = TRUE)
    
    # Toggle HUD/CoC requirements when yes-to-all box is checked/unchecked
    yes_to_all <- reactiveValues()
    lapply(c("hud","coc"), function(ttype) {
      observeEvent(input[[paste0("yes_to_all_", ttype)]], {
        new_val <- input[[paste0("yes_to_all_", ttype)]]
        req(new_val)
        
        stored_val <- isolate(yes_to_all[[ttype]])
        # is_initialized <- !is.null(stored_val)
        
        # Only update children if the user clicked (value changed from what we last recorded)
        if(!identical(new_val, stored_val)) {
          yes_to_all[[ttype]] <- new_val
          
          updateCheckboxGroupInput(
            session,
            paste0(ttype, "_requirements"),
            selected = if(new_val) thresholds_to_enter()[type == ifelse(ttype == "coc", "CoC", "HUD") & selected]$threshold_id else NULL
          )
        }
      }, ignoreInit = TRUE)
    }) # end yes_to_all handler
    
    observe({
      req(project_evaluations())
      req(updating_from_db() == FALSE)
      browser()
      params_list <- project_evaluations() |>
        fmutate(created_by = user_coc$username, new_date_updated = get_db_timestamp()) |>
        fselect(project_id, method, met_hud_thresholds, met_coc_thresholds, created_by, new_date_updated, date_updated)
        as.list() |> 
        unname()
      
      tryCatch({
        db_execute(
          "INSERT INTO project_evaluations (project_id, method, met_hud_thresholds, met_coc_thresholds, created_by)
            VALUES ($1, 'in-app', $2, $3, $4)
            ON CONFLICT (project_id) DO UPDATE SET 
              method = EXCLUDED.method,
              met_hud_thresholds = EXCLUDED.met_hud_thresholds, 
              met_coc_thresholds = EXCLUDED.met_coc_thresholds
              date_updated = $5, 
              updated_by = EXCLUDED.created_by
            WHERE date_updated = $6;",
          params = params_list
        )
        
        showNotification("Project evaluations saved successfully!", type = "message", duration = 3)
      }, error = function(e) {
        # If an error occurs, do NOT reset the flag, so it will try again.
        # Notify the user of the failure.
        showNotification(paste("Error saving data:", e$message), type = "error", duration = 10)
        cat("Database save error:", e$message, "\n")
      })
      
    })
  }) # end moduleServer
}
