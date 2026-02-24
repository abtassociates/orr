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
          # uiOutput(ns("CoC_requirements_ui"))
        ),
        open = TRUE
      ),
      card_footer(
        style = "display: flex; justify-content: space-between; align-items: center;",
        div(),
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
    
    # updating_from_db <- reactiveVal(NA)
    thresholds_to_enter <- reactiveVal(NULL)
    project_evaluations <- reactiveVal(NULL)
    refresh_trigger <- reactiveVal(0)
    
    coc_thresholds_to_enter <- reactive({
      thresholds_to_enter() |>
        fsubset(type == "CoC" & selected)
    })
    
    observeEvent(c(selected_project(), refresh_trigger()), {
      req(user_coc$coc_version_id)
      # req(is.na(updating_from_db() || updating_from_db()))

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
          WHERE p.coc_version_id = $1 and pe.project_id = $2",
          params = list(user_coc$coc_version_id, selected_project()$project_id)
        )
      )

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
      
      updateCheckboxInput(
        session,
        "yes_to_all_CoC",
        value = allv(thresholds_to_enter()[type == "CoC"]$met_threshold, TRUE)
      )
    })
    
    observeEvent(input$save_requirements, {
      req(user_coc$coc_version_id, user_coc$username)
      
      update_data <- thresholds_to_enter() |>
        add_user_stamp(user_coc, is_new = TRUE) |>
        fmutate(
          met_threshold_new = fifelse(type == "CoC", threshold_id %in% input$CoC_requirements, threshold_id %in% input$HUD_requirements),
          project_id = selected_project()$project_id, new_date_updated = get_db_timestamp()
        )
      
      if (nrow(update_data) > 0) {
        new_project_evaluations <- thresholds_to_enter() |> 
          fmutate(project_id = selected_project()$project_id) |>
          pivot(
            ids = "project_id",
            values = "met_threshold", 
            names = "type",
            how = "wider", 
            FUN = all
          ) |>
          frename(\(x) ifelse(x == "project_id", x, paste0("met_", x, "_thresholds"))) |>
          join(
            project_evaluations() |> fselect(project_id, date_updated), 
            on = "project_id"
          )
        
        pool::poolWithTransaction(DB_POOL, function(p) {
          update_threshold_entries_db(p, update_data)
          update_project_evaluations_db(p, new_project_evaluations, user_coc$username)
        })
        
        refresh_trigger(\(x) x + 1)
      }
    }, ignoreInit = TRUE)
    
    toggle_yes_to_all <- function(ttype) {
      num_thresholds_to_enter <- fnrow(thresholds_to_enter()[type == ttype])
      num_thresholds_selected <- length(input[[paste0(ttype, "_requirements")]])

      updateCheckboxInput(
        session,
        paste0("yes_to_all_", ttype),
        value = num_thresholds_to_enter == num_thresholds_selected
      )
    }
    
    observeEvent(input$HUD_requirements, toggle_yes_to_all("HUD"))
    observeEvent(input$CoC_requirements, toggle_yes_to_all("CoC"))
    
    # Toggle HUD/CoC requirements when yes-to-all box is checked/unchecked
    yes_to_all <- reactiveValues()
    lapply(c("HUD","CoC"), function(ttype) {
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
            selected = if(new_val) thresholds_to_enter()[type == ttype]$threshold_id else NULL
          )
        }
      }, ignoreInit = TRUE)
    }) # end yes_to_all handler
    
    save_to_db <- function(p, sql, params, tbl_name) {
      tryCatch({
        DBI::dbExecute(
          p,
          sql,
          params = params
        )
        showNotification(glue::glue("{tbl_name} saved successfully!"), type = "message", duration = 3)
      }, error = function(e) {
        # If an error occurs, do NOT reset the flag, so it will try again.
        # Notify the user of the failure.
        browser()
        showNotification(glue::glue("Error saving {tbl_name}: {e$message}"), type = "error", duration = 10)
        cat("Database save error:", e$message, "\n")
      })
                       
    }
    update_threshold_entries_db <- function(p, update_data) {
      save_to_db(
        p, 
        "INSERT INTO threshold_entries (project_id, threshold_id, met_threshold, created_by)
          VALUES ($1, $2, $3, $4)
          ON CONFLICT (project_id, threshold_id) DO UPDATE SET 
            met_threshold = EXCLUDED.met_threshold, 
            date_updated = $5, 
            updated_by = EXCLUDED.created_by
          WHERE date_updated = $6;",
        update_data |>
          fsubset(met_threshold_new != met_threshold) |>
          fselect(project_id, threshold_id, met_threshold, created_by, new_date_updated, date_updated) |> 
          as.list() |> 
          unname(),
        "threshold_entries"
      )
    }
    
    update_project_evaluations_db <- function(p, new_project_evaluations, username) {
      save_to_db(
        p,
        "INSERT INTO project_evaluations (project_id, method, met_hud_thresholds, met_coc_thresholds, created_by)
          VALUES ($1, 'in_app', $2, $3, $4)
          ON CONFLICT (project_id) DO UPDATE SET 
            method = EXCLUDED.method,
            met_hud_thresholds = EXCLUDED.met_hud_thresholds, 
            met_coc_thresholds = EXCLUDED.met_coc_thresholds,
            date_updated = $5, 
            updated_by = EXCLUDED.created_by
          WHERE date_updated = $6 OR ($5 IS NULL AND project_evaluations.date_updated IS NULL);",
        new_project_evaluations |>
          fmutate(
            created_by = username, 
            new_date_updated = get_db_timestamp()
          ) |>
          fselect(project_id, met_HUD_thresholds, met_CoC_thresholds, created_by, new_date_updated, date_updated) |>
          as.list() |> 
          unname(),
        "project_evaluations"
      )
    }
  }) # end moduleServer
}
