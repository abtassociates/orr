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
    
    thresholds_to_enter <- reactive({
      req(user_coc$coc_version_id)

      get_db_query(
        "SELECT st.selected_threshold_id, t.type, t.threshold_text, t.threshold_id, met_threshold, threshold_entry_id, te.date_updated
        FROM thresholds t
        LEFT JOIN selected_coc_thresholds st ON st.threshold_id = t.threshold_id
        LEFT JOIN threshold_entries te ON te.threshold_id = t.threshold_id
        WHERE st.coc_version_id = $1 OR t.type = 'HUD'
        ",
        params = list(user_coc$coc_version_id)
      )
    })
    
    load_selected_threshold_inputs <- function(threshold_type) {
      thresholds <- thresholds_to_enter() |>
        fsubset(type == threshold_type)

      if(fnrow(thresholds) == 0){
        return(NULL)  
      } else {
        lapply(1:nrow(thresholds), function(i) {
          checkboxInput(
            ns(paste0(threshold_type, "_req_", thresholds$threshold_id[i])),
            value = fcoalesce(as.logical(thresholds$met_threshold[i]), FALSE),
            label = thresholds$threshold_text[i],
            width = '100%'
          )
        })
      }
      
    }
    
    # HUD Requirements UI
    output$hud_requirements <- renderUI({
      req(selected_project)
      load_selected_threshold_inputs("HUD")
    })
    
    # CoC Requirements UI
    output$coc_requirements <- renderUI({
      req(selected_project)
      load_selected_threshold_inputs("CoC")
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
    lapply(c("hud","coc"), function(type) {
      new_val <- input[[paste0("yes_to_all_", type)]]
      observeEvent(new_val, {
        req(new_val)
        
        stored_val <- isolate(yes_to_all[[type]])
        is_initialized <- !is.null(stored_val)
        
        # Only update children if the user clicked (value changed from what we last recorded)
        if(!identical(new_val, stored_val)) {
          yes_to_all[[type]] <- new_val
          browser()
          project_evaluations(
            project_evaluations() |>
              fmutate(paste0("met_", type, "_thresholds") := new_Val)
          )
          
          updateCheckboxGroupInput(
            session,
            paste0(type, "_requirements"),
            selected = if(new_Val) thresholds_to_enter()[type == toupper(type)] else NULL
          )
        }
      }, ignoreInit = TRUE)
    }) # end yes_to_all handler
  }) # end moduleServer
}
