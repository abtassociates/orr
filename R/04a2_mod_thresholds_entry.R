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
          uiOutput(ns("hud_requirements"))
        ),
        accordion_panel(
          "CoC Requirements",
          uiOutput(ns("coc_requirements"))
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
        "SELECT st.selected_threshold_id, t.type, t.threshold_text, t.threshold_id, met_threshold, threshold_entry_id
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
          met_threshold_new = map2_lgl(type, threshold_id, ~input[[paste0(.x, "_req_", .y)]])
        )
      
      to_delete <- update_data |> 
        fsubset(!met_threshold_new & fcoalesce(as.logical(met_threshold), FALSE))
      
      to_upsert <- update_data |> 
        fsubset(met_threshold_new)
      
      DBI::dbWithTransaction(DB_POOL, {
        if (nrow(to_delete) > 0) {
          DBI::dbExecute(DB_POOL, glue::glue_sql(
            "DELETE FROM threshold_entries 
            WHERE threshold_entry_id IN ({to_delete$threshold_entry_id*})",
            .con = DB_POOL
          ))
        }
        
        if (nrow(to_upsert) > 0) {
          params_list <- lapply(seq_row(to_upsert), function(i) {
            list(
              to_upsert$threshold_entry_id[i],
              selected_project,
              to_upsert$threshold_id[i],
              to_upsert$created_by[i],
              to_upsert$updated_by[i]
            )
          })
          
          DBI::dbExecute(
            DB_POOL, 
            "INSERT INTO threshold_entries (threshold_entry_id, project_id, threshold_id, met_threshold, created_by)
            VALUES ($1, $2, $3, 1, $4)
            ON CONFLICT (threshold_entry_id) 
            DO UPDATE SET met_threshold = 1, date_updated = CURRENT_TIMESTAMP, updated_by = $5",
            params = params_list
          )
        }
        
        shiny::showNotification('Threshold entries updated!', type='message')
      })
    }, ignoreInit = TRUE)
    # Automatically set all HUD requirements to Yes when yes_to_all is checked
    # observeEvent(input$yes_to_all_hud, {
    #   if(input$yes_to_all_hud) {
    #     for(i in 1:5) {
    #       updateSelectInput(session,
    #                         paste0("hud_req_", i),
    #                         selected = "Yes")
    #     }
    #   }
    # })
  })
}
