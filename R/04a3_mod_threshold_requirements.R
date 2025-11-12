# UI modules for project rating
mod_threshold_requirements_ui <- function(id) {
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
        )
      ),
      card_footer(
        style = "display: flex; justify-content: space-between; align-items: center;",
        actionButton(ns("save_requirements"), "Save Thresholds", icon = icon("save"), class="btn-primary")
      )
    )
  )
}

mod_threshold_requirements_server <- function(id, user_coc, selected_project, selected_criteria) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    selected_thresholds <- reactive({
      req(user_coc$coc_version_id)

      get_db_query(glue::glue_sql(
        "SELECT st.selected_threshold_id, t.type, t.threshold_text, t.threshold_id
        FROM thresholds t
        FULL JOIN selected_thresholds st ON st.threshold_id = t.threshold_id
        WHERE st.coc_version_id = {user_coc$coc_version_id} OR t.type = 'HUD'
      ", .con = DB_CON))
    })
    
    threshold_entries <- reactive({
      req(user_coc$coc_version_id)
      req(selected_project)
      
      selected_thresholds() |>
        join(
          get_db_query(glue::glue_sql(
            "SELECT te.selected_threshold_id, met_threshold
            FROM threshold_entries te
            INNER JOIN selected_thresholds st ON st.selected_threshold_id = te.selected_threshold_id
            WHERE te.project_id = {selected_project}
          ", .con = DB_CON)), 
          on="selected_threshold_id"
        )
    })
    
    load_selected_thresholds <- function(threshold_type) {
      thresholds <- threshold_entries() |>
        fsubset(type == threshold_type)

      lapply(1:nrow(thresholds), function(i) {
        checkboxInput(
          ns(paste0(threshold_type, "_req_", thresholds$threshold_id[i])),
          label = thresholds$threshold_text[i],
          width = '100%'
        )
      })
    }
    
    # HUD Requirements UI
    output$hud_requirements <- renderUI({
      req(selected_project)
      load_selected_thresholds("HUD")
    })
    
    # CoC Requirements UI
    output$coc_requirements <- renderUI({
      req(selected_project)
      load_selected_thresholds("CoC")
    })
    
    observeEvent(input$save_requirements, {
      req(user_coc$coc_version_id, user_coc$username)

      update_data <- threshold_entries() |>
        add_user_stamp(user_coc) |>
        fmutate(
          met_threshold = map2_lgl(type, threshold_id, ~input[[paste0(.x, "_req_", .y)]])
        )
      
      for (i in 1:nrow(update_data)) {
        row_data <- update_data[i, ]
        update_query <- glue_data_sql(
          "UPDATE threshold_entries 
          SET met_threshold = {met_threshold}, created_by = {created_by}, updated_by = {created_by}
          WHERE project_id = {selected_project};",
          .con = DB_CON,
          .envir = as.list(row_data)
        ) 
        DBI::dbExecute(DB_CON, update_query)
      }
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