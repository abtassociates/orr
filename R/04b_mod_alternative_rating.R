mod_alternative_rating_ui <- function(id) {
  ns <- NS(id)
  
  # Alternative Rating
  nav_panel(
    "Alternative Rating",
    value = id,
    card(
      DTOutput(ns("alternative_rating_table")),
      card_footer(
        style = "display: flex; justify-content: space-between; align-items: center;",
        actionButton(ns("save_rating"), "Save Rating", icon = icon("save"), class="btn-primary"),
        actionButton(ns("import_rating"), "Import Rating", icon = icon("upload"))
      )
    )
  )
}

mod_alternative_rating_server <- function(id, user_coc) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    db_has_changed <- reactiveVal(NA)
    
    ratable_projects <- reactiveVal(NULL)
    
    observe({
      req(user_coc$coc_version_id)
      req(is.na(db_has_changed()) || db_has_changed())

      ratable_projects(
        get_db_query(
          "SELECT 
            p.project_id, 
            p.organization_name, 
            p.project_name, 
            p.grant_number, 
            p.funding_action, 
            p.project_type, 
            p.target_population, 
            pe.met_hud_thresholds,
            pe.met_coc_thresholds,
            pe.weighted_score,
            pe.date_updated
          FROM projects p
          LEFT JOIN project_evaluations pe ON p.project_id = pe.project_id
          LEFT JOIN lookups l ON p.funding_action = l.reference_id
          WHERE p.coc_version_id = $1 AND p.funding_action IS NOT NULL AND l.value <> 'Ignore'",
          params = list(user_coc$coc_version_id)
        ) |>
          fmutate(
            met_hud_thresholds = factor_yesno(met_hud_thresholds),
            met_coc_thresholds = factor_yesno(met_coc_thresholds)
          )
      )
    }) # end observe that updates ratable_projects
    
    # Alternative Rating table
    output$alternative_rating_table <- renderDT({
      
      data <- ratable_projects()
      
      shiny::validate(need(
        nrow(data) > 0, 
        "No projects to rate"
      ))
      
      editable_cols <- c("met_hud_thresholds", "met_coc_thresholds", "weighted_score")
      
      ## filter out Ignores by default-----
      initial_filter <- vector("list", ncol(data))
      initial_filter[[which(names(data) == "funding_action")]] <- list(search = '["Renew","Reallocate","Replace","New","Expand"]')
      
      colnames <- unname(project_variable_labels[names(data)])
      
      initialize_inline_edit_table_ui(
        data,
        tableID = ns("alternative_rating_table"), 
        initial_filter = initial_filter,
        column_defs = list(
          list(
            targets =c(which(names(data) %in% c("funding_action", "date_updated")) - 1),
            className = "hidden",
            visible = FALSE
          )
        ),
        formatting = list(
          function(x) formatStyle(
            x,
            columns = c("organization_name", "project_name"),
            `white-space` = "nowrap",
            `overflow` = "hidden",
            `max-width` = "400px"
          ),
          function(x) formatStyle(
            x,
            columns = editable_cols,
            backgroundColor = USER_ENTRY_BG_COLOR
          )
        ),
        colnames = colnames,
        cols_to_disable = setdiff(names(data), editable_cols)
      )
    })
    
    # Update alternative rating data when cell is edited
    observeEvent(input$alternative_rating_table_cell_edit, {
      info <- input$alternative_rating_table_cell_edit
      
      current_data <- ratable_projects()
      
      current_data[info$row, info$col + 1] <- info$value
      
      ratable_projects(current_data)
    }, ignoreInit = TRUE) # end alt rating table cell edit
    
    ## datatable proxy-----
    # By updating a proxy (via `replaceData`), updates are faster and don't "flicker" the table
    # However it doesn't work when adding new rows
    projects_table_proxy <- dataTableProxy(ns("alternative_rating_table"), session = session)
    
    observe({
      req(ratable_projects())
      replaceData(projects_table_proxy, ratable_projects(), resetPaging = FALSE)
    })
    
    observeEvent(input$save_rating, {
      req(ratable_projects())
      params_list <- ratable_projects() |>
        fmutate(created_by = user_coc$username) |>
        fselect(project_id, met_hud_thresholds, met_coc_thresholds, created_by, date_updated) |>
        as.list() |>
        unname()
      
      rows_changed <- db_execute("
        INSERT INTO project_evaluations (project_id, method, met_hud_thresholds, met_coc_thresholds, created_by)
        VALUES ($1, 'outside', $2, $3, $4)
        ON CONFLICT (project_id) DO UPDATE SET
          method = 'outside',
          met_hud_thresholds = EXCLUDED.met_hud_thresholds,
          met_coc_thresholds = EXCLUDED.met_coc_thresholds,
          date_updated = CURRENT_TIMESTAMP,
          updated_by   = EXCLUDED.created_by
        WHERE date_updated = $5",
        params = params_list
      )
      
      browser()
      if(rows_changed == 0) {
        showNotification("Someone recently edited this data! Refreshing your view. Resubmit when you're ready.", type = "message")
        db_has_changed(TRUE)
      } else if(rows_changed < fnrow(ratable_projects())) {
        showNotification("Someone recently edited one or more project ratings! Refreshing your view. Resubmit when you're ready.", type = "message")
        db_has_changed(TRUE)
      } else {
        db_has_changed(NA)
        showNotification("Saved rating info!", type = "message")
      }
    })
    
    observeEvent(input$import_rating, {
      
    })
  }) # end module server
}
