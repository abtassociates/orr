mod_inventory_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Review Projects",
    value = id,
    card(
      card_body(
        fillable = FALSE,
        DTOutput(ns("projects_table"))
      ),
      card_footer(
        actionButton(ns("add_project_btn"), "Add New Project"),
        actionButton(ns("view_giw_btn"), "View GIW Data")
      )
    )
  )
}

mod_inventory_server <- function(id, projects_data, selected_coc) {
  moduleServer(id, function(input, output, session) {
    user_columns <- c("dv_renewal", "grant_number", "coc_amount_awarded_last_year", "coc_amount_expended_last_year", "coc_funding_requested", "funding_action")
    
    # Projects table -----
    output$projects_table <- renderDT({
      req(projects_data())

      data <- projects_data() %>% 
        fselect(-coc_instance_id, -date_created, -date_updated, -created_by, -updated_by)

      validate(
        need(nrow(data) > 0, "No rows")
      )
      
      initialize_table_ui(data)
    })
    
    initialize_table_ui <- function(data) {
      # filter out Ignores by default
      initial_filter <- vector("list", ncol(data))
      initial_filter[[which(names(data) == "funding_action")]] <- list(search = "[\"Renew\"]")
      dt <- datatable(
        data,
        editable = "row",
        filter = "top",
        rownames = FALSE,
        fillContainer = TRUE,
        options = list(
          scrollX = TRUE,
          scrollY = "100%",  # Limit table height
          fixedHeader = TRUE,
          searchCols = initial_filter,
          columnDefs = list(
            list(
              targets = which(names(data) %in% user_columns),
              className = 'green-background'
            )
          )
        )
      ) %>%
        formatStyle(
          columns = c(2,3), 
          `white-space` = "nowrap",
          `overflow` = "hidden",
          `max-width` = "400px"
        )

      return(dt)
    }
    
    # inline edit handling ------
    observeEvent(input$projects_table_cell_edit, {
      req(projects_data())
      info <- input$projects_table_cell_edit
      
      # Get the current data
      data <- data.table::copy(projects_data())
      row_idx <- info$row
      actual_row <- which(data$project_name == projects_data()$project_name[row_idx])
      col_name <- names(projects_data())[info$col + 1]
      data[actual_row, col_name] <- info$value
      
      projects_data(data)
    }, ignoreInit = TRUE)
    
    # Add additonal project handling ----
    observeEvent(input$add_project_btn, {
      showModal(
        mod_inventory_add_project_ui(session$ns("add_project"))
      )
    })
    mod_inventory_add_project_server("add_project", projects_data)
    
    # View GIW Data -------
    giw_data <- reactive({
      get_db_tbl("giw")[coc == selected_coc$coc]
    })
    
    observeEvent(input$view_giw_btn, {
      showModal(
        modalDialog(
          title = "GIW",
          DT::renderDT(
            giw_data(),
            fillContainer = TRUE,
            options = list(
              pageLength = 200,
              scrollY = "400px"
            )
          ),
          size="xl",
          easyClose = TRUE
        )
      )
    })
    
  }) # end moduleServer
}

