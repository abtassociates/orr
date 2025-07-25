mod_inventory_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Review Projects",
    value = id,
    card(
      card_header("Review Projects"),
      DTOutput(ns("projects_table")),
      br(),
      actionButton(ns("add_project_btn"), "Add New Project")
    )
  )
}

mod_inventory_server <- function(id, projects_data) {
  moduleServer(id, function(input, output, session) {
    user_columns <- c("dv_renewal", "grant_number", "coc_amount_awarded_last_year", "coc_amount_expended_last_year", "coc_funding_requested", "funding_action")
    
    # Projects table -----
    output$projects_table <- renderDT({
      req(projects_data())

      data <- projects_data()

      validate(
        need(nrow(data) > 0, "No rows")
      )
      
      initialize_table_ui(data)
    })
    
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
      showModal(mod_inventory_add_project_ui("add_project"))
    })
    mod_inventory_add_project_server("add_project", projects_data)
    
    initialize_table_ui <- function(data) {
      # filter out Ignores by default
      initial_filter <- vector("list", ncol(data))
      initial_filter[[which(names(data) == "funding_action")]] <- list(search = "[\"Renew\"]")
      dt <- datatable(
        data,
        editable = "row",
        filter = "top",
        rownames = FALSE,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "400px",  # Limit table height
          fixedHeader = TRUE,
          searchCols = initial_filter,
          columnDefs = list(
            list(
              targets = which(names(data) %in% user_columns),
              className = 'green-background'
            )
          )
        )
      )
      return(dt)
    }
  })
}

