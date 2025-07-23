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
    
    # Projects table
    output$projects_table <- renderDT({
      req(projects_data())

      data <- projects_data()

      validate(
        need(nrow(data) > 0, "No rows")
      )

      # filter out Ignores by default
      initial_filter <- vector("list", ncol(data))
      initial_filter[[which(names(data) == "funding_action") + 1]] <- list(search = "[\"Renew\"]")

      dt <- datatable(
        data,
        editable = "cell",
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
            ),
            list(
              targets = which(names(data) == "funding_action") - 1,
              render = JS(
                "function(data, type, row, meta) {
                  return type === 'display' && data === null ? '' : data;
                }"
              )
            )
          )
        )
      )
      
      dt
    })
    
    # Update projects data when cell is edited
    observeEvent(input$projects_table_cell_edit, {
      req(projects_data())
      info <- input$projects_table_cell_edit
      str(info)
      
      # Get the current data
      data <- data.table::copy(projects_data())
      
      # Get the row index in the filtered view
      row_idx <- info$row
      
      # Get the actual row index in the full dataset
      actual_row <- which(data$project_name == projects_data()$project_name[row_idx])
      
      # Adjust column index since we removed CoC_Code
      col_name <- names(projects_data())[info$col + 1]
      data[actual_row, col_name] <- info$value
      
      projects_data(data)
    }, ignoreInit = TRUE)
    
    # Update projects data when cell is edited
    observeEvent(input$cell_edit, {
      req(projects_data())
      print("cell was edited")
      data <- projects_data()
      # Adjust column index since we removed CoC_Code
      actual_col <- input$cell_edit$col + 1
      if (actual_col >= which(names(data) == "CoC_Code")) {
        actual_col <- actual_col + 1
      }
      data[input$cell_edit$row + 1, actual_col] <- input$cell_edit$value
      projects_data(data)
    }, ignoreInit = TRUE)
  })
}
