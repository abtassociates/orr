mod_inventory_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Review Projects",
    value = id,
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        width = 300,
        open = FALSE,
        selectInput(ns("filter_funding_action"), "Funding Action",
                    choices = c("All", "Renew", "New", "Expand", "Reallocate", "Ignore"),
                    multiple = TRUE),
        selectInput(ns("filter_dv_renewal"), "DV Renewal",
                    choices = c("All", "Yes", "No"),
                    multiple = TRUE),
        selectInput(ns("filter_project_type"), "Project Type",
                    choices = c("All", project_types),
                    multiple = TRUE),
        selectInput(ns("filter_target_pop"), "Target Population",
                    choices = c("All", target_populations),
                    multiple = TRUE),
        selectInput(ns("filter_org"), "Organization",
                    choices = c("All"),  # Will be updated in server
                    multiple = TRUE)
      ),
      card(
        card_header("Review Projects"),
        DTOutput(ns("projects_table")),
        actionButton(ns("add_project_btn"), "Add New Project")
      )
    )
  )
}

mod_inventory_server <- function(id, projects_data) {
  moduleServer(id, function(input, output, session) {
    output$projects_table <- renderDT({
      datatable(
        projects_data(),
        editable = TRUE,
        options = list(
          pageLength = 25,
          scrollX = TRUE
        )
      )
    })
    
    # Filtered projects data
    filtered_projects <- reactive({
      req(projects_data())
      data <- projects_data()
      
      # First filter out "Ignore" projects unless specifically requested
      if (!("Ignore" %in% input$filter_funding_action)) {
        data <- data |> fsubset(is.na(Funding_Action) | Funding_Action != "Ignore")
      }
      
      # Apply filters
      if (!("All" %in% input$filter_funding_action) && length(input$filter_funding_action) > 0) {
        data <- data |> fsubset(Funding_Action %in% input$filter_funding_action)
      }
      
      if (!("All" %in% input$filter_dv_renewal) && length(input$filter_dv_renewal) > 0) {
        data <- data |> fsubset(DV_Renewal %in% input$filter_dv_renewal)
      }
      
      if (!("All" %in% input$filter_project_type) && length(input$filter_project_type) > 0) {
        data <- data |> fsubset(Project_Type %in% input$filter_project_type)
      }
      
      if (!("All" %in% input$filter_target_pop) && length(input$filter_target_pop) > 0) {
        data <- data |> fsubset(Target_Population %in% input$filter_target_pop)
      }
      
      if (!("All" %in% input$filter_org) && length(input$filter_org) > 0) {
        data <- data |> fsubset(Organization_Name %in% input$filter_org)
      }
      
      data
    })
    
    # Projects table
    output$projects_table <- renderDT({
      req(filtered_projects())
      data <- filtered_projects() |>
        fselect(-CoC_Code)  # Remove CoC Code column
      
      # Define which columns should be green (editable by user)
      user_columns <- c("DV_Renewal", "Grant_Number", "CoC_Funding_Requested", "Funding_Action")
      
      dt <- datatable(
        data,
        editable = "cell",
        filter = "top",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          orderClasses = TRUE,
          columnDefs = list(
            list(
              targets = which(names(data) %in% user_columns),
              className = 'green-background'
            ),
            list(
              targets = which(names(data) == "Funding_Action") - 1,
              render = JS(
                "function(data, type, row, meta) {
                if (type === 'display') {
                  return data === null ? '' : data;
                }
                return data;
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
      data <- projects_data()
      
      # Get the row index in the filtered view
      row_idx <- info$row
      
      # Get the actual row index in the full dataset
      actual_row <- which(data$Project_Name == filtered_projects()$Project_Name[row_idx])
      
      # Adjust column index since we removed CoC_Code
      col_name <- names(filtered_projects())[info$col + 1]
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
