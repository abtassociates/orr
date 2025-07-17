mod_alternative_rating_ui <- function(id) {
  ns <- NS(id)
  
  # Alternative Rating
  nav_panel(
    "Alternative Rating",
    value = "alternative_rating",
    card(
      DTOutput(ns("alternative_rating_table"))
    )
  )
}

mod_alternative_rating_server <- function(id, projects_data) {
  moduleServer(id, function(input, output, session) {
    
    # Alternative Rating table
    output$alternative_rating_table <- renderDT({
      req(projects_data())
      
      # Get only projects that can be rated (not "Ignore")
      ratable_projects <- projects_data() %>%
        fsubset(!is.na(Funding_Action), Funding_Action != "Ignore") %>%
        fmutate(
          Project_ID = row_number(),  # Add Project ID
          HUD_Threshold = NA_character_,  # Add threshold columns
          CoC_Threshold = NA_character_,
          Rating_Score = NA_real_
        ) %>%
        fselect(
          Project_ID,
          Grant_Number,
          Funding_Action,
          Project_Name,
          Organization_Name,
          Project_Type,
          Target_Population,
          HUD_Threshold,
          CoC_Threshold,
          Rating_Score
        )
      
      datatable(
        ratable_projects,
        editable = list(
          target = "cell",
          disable = list(columns = c(0:6)),  # Disable editing for first 7 columns
          type = list(
            HUD_Threshold = 'select',
            CoC_Threshold = 'select'
          ),
          options = list(
            HUD_Threshold = c("Yes", "No"),
            CoC_Threshold = c("Yes", "No")
          )
        ),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          columnDefs = list(
            list(
              targets = 7:9,  # Last 3 columns
              className = 'green-background'
            )
          )
        )
      )
    })
    
    # Update alternative rating data when cell is edited
    observeEvent(input$alternative_rating_table_cell_edit, {
      info <- input$alternative_rating_table_cell_edit
      str(info)
      
      # Get the current data
      data <- projects_data()
      
      # Get the row from the filtered/displayed data
      edited_row <- info$row + 1
      
      # Update the appropriate column based on what was edited
      col_idx <- info$col
      if (col_idx == 7) {  # HUD Threshold
        data$HUD_Threshold[edited_row] <- info$value
      } else if (col_idx == 8) {  # CoC Threshold
        data$CoC_Threshold[edited_row] <- info$value
      } else if (col_idx == 9) {  # Rating Score
        # Ensure the rating score is between 0 and 100
        score <- as.numeric(info$value)
        if (!is.na(score) && score >= 0 && score <= 100) {
          data$Rating_Score[edited_row] <- score
        }
      }
      
      # Update the reactive value
      projects_data(data)
    }, ignoreInit = TRUE)
  })
}