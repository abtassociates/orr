# UI modules for project rating
mod_rating_scores_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("project_rating_factors"))
}

mod_rating_scores_server <- function(id, rating_scores) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Project Rating Factors UI
    output$project_rating_factors <- renderUI({
      req(rating_scores())
      
      nested_data <- list()
      unique_groups <- funique(rating_scores()$factor_group)
      
      for (group_name in unique_groups) {
        group_dt <- rating_scores()[factor_group == group_name]
        nested_data[[group_name]] <- split(group_dt, by = "factor_subgroup")
      }
      
      # Get applicable rating factors based on project type and population
      # This should match the factors defined in the Customize Rating Criteria tab
      accordion_items_group <- purrr::map(names(nested_data), function(group_name) {
        group_data_subgroups <- nested_data[[group_name]]
        
        sub_accordion_items <- purrr::map(names(group_data_subgroups), function(subgroup_name) {
          subgroup_data <- nested_data[[subgroup_name]]
          
          factor_rows <- purrr::pmap(
            list(
              subgroup_data$rating_factor_id,
              subgroup_data$rating_factor_text_short,
              subgroup_data$piping_text,
              subgroup_data$goal,
              subgroup_data$performance,
              subgroup_data$rating_score,
              subgroup_data$max_point_value
            ), function(id, text_short, piping_text, goal, performance, rating_score, max_points) {
              piped_text <- paste0(
                gsub("<<goal>>", goal, piping_text), 
                fcase(
                  str_split(goal, " ", simplify=TRUE)[[2]] == "days", "days",
                  str_sub(goal, -1, -1) == "%", "%",
                  default = ""
                )
              )
                  
              fluidRow(
                column(3, p(text_short)),
                column(4, p(piped_text)),
                column(1, textInput(ns("performance"), value = performance)),
                column(1, textInput(ns("rating_score"), value = rating_score)),
                column(1, p("out of")),
                column(1, p(max_points))
              )
            }
          )
          
          bslib::accordion_panel(
            title = ifelse(subgroup_name == "NA", "", subgroup_name),
            hr(),
            factor_rows,
            
            # --- NEW: Add a placeholder for custom factors ---
            # This div will only be added for the specific subgroup.
            # Adjust "Other/Local Priority" to match the exact name in your database.
            if (group_name == "Other and Local Criteria") {
              div(id = ns("custom_factors_placeholder"))
            }
          )
        })
        
        bslib::accordion_panel(
          title = group_name,
          bslib::accordion(
            !!!sub_accordion_items,
            id = ns(paste0("sub_accordion_", make.names(group_name))),
            multiple = TRUE,
            open = names(group_data_subgroups)[1]
          )
        )
      })
      
      bslib::accordion(
        !!!accordion_items_group,
        id = ns("main_accordion"),
        multiple = TRUE,
        open = names(data_groups_nested)[1]
      )
      
    })
  })
}
