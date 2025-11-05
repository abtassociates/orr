# UI modules for project rating
mod_rating_scores_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Rating Entry",
    value = id,
    uiOutput(ns("project_rating_factors"))
  )
}

mod_rating_scores_server <- function(id, user_coc, selected_project, funding_action, module_returns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    factors_and_scores_for_project <- reactive({
      req(user_coc$coc_version_id)
      req(selected_project())
      req(module_returns$customize_rating_criteria)

      get_db_query(glue::glue_sql(
      "SELECT r.rating_factor_id, 
          r.rating_factor_text, 
          CASE WHEN r.rating_factor_text_short IS NOT NULL THEN r.rating_factor_text_short ELSE r.rating_factor_text END AS rating_factor_text_short, 
          r.piping_text, r.project_type, r.target_population, sr.selected_rating_factor_id, 
          fg.factor_group, fsg.factor_subgroup, 
          r.goal, r.max_point_value,
          rs.rating_score, rs.performance, rs.project_id
        FROM rating_factors r
        INNER JOIN selected_rating_factors sr ON sr.rating_factor_id = r.rating_factor_id
        JOIN factor_groups fg ON r.factor_group = fg.factor_group_id
        LEFT JOIN factor_subgroups fsg ON r.factor_subgroup = fsg.factor_subgroup_id
        LEFT JOIN rating_scores rs ON rs.selected_rating_factor_id = sr.selected_rating_factor_id
        WHERE sr.coc_version_id = {user_coc$coc_version_id} AND 
          r.funding_action = {get_lookup_refid(funding_action, 'funding_action')} AND
          r.project_type = {selected_project()$project_type} AND
          (r.target_population = {selected_project()$target_population} OR ({is.na(selected_project()$target_population)} AND r.target_population = 36)) AND
          (rs.project_id = {selected_project()$project_id} OR rs.project_id IS NULL)
      ", .con = DB_CON))
    })
    
    # Project Rating Factors UI
    output$project_rating_factors <- renderUI({
      req(factors_and_scores_for_project())

      nested_data <- list()
      unique_groups <- funique(factors_and_scores_for_project()$factor_group)
      
      for (group_name in unique_groups) {
        group_dt <- factors_and_scores_for_project()[factor_group == group_name]
        nested_data[[group_name]] <- split(group_dt, by = "factor_subgroup")
      }

      # Get applicable rating factors based on project type and population
      # This should match the factors defined in the Customize Rating Criteria tab
      accordion_items_group <- purrr::map(names(nested_data), function(group_name) {
        group_data_subgroups <- nested_data[[group_name]]

        sub_accordion_items <- purrr::map(names(group_data_subgroups), function(subgroup_name) {
          subgroup_data <- nested_data[[group_name]][[subgroup_name]]

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
              piped_text <- gsub("<<goal>>", goal, piping_text)

              fluidRow(
                column(3, p(text_short)),
                column(4, p(piped_text)),
                column(1, textInput(ns(paste0("performance_", id)), label = NULL, value = performance)),
                column(1, textInput(ns(paste0("rating_score_", id)), label = NULL, value = rating_score)),
                column(1, p("out of", style = "text-align: center;")),
                column(1, p(max_points, style = "text-align: center;"))
              )
            }
          )

          bslib::accordion_panel(
            title = ifelse(subgroup_name == "NA", "", subgroup_name),
            hr(),
            list(fluidRow(
              column(3, p("RATING FACTOR")),
              column(4, p("PERFORMANCE GOAL")),
              column(1, p("PERFORMANCE", style = "text-align: center;")),
              column(1, p("POINTS AWARDED", style = "text-align: center;")),
              column(1, p("")),
              column(1, p("MAX POINT VALUE", style = "text-align: center;"))
            ),
            factor_rows
            ),
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
        open = names(nested_data)[1]
      )
    })
  })
}
