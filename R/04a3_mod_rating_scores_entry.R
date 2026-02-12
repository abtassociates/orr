# UI modules for project rating
mod_rating_scores_entry_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Rating Entry",
    value = id,
    singleton(tags$head(tags$script(HTML("
      // Use event delegation. Listen for 'change' events on the document.
      $(document).on('change', '.score-input input', function() {
        const group = $(this).closest('.score-input').data('group');

        let groupTotal = 0;
        
        $('.score-input[data-group=\"' + group + '\"] input').each(function() {
          groupTotal += Number($(this).val()) || 0;
        });
        
        $('.subtotal-display[data-subtotal-for=\"' + group + '\"]').text(groupTotal);
      });"
    )))),
    card(
      uiOutput(ns("project_rating_factors")),
      card_footer(
        style = "display: flex; justify-content: space-between; align-items: center;",
        actionButton(ns("save_rating"), "Save Rating", icon = icon("save"))
      )
    )
  )
}

mod_rating_scores_entry_server <- function(id, user_coc, selected_project, funding_action, module_returns) {
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
          rs.rating_score, rs.performance, rs.project_id,
          rs.date_updated
        FROM rating_factors r
        INNER JOIN selected_rating_factors sr ON sr.rating_factor_id = r.rating_factor_id
        JOIN factor_groups fg ON r.factor_group = fg.factor_group_id
        LEFT JOIN factor_subgroups fsg ON r.factor_subgroup = fsg.factor_subgroup_id
        LEFT JOIN rating_scores rs ON rs.selected_rating_factor_id = sr.selected_rating_factor_id
        WHERE sr.coc_version_id = {user_coc$coc_version_id} AND 
          r.funding_action = {get_lookup_refid(funding_action, 'funding_action')} AND
          r.project_type = {selected_project()$project_type} AND
          (
            {get_lookup_label(selected_project()$target_population, 'target_population') == 'NA'} OR 
            r.target_population = {selected_project()$target_population} OR 
            (
              {is.na(selected_project()$target_population)} AND 
              r.target_population = {get_lookup_refid('General', 'target_population')}
            )
          ) AND
          (rs.project_id = {selected_project()$project_id} OR rs.project_id IS NULL)
      ", .con = DB_POOL))
    })
    
    # Project Rating Factors UI
    output$project_rating_factors <- renderUI({
      req(nrow(factors_and_scores_for_project()) > 0)
      # Group data only by the main factor_group
      grouped_data <- split(factors_and_scores_for_project(), by = "factor_group")
      
      # Create a main accordion panel for each group
      accordion_items_group <- purrr::map(names(grouped_data), function(group_name) {
        group_dt <- grouped_data[[group_name]]
        group_id <- make.names(group_name) # Create a safe ID for JS
        
        # Now, within this group, let's create the table-like content
        # First, split by subgroup to render subgroup headers
        subgroups_in_group <- split(group_dt, by = "factor_subgroup")
        
        # Use map to generate the HTML for each subgroup and its factors
        table_content <- purrr::map(names(subgroups_in_group), function(subgroup_name) {
          
          subgroup_data <- subgroups_in_group[[subgroup_name]]
          
          # Generate the rows for each rating factor in this subgroup
          factor_rows <- purrr::pmap(
            list(
              subgroup_data$rating_factor_id,
              subgroup_data$rating_factor_text, # Using the longer text here, you can choose
              subgroup_data$goal,
              subgroup_data$performance,
              subgroup_data$rating_score,
              subgroup_data$max_point_value
            ),
            function(id, text, goal, performance, rating_score, max_points) {
              # This is a single data row
              fluidRow(
                # We can add a class for CSS styling, e.g., for indentation
                column(5, p(text)),
                column(2, p(goal)),
                column(2, class = "input-col", textInput(ns(paste0("performance_", id)), label = NULL, value = performance)),
                column(1, class = "input-col", numericInput(
                  ns(paste0("rating_score_", id)), 
                  label = NULL, 
                  value = rating_score,
                  min = 0,
                  max = max_points
                )) |>
                  tagAppendAttributes(class = 'score-input', `data-group` = group_id),
                column(2, p(paste("out of", max_points)))
              )
            }
          )
          
          # Combine the subgroup header and its factor rows
          # The subgroup header is just a simple div or row
          tagList(
            if (subgroup_name != "NA") {
              fluidRow(
                column(12, h5(subgroup_name, class = "subgroup-header"))
              )
            },
            factor_rows
          )
        })
        
        # Create the single accordion panel for the whole group
        bslib::accordion_panel(
          title = group_name,
          # The single header row for the "table"
          fluidRow(
            class = "rating-table-header",
            column(5, strong("RATING FACTOR")),
            column(2, strong("PERFORMANCE GOAL")),
            column(2, strong("PERFORMANCE")),
            column(1, strong("POINTS AWARDED")),
            column(2, strong("MAX POINT VALUE"))
          ),
          hr(),
          # Add the content we generated above
          !!!table_content,
          # Add the subtotal row at the end of the group
          hr(),
          fluidRow(
            column(5, strong(paste0(group_name, " Subtotal"))),
            column(2), # empty placeholder
            column(2), # empty placeholder
            # column(1, strong(sum(group_dt$rating_score, na.rm = TRUE)), style="text-align:center"), # Example calculation
            column(1, style = "text-align:center;", class = "subtotal-column", strong(class = "subtotal-display", `data-subtotal-for` = group_id, "0")),
            column(2, strong(paste("out of", sum(group_dt$max_point_value, na.rm = TRUE)))) # Example
          )
        )
      })
      
      # The final accordion structure
      bslib::accordion(
        !!!accordion_items_group,
        id = ns("main_accordion"),
        multiple = TRUE,
        open = names(grouped_data)[1] # Open the first group by default
      )
    }) # end render factors
    
    
    observeEvent(input$save_rating, {
      all_inputs <- reactiveValuesToList(input)
      selected_ids <- factors_and_scores_for_project()$selected_rating_factor_id
      num_selected <- fnrow(factors_and_scores_for_project())
      
      # Build vectors of params upfront
      params <- list(
        project_ids                 = alloc(selected_project()$project_id, num_selected),
        selected_rating_factor_ids  = selected_ids,
        rating_scores               = sapply(selected_ids, \(id) input[[paste0("rating_score_", id)]]),
        performances                = sapply(selected_ids, \(id) input[[paste0("performance_", id)]]),
        created_bys                 = alloc(session$user, num_selected),
        date_updated_checks         = factors_and_scores_for_project()$date_updated
      )
      
      DBI::dbExecute(DB_CON, "
        INSERT INTO rating_scores (project_id, selected_rating_factor_id, rating_score, performance, created_by)
        VALUES ($1, $2, $3, $4, $5)
        ON CONFLICT (project_id, selected_rating_factor_id) DO UPDATE SET
          rating_score = EXCLUDED.rating_score,
          performance  = EXCLUDED.performance,
          date_updated = CURRENT_TIMESTAMP,
          updated_by   = EXCLUDED.created_by
        WHERE rating_scores.date_updated = $6",
        params = params
      )
    })
  }) #end module server
}
