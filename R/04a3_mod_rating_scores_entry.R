# UI modules for project rating
get_col_widths <- function() {
  # Extra small	None	<576px
  # Small	sm	≥576px
  # Medium	md	≥768px
  # Large	lg	≥992px
  # Extra large	xl	≥1200px
  # Extra extra large	xxl	≥1400px
  breakpoints(
    sm = c(4, 2, 2, 2, 2),
    md = c(4, 2, 2, 2, 2),
    lg = c(4, 2, 2, 2, 2),
    xl = c(5, 2, 2, 1, 2),
    xxl = c(5, 2, 2, 1, 2)
  )
}

mod_rating_scores_entry_ui <- function(id) {
  ns <- NS(id)
  col_widths <- get_col_widths()
  
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
      style = "overflow: visible !important;",
      mod_download_rating_ui(ns("download_rating")),
      mod_user_presence_ui(ns("presence")),
      uiOutput(ns("project_rating_factors")) |> shinycssloaders::withSpinner(),
      card(
        id = ns("total_row"),
        style = "display:none;",
        layout_columns(
          col_widths = col_widths,
          div(strong("Total")),
          div(),
          div(),
          div(style = "text-align:center;", 
              strong(textOutput(ns("total_score"), inline=TRUE))),
          div(strong(textOutput(ns("total_max"), inline=TRUE)))
        )
      ),
      card(
        id = ns("weighted_total_row"),
        style = "display:none;",
        layout_columns(
          col_widths = col_widths,
          div(strong("Weighted Total")),
          div(),
          div(),
          div(style = "text-align:center;", 
              strong(textOutput(ns("weighted_total_score"), inline=TRUE))),
          div(strong("out of 100"))
        )
      ),
      card_footer(
        class="sticky-footer",
        style = "display: flex; justify-content: space-between; align-items: center;",
        prettySwitch(ns("rating_complete"), label = "Rating Complete?", status = "success", fill=TRUE)
      )
    )
  )
}

mod_rating_scores_entry_server <- function(id, user_coc, selected_project, funding_action, active, hasProjects) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    refresh_trigger <- reactiveVal(NA)
    factors_and_scores_for_project <- reactiveVal()
    project_evaluation <- reactiveVal()
    updated_rating_complete_from_db <- reactiveVal(NULL)
    
    input_prefixes <- c("rating_score", "performance")
    
    performance_char_limit <- get_db_column_limit("rating_scores","performance")
    
    
    observeEvent(c(selected_project(), refresh_trigger(), user_coc[[paste0("customized_rating_factors_updated_", funding_action)]]), {
      req(user_coc$coc_version_id)
      
      project_is_selected <- isTruthy(fnrow(selected_project()) > 0)
      shinyjs::toggleState("rating_complete", condition = project_is_selected)
      
      req(project_is_selected)
      
      # individual threshold entries
      factors_and_scores_for_project(
        get_rating_factors_and_scores(
          user_coc$coc_version_id,
          selected_project()
        ) 
      )
      
      # project-level evaluations
      project_evaluation(
        get_project_evaluation(
          list(user_coc$coc_version_id, selected_project()$project_id)
        )
      )
      
      if(input$rating_complete != isTruthy(project_evaluation()$rating_complete == 1)) {
        updatePrettySwitch(
          session, 
          "rating_complete", 
          value = isTruthy(project_evaluation()$rating_complete == 1)
        )
        
        updated_rating_complete_from_db(TRUE)
      }
    })
    
    # Project Rating Factors UI
    output$project_rating_factors <- renderUI({
      project_is_selected <- isTruthy(fnrow(selected_project()) > 0)
      has_factors <- isTruthy(fnrow(factors_and_scores_for_project()) > 0)
      
      if(!isTruthy(project_is_selected) && !isTruthy(has_factors)) {
        shinyjs::hide(id = "total_row")
        shinyjs::hide(id = "weighted_total_row")
      }
      
      shiny::validate(
        need(hasProjects(), paste0("You do not have any ", funding_action, " projects to threshold. Add them on the Review tab.")),
        need(project_is_selected, "Select a project in the left-hand sidebar to begin rating")
      )
      shiny::validate(
        need(fnrow(factors_and_scores_for_project()) > 0, paste0("You must select 1 or more ", funding_action, " rating factors in the Customize Rating Criteria tab in order to rate this project"))
      )
      
      # Group data only by the main factor_group
      grouped_data <- split(factors_and_scores_for_project(), by = "factor_group")
      
      col_widths <- get_col_widths()
      # Create a main accordion panel for each group
      accordion_items_group <- purrr::map(names(grouped_data), function(group_name) {
        group_dt <- grouped_data[[group_name]]
        group_id <- gsub(" ", "_", group_name)
        group_total <- DT::coerceValue(fsum(group_dt$rating_score), 0L)
        group_max <- fsum(group_dt$max_point_value)
        
        # Now, within this group, let's create the table-like content
        # First, split by subgroup to render subgroup headers
        subgroups_in_group <- split(group_dt, by = "factor_subgroup")
        
        # Use map to generate the HTML for each subgroup and its factors
        table_content <- purrr::map(names(subgroups_in_group), function(subgroup_name) {
          
          subgroup_data <- subgroups_in_group[[subgroup_name]]
          
          # Generate the rows for each rating factor in this subgroup
          factor_rows <- purrr::pmap(
            list(
              subgroup_data$selected_rating_factor_id,
              subgroup_data$rating_factor_text, # Using the longer text here, you can choose
              subgroup_data$goal,
              subgroup_data$performance,
              subgroup_data$rating_score,
              subgroup_data$max_point_value
            ),
            function(id, text, goal, performance, rating_score, max_points) {
              # This is a single data row
              layout_columns(
                col_widths = col_widths,
                # We can add a class for CSS styling, e.g., for indentation
                HTML(text),
                p(goal),
                div(
                  class = "input-col", 
                  textAreaInput(
                    ns(paste0("performance_", id)), 
                    label = NULL, 
                    value = fcoalesce(performance, ""),
                    autoresize = TRUE,
                    rows = 1,
                    updateOn = "blur"
                  )
                ),
                div(
                  class = "input-col", 
                  shinyWidgets::autonumericInput(
                    inputId = ns(paste0("rating_score_", id)), 
                    label = NULL, 
                    value = rating_score,
                    align = "center",
                    decimalPlaces = 0,
                    minimumValue = ifelse(max_points < 0, max_points, 0),
                    maximumValue = ifelse(max_points < 0, 0, max_points)
                  )) |>
                    tagAppendAttributes(class = 'score-input', `data-group` = group_id),
                p(
                  ifelse(
                    max_points > 0, 
                    paste("out of", max_points), 
                    paste0("(deduct up to ", max_points, ")")
                  ),
                  style="padding-top: 5px;"
                )
              )
            }
          )
          
          # Combine the subgroup header and its factor rows
          # The subgroup header is just a simple div or row
          tagList(
            if (subgroup_name != "NA") {
              h5(subgroup_name, class = "subgroup-header")
              # fluidRow(
              #   column(12, h5(subgroup_name, class = "subgroup-header"))
              # )
            },
            factor_rows
          )
        })
        
        # Create the single accordion panel for the whole group
        bslib::accordion_panel(
          title = layout_columns(
            style = "margin-bottom: 0px",
            col_widths = col_widths,
            # tagList(
            htmltools::div(group_name),
            div(),
            div(),
            div(),
            htmltools::div(
              class = "accordion_total_display",
              HTML(paste0(
                "(",
                textOutput(ns(paste0("title_subtotal_", group_id)), inline = TRUE),
                " out of ", group_max, ")"
              ))
            )
          ),
          value = group_name,
          layout_columns(
            class = "rating-table-header",
            col_widths = col_widths,
            strong("RATING FACTOR"),
            strong("PERFORMANCE GOAL"),
            strong("PERFORMANCE"),
            strong("POINTS AWARDED"),
            strong("MAX POINT VALUE")
          ),
          hr(),
          # Add the content we generated above
          !!!table_content,
          # Add the subtotal row at the end of the group
          hr(),
          layout_columns(
            col_widths = c(5, -2, -2, 1, 2),
            strong(paste0(group_name, " Subtotal")),
            div(style = "text-align:center;", class = "subtotal-column", 
                strong(textOutput(ns(paste0("subtotal_", group_id)), inline = TRUE))),
            
            # div(style = "text-align:center;", class = "subtotal-column", strong(class = "subtotal-display", `data-subtotal-for` = group_id, "0")),
            div(strong(textOutput(ns(paste0("subtotal_max_", group_id)), inline=TRUE))) # Example
          )
        )
      })
      
      # Give shiny enough time to load factors before showing Total rows
      shinyjs::delay(800, {
        shinyjs::show(id = "total_row")
        shinyjs::show(id = "weighted_total_row")
      })
      
      # The final accordion structure
      bslib::accordion(
        !!!accordion_items_group,
        id = ns("main_accordion"),
        multiple = TRUE,
        open = FALSE # per HUD feedback, close by default
      )
    }) # end render factors
    
    # get all entered scores
    entered_scores <- reactive({
      vapply(
        factors_and_scores_for_project()$selected_rating_factor_id,
        function(id) {
          val <- input[[paste0("rating_score_", id)]]
          if (is.null(val) || is.na(val) || val == "") return(0)
          as.numeric(val)
        },
        numeric(1) # Forces the return to always be a clean numeric vector
      )
    })
    
    get_group_total <- function(group_data) {
      factor_ids <- group_data$selected_rating_factor_id
      
      # Grab the current values of all inputs in this specific group
      current_scores <- sapply(factor_ids, function(id) {
        val <- input[[paste0("rating_score_", id)]]
        # Treat NULL, NA, or non-numeric as 0
        if (is.null(val) || is.na(val)) 0 else as.numeric(val)
      })
      
      # Return the sum
      fsum(current_scores)
    }
    
    observeEvent(input$main_accordion, {
      req(isTruthy(fnrow(factors_and_scores_for_project()) > 0))
      lapply(factors_and_scores_for_project()$selected_rating_factor_id, function(i) {
        shinyjs::toggleState(paste0("performance_", i), condition = !input$rating_complete)
        shinyjs::toggleState(paste0("rating_score_", i), condition = !input$rating_complete)
      })
    })
    
    # Dynamically update subgroup totals
    observe({
      req(nrow(factors_and_scores_for_project()) > 0)
      
      # Get the data and split it by group, just like in your UI
      df <- factors_and_scores_for_project()
      grouped_data <- split(df, by = "factor_group")
      
      # Loop through each group
      lapply(names(grouped_data), function(group_name) {
        
        group_id <- gsub(" ", "_", group_name)
        group_data <- grouped_data[[group_name]]
        
        # Dynamically bind a renderText to the output
        # group total
        output[[paste0("subtotal_", group_id)]] <- renderText({
          get_group_total(group_data)
        })
        
        # group max
        output[[paste0("subtotal_max_", group_id)]] <- renderText({
          paste0("out of ", fsum(group_data$max_point_value))
        })
        
        # Accordion Title group total/max
        output[[paste0("title_subtotal_", group_id)]] <- renderText({
          get_group_total(group_data)
        })
      })
      
      output$total_score <- renderText({
        DT::coerceValue(fsum(entered_scores()), 0)
      })
      output$total_max <- renderText({
        paste0("out of ", fsum(factors_and_scores_for_project()$max_point_value))
      })
      
      output$weighted_total_score <- renderText({
        denominator <- fsum(factors_and_scores_for_project()$max_point_value)
        numerator <- DT::coerceValue(fsum(entered_scores()), 0)
        
        round(100 * numerator/denominator, 0)
      })
    })
    
    inputs_to_track <- reactive({
      factors <- factors_and_scores_for_project()
      req(nrow(factors) > 0)
      
      input_names <- lapply(input_prefixes, paste0, "_", factors$selected_rating_factor_id) |> unlist()
      req(all(input_names %in% names(input)))
      
      s <- lapply(input_names, function(i) {
        val <- input[[i]]
        if(is.null(val)) NA else val
      })
      names(s) <- input_names
      s
    })
    
    rating_scores_to_save <- reactive({
      raw_inputs <- inputs_to_track()
      req(raw_inputs)
      
      base <- isolate(factors_and_scores_for_project())
      proj <- isolate(selected_project())
      req(fnrow(base) > 0, fnrow(proj) > 0)
      
      updated_rating_scores <- get_rating_data_to_save(raw_inputs, base, "selected_rating_factor_id", input_prefixes)
      if(is.null(updated_rating_scores)) return(NULL)
      
      updated_rating_scores <- updated_rating_scores |>
        fmutate(
          created_by = user_coc$username,
          performance = fifelse(performance == "", NA, performance),
          project_id = proj$project_id
        ) |>
        fselect(
          project_id,
          selected_rating_factor_id, 
          rating_score, 
          performance, 
          created_by,
          version_id
        )
      
      eval_base <- isolate(project_evaluation())
      
      updated_project_evaluation <- if(!allNA(updated_rating_scores$rating_score)) {
        # Project evalaution prep
         data.table(
          project_id = proj$project_id,
          weighted_score = round(100 * fsum(entered_scores())/fsum(base$max_point_value), 0),
          created_by = user_coc$username,
          version_id = ifelse(fnrow(eval_base) > 0, eval_base$version_id, NA)
        )
      }

      list(rating_scores = updated_rating_scores, project_evaluation = updated_project_evaluation)
    }, label = "rating_scores_to_Save_reactive") |> debounce(2000)

    observeEvent(rating_scores_to_save(), {
      to_save <- rating_scores_to_save()
      
      req(to_save)
      
      refresh_flags <- pool::poolWithTransaction(get_db_pool(), function(p) {
        needs_ref1 <- update_rating_scores_db(p, to_save$rating_scores)

        needs_ref2 <- FALSE
        if (!is.null(to_save$project_evaluation))
          needs_ref2 <- update_rating_score_project_evaluation_db(p, to_save$project_evaluation)

        return(c(needs_ref1, needs_ref2))
      })

      needs_refresh <- any(unlist(refresh_flags))

      if (!needs_refresh) {
        # SUCCESS: The "Silent Update" to the baselines
        # 1. Update thresholds_to_enter baseline
        factors_and_scores_for_project()[
          to_save$rating_scores,
          on = "selected_rating_factor_id",
          `:=`(
            rating_score = as.integer(i.rating_score),
            performance = i.performance,
            version_id = version_id + 1
          )
        ]

        # 2. Update project_evaluation baseline
        if (!is.null(to_save$project_evaluation))
          project_evaluation(
            to_save$project_evaluation |>
              fmutate(version_id = version_id + 1)
          )
      } else {
        # COLLISION: Trigger full refresh
        refresh_trigger(refresh_trigger() + 1)
      }
    }) # end save
    
    mod_download_rating_server("download_rating", user_coc, selected_project, funding_action, factors_and_scores_for_project)
    
    observeEvent(input$rating_complete, {
      # only proceed if all scores are entered
      req(fnrow(project_evaluation()) > 0)
      req(isTruthy(fnrow(factors_and_scores_for_project()) > 0))
      req(fnrow(selected_project()) > 0)
      
      # Disable/Enable individual score fields
      lapply(factors_and_scores_for_project()$selected_rating_factor_id, function(i) {
        shinyjs::toggleState(paste0("performance_", i), condition = !input$rating_complete)
        shinyjs::toggleState(paste0("rating_score_", i), condition = !input$rating_complete)
      })
      
      if(isTruthy(updated_rating_complete_from_db())) {
        updated_rating_complete_from_db(FALSE)
        return(FALSE)
      }
      
      # pull latest Project Evaluation in case they just updated Threshold
      project_evaluation(
        get_project_evaluation(list(user_coc$coc_version_id, selected_project()$project_id))
      )
      
      # Update db
      data <- factors_and_scores_for_project() |>
        fmutate(
          rating_complete = input$rating_complete,
          updated_by = user_coc$username,
          version_id = project_evaluation()$version_id
        ) |>
        fselect(rating_complete, updated_by, project_id, version_id) |>
        funique()
      
      update_rating_complete(get_db_pool(), data)
      
      status <- calculate_coc_status(user_coc$coc_version_id)
      update_coc_status(user_coc, status)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
    # --- User PResence ----
    mod_user_presence_server(
      id = "presence", # Internal ID for this leaf module
      user_coc = user_coc,
      # We use the project ID because we are rating a specific project
      record_id = reactive({ selected_project()$project_id }), 
      active = active
    )
  }) #end module server
}
