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
        class="sticky-save",
        style = "display: flex; justify-content: space-between; align-items: center;",
        actionButton(ns("save_rating"), "Save Rating", icon = icon("save"))
      )
    )
  )
}

mod_rating_scores_entry_server <- function(id, user_coc, selected_project, active) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    refresh_trigger <- reactiveVal(NA)
    factors_and_scores_for_project <- reactiveVal()
    project_evaluation <- reactiveVal()
    
    performance_char_limit <- get_db_column_limit("rating_scores","performance")
    
    
    observeEvent(c(selected_project(), refresh_trigger(), user_coc$customized_rating_factors_updated), {
      req(user_coc$coc_version_id)
      req(selected_project())
      req(fnrow(selected_project()) > 0)
      
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
          user_coc$coc_version_id, 
          selected_project()$project_id
        )
      )
    })
    
    # ---------------------------------------------------------------------------
    # InputValidator — single instance created once; rules are rebuilt whenever
    # the factor data changes (e.g. project switch, db refresh). iv$initialize()
    # wipes all previously registered rules without invalidating the object
    # reference, so the save observeEvent below can safely hold a closure over
    # `iv` without needing to be recreated itself.
    # ---------------------------------------------------------------------------
    iv <- shinyvalidate::InputValidator$new()
    
    observe({
      factors_df <- factors_and_scores_for_project()
      req(nrow(factors_df) > 0)
      
      # Wipe all previously registered rules before re-registering.
      iv$initialize()
      purrr::walk2(
        factors_df$selected_rating_factor_id,
        factors_df$max_point_value,
        function(id, max_pts) {
          # Input IDs must be fully namespaced when registering rules outside
          # of renderUI, because InputValidator operates at the session level.
          iv$add_rule(
            paste0("rating_score_", id),
            ~ {
              if (is.null(.) || is.na(.)) return(NULL)
              if (!is.numeric(.) || . < -999.9 || . > max_pts) 
                return(paste0("Score must be a number between 0 and ", max_pts, "."))
            }
          )
          
          iv$add_rule(
            paste0("performance_", id),
            ~ {
              # nchar(NULL) is integer(0), so we check for truthiness first
              if (!is.null(.) && nchar(.) > performance_char_limit) {
                return(paste0("Maximum length is ", performance_char_limit, " characters (currently ", nchar(.), ")."))
              }
            }
          )
        }
      )
      
      # enable() activates live validation — errors appear as the user types,
      # not only on save. Call it after rules are registered so the first
      # render doesn't flash errors on blank inputs immediately.
      iv$enable()
    })
    
    # Project Rating Factors UI
    output$project_rating_factors <- renderUI({
      selected_project_exists <- !is.null(selected_project()) && fnrow(selected_project()) > 0
      
      if(!selected_project_exists) {
        shinyjs::hide(id = "total_row")
        shinyjs::hide(id = "weighted_total_row")
      }
      
      shiny::validate(need(
        selected_project_exists,
        "Select a project in the left-hand sidebar to begin rating"
      ))

      req(nrow(factors_and_scores_for_project()) > 0)
      
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
                p(text),
                p(goal),
                div(
                  class = "input-col", 
                  textAreaInput(
                    ns(paste0("performance_", id)), 
                    label = NULL, 
                    value = performance,
                    autoresize = TRUE,
                    rows = 1
                  ) # |> 
                    #shiny::tagAppendAttributes(maxlength = performance_char_limit)
                ),
                div(
                  class = "input-col", 
                  numericInput(
                    ns(paste0("rating_score_", id)), 
                    label = NULL, 
                    value = rating_score,
                    min = 0,
                    max = max_points
                  )) |>
                    tagAppendAttributes(class = 'score-input', `data-group` = group_id),
                p(paste("out of", max_points), style="padding-top: 5px;")
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
          title = tagList(
            htmltools::span(group_name),
            htmltools::span(
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
        open = names(grouped_data)[1] # Open the first group by default
      )
    }) # end render factors
    
    # get all entered scores
    entered_scores <- reactive({
      sapply(
        factors_and_scores_for_project()$selected_rating_factor_id,
        \(id) input[[paste0("rating_score_", id)]]
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
    
    
    # Disable save_rating if any invalid responses
    observe({
      # for reactive dependency on the inputs so we can ensure this triggers.
      req(entered_scores())
      shinyjs::toggleState("save_rating", condition = iv$is_valid())
    })
    
    get_updated_project_evaluation <- function(params) {
      data.table(
        project_id = params$project_id,
        weighted_score = params$weighted_score,
        created_by = params$username,
        version_id = params$version_id
      )
    }
    
    observeEvent(input$save_rating, {
      # rating scores prep
      df <- factors_and_scores_for_project()
      selected_ids <- df$selected_rating_factor_id
      num_selected <- fnrow(df)
      
      updated_rating_scores <- list(
        project_ids                 = alloc(selected_project()$project_id, num_selected),
        selected_rating_factor_ids  = selected_ids,
        rating_scores               = sapply(selected_ids, \(id) input[[paste0("rating_score_", id)]]),
        performances                = sapply(selected_ids, \(id) input[[paste0("performance_", id)]]),
        created_bys                 = alloc(user_coc$username, num_selected),
        version_id                = df$version_id
      )
      
      
      # Project evalaution prep
      numerator <- fsum(entered_scores())
      denominator <- fsum(df$max_point_value)
      
      updated_project_evaluation <- get_updated_project_evaluation(
        list(
          project_id =  selected_project()$project_id,
          weighted_score = round(100 * numerator/denominator, 0),
          username = user_coc$username,
          version_id = ifelse(fnrow(project_evaluation()) > 0, project_evaluation()$version_id, NA)
        )
      )
      
      needs_refresh1 <- FALSE
      needs_refresh2 <- FALSE
      pool::poolWithTransaction(get_db_pool(), function(p) {
        needs_refresh1 <- update_rating_scores_db(p, updated_rating_scores)
        needs_refresh2 <- update_rating_score_project_evaluation_db(p, updated_project_evaluation)
      })

      # if(needs_refresh1 || needs_refresh2)
        refresh_trigger(refresh_trigger() + 1)
    })
    
    # --- User PResence ----
    mod_user_presence_server(
      id = ns("presence"), # Internal ID for this leaf module
      user_coc = user_coc,
      # We use the project ID because we are rating a specific project
      record_id = reactive({ selected_project()$project_id }), 
      active = active
    )
  }) #end module server
}
