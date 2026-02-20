#' @title mod_new_factors_ui
#' @noRd
mod_customize_rating_factors_ui <- function(id, funding_action) {
  ns <- NS(id)
  
  display_funding_action = ifelse(funding_action == "Renew", "Renewal/Expansion", "New")
  
  project_and_pop_dropdowns <- function(ns) {
    project_type_dropdown <- selectInput(
      inputId = ns("project_type"),
      label = "Select project type",
      choices = get_labelled_lookups("project_type")[MAIN_PROJECT_TYPES],
      multiple = TRUE,
      selected = MAIN_PROJECT_TYPES # Pre-select all for initial state
    )
    
    target_pop_dropdown <- selectInput(
      inputId = ns("target_population"),
      label = "Select special populations",
      choices = get_labelled_lookups("target_population")[c("DV", "General")],
      multiple = TRUE,
      selected = c("DV", "General") # Pre-select all for initial state
    )
    
    dropdowns_to_include <- target_pop_dropdown
    if(funding_action == "Renew") dropdowns_to_include <- list(project_type_dropdown, dropdowns_to_include)
    inner_layout_args <- c(
      # if Renew, each dropdown takes half of this 8-column space. If New, it's just one column taking up the whole space
      width = ifelse(funding_action == "Renew", 1/2, 1),
      dropdowns_to_include
    )
    
    dropdowns_to_include
  }
  
  nav_panel(
    paste0(display_funding_action, " Rating Factors"),
    value = id,
    layout_sidebar(
      sidebar = sidebar(
        width = "10%",
        project_and_pop_dropdowns(ns)
      ),
      card(
        uiOutput(ns("factors_ui")) |> withSpinner(),
        card_footer(
          style = "display: flex; justify-content: space-between; align-items: center;",
          actionButton(ns("add_custom_factor"), "Add Custom Rating Factor", icon = icon("plus")),
          actionButton(ns("save_factors"), paste0("Save ", display_funding_action, " Criteria"), icon = icon("save"), class = "btn-primary")
        )
      )
    )
  )
}

#' @title mod_new_factors_server
#' @noRd
mod_customize_rating_factors_server <- function(id, user_coc, funding_action, module_returns) {
  # The server logic here is identical in structure to the renewal/expansion module,
  # differing only by the `funding_action` filter ('New').
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Counter for unique IDs for custom factor rows
    custom_factor_counter <- reactiveVal(0)
    # Store observers for remove buttons to manage them
    custom_factor_observers <- reactiveValues()
    subgroup_check_all_values <- reactiveValues()

    fetch_and_structure_rating_factors <- function(funding_action_type, coc_version_id, selected_target_populations = NULL, selected_project_types = NULL) {
      # Determine the WHERE clause based on the funding_action_type
      funding_action_values <- switch(
        funding_action_type,
        "Renew" = c("Renew", "Expand"),
        "New" = c("New"),
        stop("Invalid funding_action_type provided. Must be 'Renew' or 'New'.")
      )
      
      # Fetch all possible factors for the given action type
      # Use glue::glue for easy SQL string interpolation
      all_factors_q <- "
        SELECT rf.rating_factor_id, rf.funding_action, rf.project_type, rf.target_population, rf.rating_factor_text, rf.goal AS default_goal,
               rf.max_point_value AS default_points, fg.factor_group, fsg.factor_subgroup
        FROM rating_factors rf
        JOIN factor_groups fg ON rf.factor_group = fg.factor_group_id
        LEFT JOIN factor_subgroups fsg ON rf.factor_subgroup = fsg.factor_subgroup_id
        JOIN lookups fa ON rf.funding_action = fa.reference_id
        WHERE fa.reference_type = 'funding_action' AND fa.value IN ($1)
      "
      
      user_selected_factors <- get
      all_factors <- get_db_query(all_factors_q, params = list(funding_action_values))
      if(!is.null(selected_project_types)) all_factors <- all_factors[project_type %in% selected_project_types]
      if(!is.null(selected_target_populations)) all_factors <- all_factors[target_population %in% selected_target_populations]
      
      # Update rating_factor_text to include project_type and target_population
      # all_factors <- all_factors |>
      #   join(
      #     lookups[reference_type == "project_type", .("project_type_value" = value, reference_id)], 
      #     on = c("project_type" = "reference_id")
      #   ) |>
      #   join(
      #     lookups[reference_type == "target_population", .("target_population_value" = value, reference_id)], 
      #     on = c("target_population" = "reference_id")
      #   )  |>
      # fmutate(
      #   rating_factor_text = fifelse(
      #     !is.na(project_type_value) & !is.na(target_population_value),
      #     glue::glue("{project_type_value} ({target_population_value}) - {rating_factor_text}"),
      #     fifelse(
      #       !is.na(project_type_value),
      #       glue::glue("{project_type_value} - {rating_factor_text}"),
      #       fifelse(
      #         !is.na(target_population_value),
      #         glue::glue("{target_population_value} - {rating_factor_text}"),
      #         rating_factor_text
      #       )
      #     )
      #   )
      # )
      
      selected_factors_q <- "
        SELECT rating_factor_id, goal, max_point_value
        FROM selected_rating_factors
        WHERE coc_version_id = $1
      "
      selected_factors <- get_db_query(selected_factors_q, params = list(coc_version_id))
      
      # Merge them to get the final state for the UI
      if (nrow(selected_factors) > 0) {
        merged_data <- join(
          all_factors, 
          selected_factors |> fmutate(selected = TRUE), 
          on = "rating_factor_id"
        ) |>
          fmutate(
            selected = fcoalesce(selected, FALSE),
            goal = fcoalesce(goal, default_goal),
            max_point_value = fcoalesce(as.double(max_point_value), as.double(default_points))
          )
      } else {
        merged_data <- all_factors |>
          frename(goal = default_goal, max_point_value = default_points) |>
          fmutate(selected = FALSE)
      }
      
      # Create a nested list: Group -> Subgroup -> Factors
      nested_data <- list()
      unique_groups <- unique(merged_data$factor_group)
      
      for (group_name in unique_groups) {
        group_dt <- merged_data[factor_group == group_name]
        nested_data[[group_name]] <- split(group_dt, by = "factor_subgroup")
      }
      
      return(nested_data)
    }
    
    render_nested_factor_accordion_ui <- function(ns, funding_action = "Renew", data_groups_nested, placeholder_text = "No rating factors found.") {
      if (length(data_groups_nested) == 0) {
        return(p(placeholder_text))
      }
      
      accordion_items_group <- purrr::map(names(data_groups_nested), function(group_name) {
        group_data_subgroups <- data_groups_nested[[group_name]]
        
        sub_accordion_items <- purrr::map(names(group_data_subgroups), function(subgroup_name) {
          subgroup_data <- group_data_subgroups[[subgroup_name]]
          
          factor_rows <- purrr::pmap(
            list(
              subgroup_data$rating_factor_id, 
              subgroup_data$project_type, 
              subgroup_data$target_population, 
              subgroup_data$rating_factor_text, 
              subgroup_data$goal, 
              subgroup_data$max_point_value, 
              subgroup_data$selected
            ), function(id, project_type, target_population, text, goal, points, selected) {
              fluidRow(
                column(1, checkboxInput(ns(paste0("select_", id)), label = NULL, value = selected)),
                if(funding_action == "Renew") column(1, p(get_lookup_label(project_type, ref_type = "project_type"))),
                if(funding_action == "Renew") column(1, p(get_lookup_label(target_population, ref_type = "target_population"))),
                column(ifelse(funding_action == "Renew", 7, 9), p(text)),
                column(1, textInput(ns(paste0("goal_", id)), label = NULL, value = goal)),
                column(1, numericInput(ns(paste0("points_", id)), label = NULL, value = points, step = 1))
              )
            }
          )
          
          all_subgroup_factors_selected <- nrow(subgroup_data) == nrow(subgroup_data[selected == TRUE])
          bslib::accordion_panel(
            title = ifelse(subgroup_name == "NA", "", subgroup_name),
            fluidRow(
              column(1,
                     tags$b("Use in rating?"),
                     checkboxInput(
                       ns(make.names(paste0(group_name, "_check_all_", subgroup_name))),
                       label = NULL,
                       value = all_subgroup_factors_selected
                     )
              ),
              if(funding_action == "Renew") column(1, tags$b("Project Type")),
              if(funding_action == "Renew") column(1, tags$b("Target Population", style="word-wrap: normal;")),
              column(ifelse(funding_action == "Renew", 7, 9), tags$b("Rating Factor")),
              column(1, tags$b("Factor/Goal", style="word-wrap: normal;")),
              column(1, tags$b("Max Point Value"))
            ),
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
    }
    
    # project_and_pop_dropdowns <- function(ns, funding_action) {
    #   project_type_dropdown <- selectInput(
    #     inputId = ns("project_type"),
    #     label = "Select project type",
    #     choices = get_labelled_lookups("project_type")[MAIN_PROJECT_TYPES],
    #     multiple = TRUE,
    #     selected = MAIN_PROJECT_TYPES # Pre-select all for initial state
    #   )
    #   
    #   target_pop_dropdown <- selectInput(
    #     inputId = ns("target_population"),
    #     label = "Select special populations",
    #     choices = get_labelled_lookups("target_population")[c("DV", "General")],
    #     multiple = TRUE,
    #     selected = c("DV", "General") # Pre-select all for initial state
    #   )
    #   
    #   dropdowns_to_include <- target_pop_dropdown
    #   if(funding_action == "Renew") dropdowns_to_include <- list(project_type_dropdown, dropdowns_to_include)
    #   inner_layout_args <- c(
    #     # if Renew, each dropdown takes half of this 8-column space. If New, it's just one column taking up the whole space
    #     width = ifelse(funding_action == "Renew", 1/2, 1),
    #     dropdowns_to_include
    #   )
    #   bslib::layout_column_wrap(
    #     width = 1/3,
    #     div(), # left spacer
    #     do.call(bslib::layout_column_wrap, inner_layout_args),
    #     div() # right spacer
    #   )
    # }
    # 
    handle_check_all_box_functionality <- function(input) {
      # 1. Fetch ALL possible subgroup names ONCE at the start.
      #    This decouples observer creation from the reactive data flow.
      #    We query the source table directly for this static list.
      funding_action_id <- get_lookup_refid(funding_action, "funding_action")
      
      all_possible_subgroups <- get_db_query(
        "SELECT sg.factor_subgroup, fg.factor_group
          FROM factor_subgroups sg
          RIGHT JOIN factor_groups fg ON fg.factor_group_id = sg.factor_group
          WHERE fg.funding_action = $1
        ", 
        params = funding_action_id
      )

      # PARENT -> CHILDREN
      # observe changes to check-all boxes
      lapply(seq_row(all_possible_subgroups), function(i) {
        group <- all_possible_subgroups$factor_group[i]
        subgroup <- all_possible_subgroups$factor_subgroup[i]
        subgroup_check_all_input <- make.names(paste0(group, "_check_all_", subgroup))
        
        observeEvent(input[[subgroup_check_all_input]], {
          new_val <- input[[subgroup_check_all_input]]

          if (is.null(new_val)) return()
          
          stored_val <- isolate(subgroup_check_all_values[[subgroup_check_all_input]])
          is_initialized <- !is.null(stored_val)
          
          # Only update children if the user clicked (value changed from what we last recorded)
          if(!identical(new_val, stored_val)) {
            subgroup_check_all_values[[subgroup_check_all_input]] <- new_val
            subgroup_factors <- isolate(selected_factors_data())[[group]][[toString(subgroup)]]
            
            lapply(subgroup_factors$rating_factor_id, function(factor_id) {
              checkbox_id <- paste0("select_", factor_id)
              if (!identical(input[[checkbox_id]], new_val)) {
                updateCheckboxInput(session, checkbox_id, value = new_val)
              }
            })
          }
          
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
      
      # CHILDREN -> PARENT
      # Update subgroup check-all-that-apply boxes based on underlying factor boxes ------
      observe({
        data <- selected_factors_data()
        req(data)
        # This part checks the children and updates the parent "check all" box.
        
        # Loop through only the groups and subgroups currently visible on the UI.
        for (i in seq_along(data)) {
          group_data <- data[[i]]
          group_name <- names(data)[i]
          for (subgroup_name in names(group_data)) {
            
            subgroup_data <- group_data[[subgroup_name]]
            factor_ids <- subgroup_data$rating_factor_id
            
            # Read the current values of all child factor checkboxes for this subgroup.
            # The `req(input[[...]])` is crucial to prevent this from running before
            # the child checkboxes are rendered and available in the `input` object.
            factor_selections <- sapply(factor_ids, function(id) input[[paste0("select_", id)]])
            if (any(sapply(factor_selections, is.null))) next
            
            # message(paste0("Selected factors for ", subgroup_name, ": ", paste0(factor_selections, collapse=", ")))
            # Determine the new state for the parent "check all" box.
            # It should be checked if and only if all its children are checked.
            parent_should_be_checked <- all(unlist(factor_selections))
            
            # Get the ID of the parent checkbox
            subgroup_check_all_input <- make.names(paste0(group_name, "_check_all_", subgroup_name))

            # Update the parent checkbox ONLY if its state needs to change.
            # This avoids unnecessary updates and potential infinite loops.
            if (!identical(isolate(input[[subgroup_check_all_input]]), parent_should_be_checked)) {
              # CRITICAL: Also update our memory so that this programmatic change
              # doesn't re-trigger the `check_all -> factors` logic above.
              subgroup_check_all_values[[subgroup_check_all_input]] <- parent_should_be_checked
              
              updateCheckboxInput(session, subgroup_check_all_input, value = parent_should_be_checked)
            }
          }
        }
      })
    }
    
    
    # Function to generate the UI for a single custom factor row
    create_custom_factor_row_ui <- function(ns, row_id, funding_action) {
      # Use a unique ID for the row's wrapper div for easy removal
      row_div_id <- ns(paste0("custom_row_", row_id))
      
      # Define the namespaced input IDs
      pt_input_id <- ns(paste0("custom_pt_", row_id))
      tp_input_id <- ns(paste0("custom_tp_", row_id))
      
      div(
        id = row_div_id,
        fluidRow(
          style = "padding-top: 10px; border-top: 1px solid #eee;",
          column(1, checkboxInput(ns(paste0("custom_select_", row_id)), label = NULL, value = TRUE)),
          column(1, if(funding_action == "Renew") 
            selectInput(
              inputId = pt_input_id,
              label = NULL,
              choices = get_labelled_lookups("project_type")[MAIN_PROJECT_TYPES],
              multiple = TRUE,
              selected = MAIN_PROJECT_TYPES # Pre-select all for initial state
            ) else NULL
          ),
          column(1, selectInput(
            inputId = tp_input_id,
            label = NULL,
            choices = get_labelled_lookups("target_population")[c("DV", "General")],
            multiple = TRUE,
            selected = c("DV", "General") # Pre-select all for initial state
          )),
          column(7, textInput(ns(paste0("custom_text_", row_id)), label = NULL, placeholder = "Enter custom factor text")),
          column(1, textInput(ns(paste0("custom_goal_", row_id)), label = NULL, placeholder = "Enter goal")),
          column(1, 
                 div(style="display:flex; align-items:center; gap:5px;",
                     numericInput(ns(paste0("custom_points_", row_id)), label = NULL, value = 0, step = 1),
                     actionButton(ns(paste0("remove_custom_", row_id)), "", icon = icon("trash-alt"), class = "btn-sm btn-danger", style="margin-bottom: 1rem;") # margin-bottom matches container div
                 )
          )
        )
      )
    }
    
    add_custom_factor <- function(ns, input) {
      # Increment counter
      current_id <- custom_factor_counter() + 1
      custom_factor_counter(current_id)
      
      bslib::accordion_panel_open(
        id = "main_accordion", 
        values = "Other and Local Criteria"
      )
      
      # Insert the new UI row
      insertUI(
        selector = paste0("#", ns("custom_factors_placeholder")),
        where = "beforeEnd",
        ui = create_custom_factor_row_ui(ns, current_id, funding_action)
      )
      
      # Focus on the first text input of the new row
      pt_input_id_js <- ns(paste0("custom_pt_", current_id))
      shinyjs::runjs(sprintf("$('#%s').focus();", pt_input_id_js))
      
      # Create and store an observer for the new "Remove" button
      remove_btn_id <- paste0("remove_custom_", current_id)
      custom_factor_observers[[remove_btn_id]] <- observeEvent(input[[remove_btn_id]], {
        removeUI(selector = paste0("#", ns(paste0("custom_row_", current_id))))
        # Destroy this observer to prevent memory leaks
        custom_factor_observers[[remove_btn_id]]$destroy()
      }, ignoreInit = TRUE, once = TRUE) # `once = TRUE` is crucial
    }
    
    save_factors <- function(ns, input) {
      # 1. Get all factor IDs that were rendered on the UI.
      all_ids <- rbindlist(
        unlist(selected_factors_data(), recursive = FALSE), 
        use.names = TRUE, 
        fill = TRUE
      )$rating_factor_id
      
      # Give user immediate feedback
      showNotification(
        "Saving criteria...", 
        duration = NULL, 
        id = ns("saving_msg"),
        type = "message"
      )
      
      # 2. Collect the current state from the UI into a data.table
      ui_state <- purrr::map_df(all_ids, function(id) {
        data.table(
          rating_factor_id = id,
          is_selected = isTRUE(input[[paste0("select_", id)]]),
          goal = as.character(input[[paste0("goal_", id)]]),
          max_point_value = as.numeric(input[[paste0("points_", id)]])
        )
      })
      
      # 3. Get the current state from the database
      db_factors <- get_db_query(
        "SELECT rating_factor_id FROM selected_rating_factors WHERE coc_version_id = $1",
        params = list(user_coc$coc_version_id)
      )
      db_ids <- if(nrow(db_factors) > 0) db_factors$rating_factor_id else integer(0)
      
      # 4. Determine what needs to be inserted, updated, or deleted
      to_insert <- ui_state[is_selected & !(rating_factor_id %in% db_ids)]
      to_update <- ui_state[is_selected & rating_factor_id %in% db_ids]
      to_delete_ids <- ui_state[!is_selected & rating_factor_id %in% db_ids, rating_factor_id]
      
      # 8. Wrap all database operations in a transaction for atomicity
      # This assumes you have a helper function `poolWithTransaction`.
      # If not, you would use DBI::dbBegin, tryCatch, DBI::dbCommit/dbRollback here.
      tryCatch({
        pool::poolWithTransaction(DB_POOL, function(p) {
          
          # 7. DELETE records that were deselected
          if (length(to_delete_ids) > 0) {
            dbExecute(p, glue::glue_sql("
              DELETE FROM selected_rating_factors
              WHERE coc_version_id = {user_coc$coc_version_id} AND rating_factor_id IN ({to_delete_ids*})
            ", .con = p))
          }
          
          # 5. INSERT new records that are now selected
          if (nrow(to_insert) > 0) {
            dbAppendTable(p,
              "selected_rating_factors",
              to_insert |> fmutate(coc_version_id = user_coc$coc_version_id, is_selected = NULL)
            )
          }
          
          # 6. UPDATE existing records
          if (nrow(to_update) > 0) {
            update_q <- "
              UPDATE selected_rating_factors
              SET goal = $1, max_point_value = $2
              WHERE coc_version_id = $3 AND rating_factor_id = $4
            "
            params_list <- to_update |>
              fmutate(coc_version_id = user_coc$coc_version_id) |>
              fselect(goal, max_point_value, coc_version_id, rating_factor_id) |>
              as.list() |>
              unname()
            
            dbExecute(p, update_q, params = params_list)
          }
          
          if (custom_factor_counter() > 0) {
            num_custom_factors <- custom_factor_counter()
            funding_action_id <- get_lookup_refid(funding_action, "funding_action")
            
            other_factor_group_id <- DBI::dbGetQuery(p, "
              SELECT factor_group_id 
              FROM factor_groups
              WHERE factor_group = 'Other and Local Criteria' AND funding_action = $1
            ", params = funding_action_id)
            
            custom_factors_to_insert <- list()
            
            for (i in 1:num_custom_factors) {
              if (!is.null(input[[paste0("custom_pt_", i)]])) {
                custom_factors_to_insert[[length(custom_factors_to_insert) + 1]] <- list(
                  funding_action_id,
                  input[[paste0("custom_pt_", i)]],
                  input[[paste0("custom_tp_", i)]],
                  input[[paste0("custom_text_", i)]],
                  other_factor_group_id$factor_group_id,
                  is_selected = isTRUE(input[[paste0("custom_select_", i)]]),
                  goal = input[[paste0("custom_goal_", i)]],
                  points = input[[paste0("custom_points_", i)]]
                )
              }
            }
            
            # Batch insert custom factors
            if (length(custom_factors_to_insert) > 0) {
              # Insert all factors and get their IDs back
              insert_query <- "
                INSERT INTO rating_factors (funding_action, project_type, target_population, rating_factor_text, factor_group) 
                VALUES ($1, $2, $3, $4, $5) 
                RETURNING rating_factor_id
              "
              
              # params come from custom_factors_to_insert above
              params_factors <- lapply(custom_factors_to_insert, function(x) {
                x[1:5]
              })
              
              new_factor_ids <- dbGetQuery(p, insert_query, params = params_factors)$rating_factor_id
              
              # Prepare selected_rating_factors batch insert
              selections_to_insert <- data.frame()
              for (i in seq_row(custom_factors_to_insert)) {
                if (custom_factors_to_insert[[i]]$is_selected) {
                  selections_to_insert <- rbind(
                    selections_to_insert,
                    data.frame(
                      coc_version_id = user_coc$coc_version_id,
                      rating_factor_id = new_factor_ids[i],
                      goal = custom_factors_to_insert[[i]]$goal,
                      max_point_value = custom_factors_to_insert[[i]]$points
                    )
                  )
                }
              }
              
              # Batch insert selections
              if (nrow(selections_to_insert) > 0) {
                dbAppendTable(p, "selected_rating_factors", selections_to_insert)
              }
            }
          }
        }) # End pool::poolWithTransaction
        
        removeNotification(ns("saving_msg"))
        showNotification("Criteria saved successfully!")
        
        custom_factor_counter(0)
      }, error = function(e) {
        # Log the error for debugging
        removeNotification(ns("saving_msg"))
        showNotification(
          paste("Error saving criteria:", e$message),
          type = "error",
          duration = 10 # Keep error message on screen longer
        )
        cat("Database save error:", e$message, "\n")
        # Return the error object or FALSE
        e
      }) 
    }
    
    
    selected_factors_data <- reactive({
      req(user_coc$coc_version_id)

      fetch_and_structure_rating_factors(
        funding_action, 
        user_coc$coc_version_id, 
        selected_target_populations = input$target_population,
        selected_project_types = input$project_type
      )
    })
    
    output$factors_ui <- renderUI({ # Assuming you have a UI output for 'new' factors
      data_groups_nested <- selected_factors_data()

      render_nested_factor_accordion_ui(
        ns = ns,
        funding_action = funding_action,
        data_groups_nested = data_groups_nested,
        placeholder_text = "No rating factors found"
      )
    })
    
    handle_check_all_box_functionality(input)
    
    # Observer for the "Add Custom Rating Factor" button
    # This just adds UI for the user to enter a new factor
    # It doesn't actually save until they click Save
    observeEvent(input$add_custom_factor, {
      add_custom_factor(ns, input)
    }, ignoreInit = TRUE)
    
    observeEvent(input$save_factors, {
      save_factors(ns, input)
      module_returns$customize_rating_criteria <- TRUE
    }, ignoreInit = TRUE)
  })
}
