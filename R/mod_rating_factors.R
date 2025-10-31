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
  # all_factors <- all_factors %>%
  #   join(
  #     lookups[reference_type == "project_type", .("project_type_value" = value, reference_id)], 
  #     on = c("project_type" = "reference_id")
  #   ) %>%
  #   join(
  #     lookups[reference_type == "target_population", .("target_population_value" = value, reference_id)], 
  #     on = c("target_population" = "reference_id")
  #   )  %>%
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
      selected_factors %>% fmutate(selected = TRUE), 
      on = "rating_factor_id"
    ) %>%
      fmutate(
        selected = fcoalesce(selected, FALSE),
        goal = fcoalesce(goal, default_goal),
        max_point_value = fcoalesce(as.double(max_point_value), default_points)
      )
  } else {
    merged_data <- all_factors %>%
      frename(goal = default_goal, max_point_value = default_points) %>%
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
        title = fcoalesce(subgroup_name, ""),
        fluidRow(
          column(1,
                 tags$b("Use in rating?"),
                 checkboxInput(
                   ns(paste0("check_all_", subgroup_name)),
                   label = NULL,
                   value = all_subgroup_factors_selected
                 )
          ),
          if(funding_action == "Renew") column(1, tags$b("Project Type")),
          if(funding_action == "Renew") column(1, tags$b("Target Population")),
          column(ifelse(funding_action == "Renew", 7, 9), tags$b("Rating Factor")),
          column(1, tags$b("Factor/Goal")),
          column(1, tags$b("Max Point Value"))
        ),
        hr(),
        factor_rows
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

#-------------------------------------------------------------------------------
# 3. RENEWAL/EXPANSION Project Factors Sub-Module
# - Displays rating factors applicable to Renewal and Expansion projects.
# - Allows customization of 'Goal' and 'Max Point Value'.
#-------------------------------------------------------------------------------
project_and_pop_dropdowns <- function(ns, funding_action) {
  project_type_dropdown <- selectInput(
    inputId = ns("project_type"),
    label = "Select project type",
    choices = get_labelled_lookups("project_type")[main_project_types],
    multiple = TRUE,
    selected = main_project_types # Pre-select all for initial state
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
  bslib::layout_column_wrap(
    width = 1/3,
    div(), # left spacer
    do.call(bslib::layout_column_wrap, inner_layout_args),
    div() # right spacer
  )
}
#' @title mod_renewal_factors_ui
#' @noRd
mod_renewal_factors_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Renewal/Expansion Project Rating Criteria",
    value = id,
    card(
      project_and_pop_dropdowns(ns, "Renew"),
      hr(),
      uiOutput(ns("renewal_factors_ui")) %>% withSpinner(),
      actionButton(ns("save_renewal_factors"), "Save Renewal/Expansion Criteria", icon = icon("save"), class = "btn-primary")
    )
  )
}

#' @title mod_renewal_factors_server
#' @noRd
mod_renewal_factors_server <- function(id, user_coc, selected_project_types, selected_target_populations) {
  # This server function would be nearly identical to the new_factors_server,
  # but with a different filter on `funding_action`. A helper function could be
  # created to avoid code duplication. For clarity here, it is written out.
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    subgroup_check_all_values <- reactiveValues()
    
    renewal_expand_factors_data <- reactive({
      req(user_coc$coc_version_id)
      
      fetch_and_structure_rating_factors(
        "Renew", 
        user_coc$coc_version_id, 
        selected_target_populations = input$target_population,
        selected_project_types = input$project_type
      )
    })
    
    # Dynamically render the UI for rating factors using nested accordions
    output$renewal_factors_ui <- renderUI({
      data_groups_nested <- renewal_expand_factors_data() # This now returns the nested list
      if (length(data_groups_nested) == 0) return(p("No rating factors found for Renewal/Expansion projects."))
      
      render_nested_factor_accordion_ui(
        ns = ns,
        funding_action = "Renew",
        data_groups_nested = data_groups_nested,
        placeholder_text = "No rating factors found for Renewal/Expansion projects."
      )
    })
    
    # 1. Fetch ALL possible subgroup names ONCE at the start.
    #    This decouples observer creation from the reactive data flow.
    #    We query the source table directly for this static list.
    all_possible_subgroups <- get_db_query("SELECT DISTINCT factor_subgroup FROM factor_subgroups")$factor_subgroup
    
    lapply(all_possible_subgroups, function(subgroup) {
      subgroup_check_all_input <- paste0("check_all_", subgroup)
      observeEvent(input[[subgroup_check_all_input]], {
        
        # When triggered by a user click, get the CURRENT state of the data
        val <- input[[subgroup_check_all_input]]
        message(paste0("handling ", subgroup, " check box"))
        if (is.null(val)) return()
        message(paste0(subgroup, " check box is not null!"))
        
        stored_val <- subgroup_check_all_values[[subgroup_check_all_input]]
        is_initialized <- !is.null(stored_val)
        
        if(!identical(val, stored_val) && is_initialized) {
          # Find the factor IDs for this specific subgroup from the current data
          factor_ids_to_update <- c()
          for (group in renewal_expand_factors_data()) {
            # Check if the clicked subgroup exists in this group for the current filters
            if (subgroup %in% names(group)) {
              factor_ids_to_update <- group[[subgroup]]$rating_factor_id
              break # Found it, no need to check other groups
            }
          }
          message("updating individual checkboxes")
          lapply(factor_ids_to_update, function(factor_id) {
            updateCheckboxInput(session, paste0("select_", factor_id), value = val)
          })
        }
        
        subgroup_check_all_values[[subgroup_check_all_input]] <- val
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    })
    
    observe({
      current_data <- renewal_expand_factors_data()
      req(current_data)
      
      # This part checks the children and updates the parent "check all" box.
      
      # Loop through only the groups and subgroups currently visible on the UI.
      for (group in current_data) {
        for (subgroup_name in names(group)) {
          
          subgroup_data <- group[[subgroup_name]]
          factor_ids <- subgroup_data$rating_factor_id
          
          # Read the current values of all child factor checkboxes for this subgroup.
          # The `req(input[[...]])` is crucial to prevent this from running before
          # the child checkboxes are rendered and available in the `input` object.
          factor_selections <- lapply(factor_ids, function(id) input[[paste0("select_", id)]])
          if(is.null(unlist(factor_selections))) next
          
          message(paste0("Selected factors for ", subgroup_name, ": ", paste0(factor_selections, collapse=", ")))
          # Determine the new state for the parent "check all" box.
          # It should be checked if and only if all its children are checked.
          parent_should_be_checked <- all(unlist(factor_selections))
          
          # Get the ID of the parent checkbox
          subgroup_check_all_input <- paste0("check_all_", subgroup_name)
          
          # Update the parent checkbox ONLY if its state needs to change.
          # This avoids unnecessary updates and potential infinite loops.
          if (!identical(input[[subgroup_check_all_input]], parent_should_be_checked)) {
            updateCheckboxInput(session, ns(subgroup_check_all_input), value = parent_should_be_checked)
            
            # CRITICAL: Also update our memory so that this programmatic change
            # doesn't re-trigger the `check_all -> factors` logic above.
            subgroup_check_all_values[[subgroup_check_all_input]] <- parent_should_be_checked
          }
        }
      }
    })
    
    
    # Save Logic for Renewal/Expansion Factors
    observeEvent(input$save_renewal_factors, {
      # Ensure we have the necessary ID to save against
      req(user_coc$coc_version_id)
      
      # 1. Get all factor IDs that were rendered on the UI.
      all_ids <- rbindlist(
        unlist(renewal_expand_factors_data(), recursive = FALSE), 
        use.names = TRUE, 
        fill = TRUE
      )$rating_factor_id
      if (length(all_ids) == 0) {
        removeNotification(ns("saving_msg"))
        showNotification("No factors to save.", type = "warning")
        return()
      }
      
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
        DBI::dbWithTransaction(DB_CON, {
          
          # 7. DELETE records that were deselected
          if (length(to_delete_ids) > 0) {
            delete_q <- glue::glue_sql("
              DELETE FROM selected_rating_factors
              WHERE coc_version_id = {coc_id} AND rating_factor_id IN ({ids*})
            ", coc_id = user_coc$coc_version_id, ids = to_delete_ids, .con = DB_CON)
            
            dbExecute(DB_CON, delete_q)
          }
          
          # 5. INSERT new records that are now selected
          if (nrow(to_insert) > 0) {
            dbAppendTable(
              DB_CON,
              "selected_rating_factors",
              to_insert %>% fmutate(coc_version_id = user_coc$coc_version_id, is_selected = NULL)
            )
          }
          
          # 6. UPDATE existing records
          if (nrow(to_update) > 0) {
            update_q <- "
              UPDATE selected_rating_factors
              SET goal = $1, max_point_value = $2
              WHERE coc_version_id = $3 AND rating_factor_id = $4
            "
            purrr::pwalk(to_update, function(rating_factor_id, goal, max_point_value, ...) {
              dbExecute(
                DB_CON,
                update_q,
                params = list(goal, max_point_value, user_coc$coc_version_id, rating_factor_id)
              )
            })
          }
        }) # End dbWithTransaction
        
        removeNotification(ns("saving_msg"))
        showNotification("Renewal/Expansion criteria saved successfully!")
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
    }) # End save button
  })
}


#-------------------------------------------------------------------------------
# 4. NEW Project Factors Sub-Module
# - Displays rating factors applicable to New projects.
# - Allows customization of 'Goal' and 'Max Point Value'.
#-------------------------------------------------------------------------------

#' @title mod_new_factors_ui
#' @noRd
mod_new_factors_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "New Project Rating Criteria",
    value = id,
    card(
      project_and_pop_dropdowns(ns, "New"),
      hr(),
      uiOutput(ns("new_factors_ui")),
      actionButton(ns("save_new_factors"), "Save New Project Criteria", icon = icon("save"), class = "btn-primary")
    )
  )
}

#' @title mod_new_factors_server
#' @noRd
mod_new_factors_server <- function(id, user_coc, selected_target_populations) {
  # The server logic here is identical in structure to the renewal/expansion module,
  # differing only by the `funding_action` filter ('New').
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    new_factors_data <- reactive({
      req(user_coc$coc_version_id)
      fetch_and_structure_rating_factors(
        "New", 
        user_coc$coc_version_id, 
        selected_target_populations = input$target_population
      )
    })
    
    output$new_factors_ui <- renderUI({ # Assuming you have a UI output for 'new' factors
      data_groups_nested <- new_factors_data()
      render_nested_factor_accordion_ui(
        ns = ns,
        funding_action = "New",
        data_groups_nested = data_groups_nested,
        placeholder_text = "No rating factors found for New projects."
      )
    })
    
    observeEvent(input$save_new_factors, {
      # The save logic would be implemented here, identical in form to the renewal logic.
      shiny::showNotification("New Project criteria save logic would be implemented here.", type = "message")
    })
  })
}