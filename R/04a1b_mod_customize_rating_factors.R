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
        title = "Filters",
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
    
    funding_action_id <- get_lookup_refid(funding_action, "funding_action")
    other_factor_group_id <- get_other_factor_group_id(funding_action_id)
    
    refresh_trigger <- reactiveVal(0)
    # Counter for unique IDs for custom factor rows
    custom_factor_counter <- reactiveVal(0)
    # Store observers for remove buttons to manage them
    custom_factor_observers <- reactiveValues()
    subgroup_check_all_values <- reactiveValues()

    all_coc_factors <- reactive({
      req(funding_action, user_coc$coc_version_id)
      
      all_factors <- get_all_coc_factors(funding_action_id, user_coc$coc_version_id)
      
      if(!is.null(input$project_type)) all_factors <- all_factors[project_type %in% input$project_type]
      if(!is.null(input$target_population)) all_factors <- all_factors[target_population %in% input$target_population]
      
      all_factors
    })
    
    all_coc_factors_structured <- reactive({
      req(user_coc$coc_version_id)
      
      all_factors <- all_coc_factors()
      nested_data <- list()
      unique_groups <- unique(all_factors$factor_group)
      
      for (group_name in unique_groups) {
        group_dt <- all_factors[factor_group == group_name]
        nested_data[[group_name]] <- split(group_dt, by = "factor_subgroup")
      }
      
      nested_data
    })
    
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
    
    handle_check_all_box_functionality <- function(input) {
      # 1. Fetch ALL possible subgroup names ONCE at the start.
      #    This decouples observer creation from the reactive data flow.
      #    We query the source table directly for this static list.
      all_possible_subgroups <- get_subgroups_by_funding_action(funding_action_id)

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
            subgroup_factors <- isolate(all_coc_factors())[[group]][[toString(subgroup)]]
            
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
        data <- all_coc_factors_structured()
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
              multiple = FALSE
            ) else NULL
          ),
          column(1, selectInput(
            inputId = tp_input_id,
            label = NULL,
            choices = get_labelled_lookups("target_population")[c("DV", "General", "NA")],
            multiple = FALSE
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
    
    add_custom_factor_ui <- function(ns, input) {
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
    
    output$factors_ui <- renderUI({ # Assuming you have a UI output for 'new' factors
      data_groups_nested <- all_coc_factors_structured()
      
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
      add_custom_factor_ui(ns, input)
    }, ignoreInit = TRUE)
    
    observeEvent(input$save_factors, {
      updated_selected_rating_factors <- rbindlist(lapply(all_coc_factors()$rating_factor_id, function(id) {
        data.table(
          rating_factor_id = id,
          coc_version_id = user_coc$coc_version_id,
          selected = isTRUE(input[[paste0("select_", id)]]),
          goal = as.character(input[[paste0("goal_", id)]]),
          max_point_value = as.numeric(input[[paste0("points_", id)]]),
          created_by = user_coc$username,
          date_updated = all_coc_factors()[rating_factor_id == id]$date_updated
        )
      }))
      
      if(custom_factor_counter() > 0) {
        custom_factor_data <- rbindlist(lapply(seq(custom_factor_counter()), function(i) {
          data.table(
            funding_action = funding_action_id,
            coc_version_id = user_coc$coc_version_id,
            project_type = input[[paste0("custom_pt_", i)]],
            target_population = input[[paste0("custom_tp_", i)]],
            rating_factor_text = input[[paste0("custom_text_", i)]],
            factor_group_id = other_factor_group_id,
            selected = isTRUE(input[[paste0("custom_select_", i)]]),
            goal = input[[paste0("custom_goal_", i)]],
            max_point_value = input[[paste0("custom_points_", i)]],
            username = user_coc$username
          )
        }))
      }
      
      inserted_custom_factor_info <- NULL
      needs_refresh2 <- FALSE
      pool::poolWithTransaction(DB_POOL, function(p) {
        if(custom_factor_counter() > 0) {
          # insert new factor into DB, return rating_factor_id
          inserted_custom_factor_info <- insert_custom_factor_to_db(
            p, 
            custom_factor_data |> fselect(-selected)
          )
         
          # add the newly created rating factor ID to the set of selected factors (it's auto-selected)
         updated_selected_rating_factors <- updated_selected_rating_factors |>
            rbind(
              custom_factor_data |> 
                cbind(inserted_custom_factor_info),
              fill = TRUE
            ) |>
           fselect(rating_factor_id, coc_version_id, selected, goal, max_point_value, created_by, date_updated)
        }
        
        needs_refresh2 <- update_selected_rating_factors_db(p, updated_selected_rating_factors)
      })
      
      # if(is.null(inserted_custom_factor_info) || needs_refresh2)
        refresh_trigger(\(x) x + 1)
      
      module_returns$customize_rating_criteria <- TRUE
    }, ignoreInit = TRUE)
  })
}
