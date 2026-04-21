#' @title mod_new_factors_ui
#' @noRd
mod_customize_rating_factors_ui <- function(id, funding_action) {
  ns <- NS(id)
  
  display_funding_action = ifelse(funding_action == "Renew", "Renewal/Expansion", "New")
  
  project_and_pop_dropdowns <- function(ns) {
    project_type_dropdown <- selectInput(
      inputId = ns("project_type_filter"),
      label = "Select project type",
      choices = get_labelled_lookups("project_type")[MAIN_PROJECT_TYPES],
      multiple = TRUE,
      selected = MAIN_PROJECT_TYPES # Pre-select all for initial state
    )
    
    target_pop_dropdown <- selectInput(
      inputId = ns("target_population_filter"),
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
      style = "overflow: visible !important;", 
      sidebar = sidebar(
        title = "Filters",
        id = ns("sidebar"),
        width = "10%",
        project_and_pop_dropdowns(ns)
      ),
      card(
        style = "overflow: visible !important;", 
        
        div(
          id = ns("factor_container"),
          mod_user_presence_ui(ns("presence")),
          uiOutput(ns("factors_ui")) |> withSpinner()
        ),
        
        card_footer(
          class = "sticky-footer d-flex justify-content-between align-items-center",
          actionButton(ns("add_custom_factor"), "Add Custom Rating Factor", icon = icon("plus")),
          actionButton(ns("save_factors"), paste0("Save ", display_funding_action, " Criteria"), icon = icon("save"), class = "btn-primary")
        )
      )
    )
  )
}

#' @title mod_new_factors_server
#' @noRd
mod_customize_rating_factors_server <- function(id, user_coc, funding_action, nav_control, active) {
  # The server logic here is identical in structure to the renewal/expansion module,
  # differing only by the `funding_action` filter ('New').
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    funding_action_id <- get_lookup_refid(funding_action, "funding_action")
    other_factor_group_id <- get_other_factor_group_id(funding_action_id) #used to ensure the custom factor goes in the Other group
    goal_char_limit <- get_db_column_limit("rating_factors","goal")
    
    refresh_trigger <- reactiveVal(0)
    subgroup_check_all_values <- reactiveValues()
    current_versions <- reactiveValues()
    
    all_coc_factors <- reactive({
      req(funding_action, user_coc$coc_version_id, refresh_trigger())
      
      # Fetch data from DB
      get_all_coc_factors(funding_action_id, user_coc$coc_version_id)
    })
    
    all_coc_factors_structured <- reactive({
      req(user_coc$coc_version_id)
      
      # Use the unfiltered data here
      f <- all_coc_factors() 
      
      nested_data <- list()
      unique_groups <- unique(f$factor_group)
      
      for (group_name in unique_groups) {
        group_dt <- f[factor_group == group_name]
        nested_data[[group_name]] <- split(group_dt, by = "factor_subgroup")
      }
      
      nested_data
    })

    # Handle filters
    observeEvent(c(input$project_type_filter, input$target_population_filter), {
      # This observer depends on the filters
      pt_filter <- input$project_type_filter
      tp_filter <- input$target_population_filter
      
      # We use JS for speed to avoid 100+ server-to-client messages
      # Construct a CSS selector that hides everything, then shows matches
      # Or simply iterate through the classes
      
      # Logic:
      # 1. Hide all rows with class 'factor-row'
      # 2. Show rows where (class contains a selected PT OR pt-all) 
      #    AND (class contains a selected TP OR tp-all)
      
      pt_selectors <- if(is.null(pt_filter)) "'.pt-all'" else paste0("'.pt-all, ", paste0(".pt-", pt_filter, collapse = ", "), "'")
      tp_selectors <- if(is.null(tp_filter)) "'.tp-all'" else paste0("'.tp-all, ", paste0(".tp-", tp_filter, collapse = ", "), "'")
      
      shinyjs::runjs(sprintf("
        $('.factor-row').hide();
        $('.factor-row').filter(function() {
        debugger;
        
           var hasPT = $(this).is(%s) || %s;
           var hasTP = $(this).is(%s) || %s;
           return hasPT && hasTP;
        }).show();
        
        // Hide the whole accordion if all subfactors are hidden
        $('.bslib-accordion-panel').each(function() {
          var visibleChildren = $(this).find('.factor-row:visible').length;
          $(this).toggle(visibleChildren !== 0);
        });
      ", 
      pt_selectors, tolower(as.character(is.null(pt_filter))), 
      tp_selectors, tolower(as.character(is.null(tp_filter)))))
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    
    # get_col_widths <- function(funding_action, adding_custom_factor = FALSE) {
    #   if(funding_action == "Renew")
    #     breakpoints(
    #       sm = c(1, 1, 1, 5, 2, 2),
    #       md = c(1, 1, 1, 5, 2, 2),
    #       lg = c(1, 1, 1, 5, 2, 2),
    #       xl = c(1, 1, 1, 5, 2, 2),
    #       xxl = c(1, 1, 1, 7, 1, 1)
    #     )
    #   else if(!adding_custom_factor)
    #     breakpoints(
    #       sm = c(1, 1, 6, 2, 2),
    #       md = c(1, 1, 6, 2, 2),
    #       lg = c(1, 1, 6, 2, 2),
    #       xl = c(1, 1, 6, 2, 2),
    #       xxl = c(1, 1, 8, 1, 1)
    #     )
    #   else c(1, 1, 6, 2, 2)
    # }
    # Helper to make a single row - much faster than layout_columns
    make_factor_row <- function(id, project_type, target_population, text, goal, points, selected, group_name) {
      iv$add_rule(paste0("goal_", id), ~if (isTRUE(nchar(.) > goal_char_limit)) glue::glue("Limited to {goal_char_limit} characters"))
      iv$add_rule(paste0("points_", id), sv_between(-999.9, 999.9))


      # Metadata classes for the JS Filtering
      pt_class <- if(is.na(project_type)) "pt-all" else paste0("pt-", project_type)
      tp_class <- if(is.na(target_population)) "tp-all" else paste0("tp-", target_population)

      div(
        class = paste("factor-row", pt_class, tp_class),
        style = "display: flex; gap: 15px; align-items: center; padding: 8px 0; border-bottom: 1px solid #f0f0f0;",
        # Use inline-styles or a CSS file to define these widths
        div(
          # 40px forces it to stay narrow. margin-bottom: 0px removes Shiny's default spacing
          style = "flex: 0 0 100px; margin-bottom: 0px; display: flex; justify-content: center;",
          checkboxInput(ns(paste0("select_", id)), label = NULL, value = selected, width = "100%")
        ),
        if(funding_action == "Renew") div(style = "flex: 1;", get_lookup_label(project_type, "project_type")) else NULL,
        div(style = "flex: 1;", get_lookup_label(target_population, "target_population")),
        div(style = "flex: 3; font-size: 0.9rem;", text),
        div(style = "flex: 1;", textInput(ns(paste0("goal_", id)), NULL, value = goal, width = "100%")),
        div(style = "flex: 0 0 80px;", numericInput(ns(paste0("points_", id)), NULL, value = points, step = 0.1, width = "100%"))
      )
      # 
      # row_items <- list(
      #   checkboxInput(ns(paste0("select_", id)), label = NULL, value = selected),
      #   if(funding_action == "Renew") div(get_lookup_label(project_type, ref_type = "project_type")) else NULL,
      #   div(get_lookup_label(target_population, ref_type = "target_population")),
      #   div(text),
      #   textInput(ns(paste0("goal_", id)), label = NULL, value = goal),
      #   numericInput(ns(paste0("points_", id)), min = -999.9, max=999.9, label = NULL, value = points, step = 0.1)
      # )
      # 
      # layout_columns(
      #   class = paste("factor-row", pt_class, tp_class),
      #   id = ns(paste0("rows_items_", janitor::make_clean_names(group_name))),
      #   col_widths = get_col_widths(funding_action),
      #   !!!purrr::compact(row_items)
      # )
    }
    
    render_nested_factor_accordion_ui <- function(data_groups_nested) {
      if (length(data_groups_nested) == 0) return(p("No rating factors found."))
      
      accordion_items_group <- lapply(names(data_groups_nested), function(group_name) {
        group_data_subgroups <- data_groups_nested[[group_name]]
        
        sub_accordion_items <- lapply(names(group_data_subgroups), function(subgroup_name) {
          subgroup_data <- group_data_subgroups[[subgroup_name]]
          
          ## Build the rows ---------
          factor_rows <- if(allNA(subgroup_data$rating_factor_id)) NULL else {
            purrr::pmap(list(
              subgroup_data$rating_factor_id, 
              subgroup_data$project_type, 
              subgroup_data$target_population, 
              subgroup_data$rating_factor_text, 
              subgroup_data$goal, 
              subgroup_data$max_point_value, 
              subgroup_data$selected,
              group_name
            ), make_factor_row)
          }
          
          all_subgroup_factors_selected <- nrow(subgroup_data) == nrow(subgroup_data[selected == TRUE])
          ## Build the header --------
          # header_items <- list(
          #   div(
          #     tags$b("Use in rating?"),
          #     checkboxInput(
          #       ns(janitor:::make_clean_names(paste0(group_name, "_check_all_", subgroup_name))),
          #       label = NULL,
          #       value = all_subgroup_factors_selected
          #     )
          #   ),
          #   # Use if() normally; compact() will remove the NULLs later
          #   if(funding_action == "Renew") tags$b("Project Type") else NULL,
          #   tags$b("Target Population", style="word-wrap: normal;"),
          #   tags$b("Rating Factor"),
          #   tags$b("Factor/\nGoal", style="word-wrap: normal;"),
          #   tags$b("Max Point Value")
          # )
          
          header_id <- 
          
          bslib::accordion_panel(
            title = ifelse(subgroup_name == "NA", "", subgroup_name),
            # layout_columns(
            #   col_widths = get_col_widths(funding_action),
            #   !!!purrr::compact(header_items)
            # )
            div(style = "display: flex; gap: 15px; font-weight: bold; margin-bottom: 10px; border-bottom: 2px solid #ddd;",
                div(
                  style = "flex: 0 0 100px; margin-bottom: 0px; display: flex; justify-content: center;", 
                  checkboxInput(
                    ns(janitor::make_clean_names(paste0(group_name, "_check_all_", subgroup_name))), 
                    label = "Use in Rating?", 
                    value = all_subgroup_factors_selected, 
                    width = "100%"
                  )
                ),
                if(funding_action == "Renew") div(style = "flex: 1;", "Project Type") else NULL,
                div(style = "flex: 1;", "Target Population"),
                div(style = "flex: 3;", "Rating Factor"),
                div(style = "flex: 1;", "Goal"),
                div(style = "flex: 0 0 80px;", "Max Points")
            ),
            hr(),
            if(allNA(subgroup_data$rating_factor_id)) NULL else factor_rows,
            
            # Add a placeholder for custom factors ---
            # This div will only be added for the specific subgroup.
            if (group_name == "Other and Local Criteria") {
              div(id = ns("custom_factors_placeholder"))
            }
          )
        })
        
        bslib::accordion_panel(
          title = group_name,
          bslib::accordion(
            !!!sub_accordion_items,
            id = ns(paste0("sub_accordion_", janitor:::make_clean_names(group_name))),
            multiple = TRUE,
            open = names(group_data_subgroups)[1]
          )
        )
      })
      
      bslib::accordion(!!!accordion_items_group, id = ns("main_accordion"), multiple = TRUE)
    }
    
    handle_check_all_box_functionality <- function(input) {
      # 1. Fetch ALL possible subgroup names ONCE at the start.
      all_possible_subgroups <- get_subgroups_by_funding_action(funding_action_id)
      
      # PARENT -> CHILDREN
      # observe changes to check-all boxes
      lapply(seq_len(nrow(all_possible_subgroups)), function(i) {
        group <- all_possible_subgroups$factor_group[i]
        
        # Ensure NA subgroups are handled correctly as "NA" strings to match the UI
        subgroup <- all_possible_subgroups$factor_subgroup[i]
        subgroup_str <- if(is.na(subgroup)) "NA" else as.character(subgroup)
        
        subgroup_check_all_input <- janitor::make_clean_names(paste0(group, "_check_all_", subgroup_str))
        
        observeEvent(input[[subgroup_check_all_input]], {
          new_val <- input[[subgroup_check_all_input]]
          if (is.null(new_val)) return()
          
          stored_val <- isolate(subgroup_check_all_values[[subgroup_check_all_input]])
          
          # Only update children if the user clicked (value changed from what we last recorded)
          # We don't need 'is_initialized' because ignoreInit = TRUE prevents initial fire
          if (!identical(new_val, stored_val)) {
            
            # Store in memory to prevent infinite loops
            subgroup_check_all_values[[subgroup_check_all_input]] <- new_val
            
            # Extract child factors safely
            subgroup_factors <- isolate(all_coc_factors_structured())[[group]][[subgroup_str]]
            
            if (!is.null(subgroup_factors) && nrow(subgroup_factors) > 0) {
              for (factor_id in subgroup_factors$rating_factor_id) {
                checkbox_id <- paste0("select_", factor_id)
                # Only update if different
                if (!identical(input[[checkbox_id]], new_val)) {
                  updateCheckboxInput(session, checkbox_id, value = new_val)
                }
              }
            }
          }
          
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
      
      # CHILDREN -> PARENT
      # Update subgroup check-all-that-apply boxes based on underlying factor boxes
      observe({
        data <- all_coc_factors_structured()
        req(data)
        
        # Loop through only the groups and subgroups currently visible on the UI.
        for (i in seq_along(data)) {
          group_data <- data[[i]]
          group_name <- names(data)[i]
          
          for (subgroup_name in names(group_data)) {
            
            subgroup_data <- group_data[[subgroup_name]]
            factor_ids <- subgroup_data$rating_factor_id
            
            # Skip if this subgroup has no real factors
            if (length(factor_ids) == 0 || all(is.na(factor_ids))) next
            
            # Read the current values of all child factor checkboxes for this subgroup.
            factor_selections <- sapply(factor_ids, function(id) input[[paste0("select_", id)]])
            if (any(sapply(factor_selections, is.null))) next
            
            # Determine the new state for the parent "check all" box.
            parent_should_be_checked <- all(unlist(factor_selections))
            
            # Get the ID of the parent checkbox (FIXED: 2 colons instead of 3)
            subgroup_check_all_input <- janitor::make_clean_names(paste0(group_name, "_check_all_", subgroup_name))
            
            current_parent_val <- isolate(input[[subgroup_check_all_input]])
            
            # Update the parent checkbox ONLY if its state needs to change.
            if (!identical(current_parent_val, parent_should_be_checked)) {
              # CRITICAL: Update memory so programmatic change doesn't re-trigger logic
              subgroup_check_all_values[[subgroup_check_all_input]] <- parent_should_be_checked
              
              updateCheckboxInput(session, subgroup_check_all_input, value = parent_should_be_checked)
            }
          }
        }
      })
    }
    # Function to generate the UI for a single custom factor row
    
    iv <- shinyvalidate::InputValidator$new()
    iv$enable()
    
    output$factors_ui <- renderUI({ # Assuming you have a UI output for 'new' factors
      render_nested_factor_accordion_ui(all_coc_factors_structured())
    })
    
    handle_check_all_box_functionality(input)
    
    # Observer for the "Add Custom Rating Factor" button
    # This just adds UI for the user to enter a new factor
    # It doesn't actually save until they click Save
    iv_custom <- shinyvalidate::InputValidator$new()
    # validate that rating_factor_text is not empty
    iv_custom$add_rule("custom_text", sv_required())
    iv_custom$add_rule("custom_text", ~ if(. %in% all_coc_factors()$rating_factor_text) "You already have a rating factor with this text.")
    ## validate that max point value of >= 0
    iv_custom$add_rule("custom_points",  ~ if(is.na(.)) "Please input a numeric value")
    iv_custom$add_rule("custom_points", sv_between(-999.9, 999.9))
    iv_custom$add_rule("custom_goal", ~
                  if (isTRUE(nchar(.) > goal_char_limit)) "Limited to 10 characters"
    )
    observeEvent(input$add_custom_factor, {
      showModal(
        modalDialog(
          title = "Additional Rating Factor",
          # Project Type - dropdown
          if(funding_action == "Renew") 
            selectInput(
              inputId = ns("custom_pt"),
              label = "Project Type",
              choices = c("Select an option below" = "", get_labelled_lookups("project_type")[MAIN_PROJECT_TYPES]),
              multiple = TRUE
            ) else NULL,
          # Target Population - dropdown
          selectInput(
            inputId = ns("custom_tp"),
            label = "Target Population",
            choices = c("Select an option below" = "", get_labelled_lookups("target_population")[c("DV", "General", "NA")]),
            multiple = TRUE
          ),
          # Rating Factor Text
          textInput(ns("custom_text"), label = "Rating Factor*", placeholder = "Enter custom factor text"),
          # Factor/Goal - short text
          textInput(ns("custom_goal"), label = "Factor/Goal", placeholder = "Enter goal"),
          # Max Point Value - numeric
          numericInput(ns("custom_points"), min = -999.9, max = 999.9, label = "Max Point Value*", value = 0, step = 0.1),
          
          footer = tagList(
            actionButton(ns("submit_custom_factor"), "Submit", class = "btn-primary"),
            actionButton(ns("cancel_custom_factor"), "Cancel")
          )
        )
      )
      
    }, ignoreInit = TRUE)
    
    observeEvent(input$cancel_custom_factor, {
      iv_custom$disable()
      removeModal()
    })
    
    observeEvent(input$submit_custom_factor, {
      
      iv_custom$enable()
      req(iv_custom$is_valid())
      iv_custom$disable()
      removeModal()
      
      ## build new selected_rating_factors row
      updated_selected_rating_factors <- data.table(
        coc_version_id = user_coc$coc_version_id,
        selected = TRUE,
        goal = input$custom_goal,
        max_point_value = as.numeric(input$custom_points),
        created_by = user_coc$username
      )
    
      pt_tp_combo <- expand.grid(
        list(
          project_type = if(funding_action == "Renew") as.integer(input$custom_pt) else NA,
          target_population = as.integer(input$custom_tp)
        )
      )
        
      ## build new rating_factors row
      custom_factor_data <- data.table(
        funding_action = funding_action_id,
        coc_version_id = user_coc$coc_version_id,
        rating_factor_text = input$custom_text,
        factor_group = other_factor_group_id,
        factor_subgroup = NA,
        selected = TRUE,
        goal = input$custom_goal,
        max_point_value = input$custom_points,
        created_by = user_coc$username
      ) |> cbind(pt_tp_combo)
      
      inserted_custom_factor_info <- NULL
      
      pool::poolWithTransaction(get_db_pool(), function(p) {
        # insert new factor into rating_Factor table in DB, return rating_factor_id
        inserted_custom_factor_info <- insert_custom_factor_to_db(
          p,
          custom_factor_data |>
            fselect(funding_action, coc_version_id, rating_factor_text, factor_group, goal, max_point_value, created_by, project_type, target_population)
        )
        ## add to selected_rating_factor table only if first attempt succeeded
        if(length(inserted_custom_factor_info) > 0 && isTruthy(inserted_custom_factor_info)){
          # add the newly created rating factor ID to the set of selected factors (it's auto-selected)
          dbAppendTable(
            p, 
            'selected_rating_factors',
            updated_selected_rating_factors |>
              cbind(inserted_custom_factor_info)
          )
        }
        
      })
      showNotification("Custom rating factor added!", type = 'message')
      
      refresh_trigger(refresh_trigger() + 1)
      
      user_coc$customized_rating_factors_updated <- user_coc$customized_rating_factors_updated + 1
    })
    
    ## store user settings for project type and target population
    observeEvent(input$project_type, {
      req(!is.null(user_coc$coc_version_id) & nav_control() == 'rating')
      user_coc$settings[[glue::glue('rating_{id}_project_type')]] <- input$project_type
    }, ignoreInit = TRUE)
    
    observeEvent(input$target_population, {
      req(!is.null(user_coc$coc_version_id) & nav_control() == 'rating')
      user_coc$settings[[glue::glue('rating_{id}_target_population')]] <- input$target_population
    }, ignoreInit = TRUE)
    
    
    # Initialize version_ids from db.
    # These will only be updated after a save
    observeEvent(user_coc$coc_version_id, {
      data <- all_coc_factors() |>
        fsubset(!is.na(rating_factor_id))
      
      req(data)
      for (i in seq_row(data)) {
        rid <- as.character(data$rating_factor_id[[i]])
        vid <- data$version_id[[i]]
        current_versions[[rid]] <- vid
      }
    }, ignoreInit = TRUE)
    
    # Save --------
    observeEvent(input$save_factors, {
      req(iv$is_valid())
      data <- isolate(all_coc_factors())
      if (nrow(data) == 0) return()
      
      updated_selected_rating_factors <- rbindlist(lapply(data$rating_factor_id, function(id) {
        current_v <- current_versions[[as.character(id)]]
        
        data.table(
          rating_factor_id = id,
          coc_version_id = user_coc$coc_version_id,
          selected = isTRUE(input[[paste0("select_", id)]]),
          goal = as.character(input[[paste0("goal_", id)]]),
          max_point_value = as.numeric(input[[paste0("points_", id)]]),
          created_by = user_coc$username,
          version_id = current_v
        )
      }))
      
      needs_refresh <- pool::poolWithTransaction(get_db_pool(), function(p) {
        update_selected_rating_factors_db(p, updated_selected_rating_factors)
      })
      
      if(!needs_refresh) {
        for (id in data$rating_factor_id) {
          rid <- as.character(id)
          current_versions[[rid]] <- current_versions[[rid]] + 1
        }
        
        user_coc$customized_rating_factors_updated <- user_coc$customized_rating_factors_updated + 1
      } else {
        refresh_trigger(refresh_trigger() + 1)
      }
    }, ignoreInit = TRUE)
    
    # --- User presence ---------
    record_being_edited <- reactiveVal(NULL)
    observeEvent(input$projects_table_cell_being_edited, {
      record_being_edited(
        list(
          record_id = projects_data()[input$projects_table_cell_clicked$row]$project_id,
          field = names(projects_data())[[input$projects_table_cell_clicked$col + 1]]
        )
      )
    })
    
    mod_user_presence_server(
      id = ns("presence"),
      user_coc = user_coc,
      # Record is the CoC Version
      record_id = reactive({ user_coc$coc_version_id }),
      active = active
    )
  })
}
