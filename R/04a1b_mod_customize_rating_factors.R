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
    
    special_pop_dropdown <- selectInput(
      inputId = ns("special_population_filter"),
      label = "Select special populations",
      choices = get_labelled_lookups("target_population")[c("DV", "General")],
      multiple = TRUE,
      selected = c("General") # Pre-select General for initial state
    )
    
    dropdowns_to_include <- special_pop_dropdown
    if(funding_action == "Renew") dropdowns_to_include <- list(project_type_dropdown, dropdowns_to_include)
    
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
          actionButton(ns("add_custom_factor"), "Add Custom Rating Factor", icon = icon("plus"))
        )
      )
    )
  )
}

#' @title mod_new_factors_server
#' @noRd
mod_customize_rating_factors_server <- function(id, user_coc, funding_action, nav_control, active) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    funding_action_id <- get_lookup_refid(funding_action, "funding_action")
    other_factor_group_id <- get_other_factor_group_id(funding_action_id) 
    goal_char_limit <- get_db_column_limit("rating_factors","goal")
    
    refresh_trigger <- reactiveVal(0)
    subgroup_check_all_values <- reactiveValues()
    
    input_prefixes <- c("selected", "goal", "max_point_value")
    
    
    all_coc_factors <- reactive({
      req(funding_action, user_coc$coc_version_id, refresh_trigger())
      
      # Fetch data from DB
      get_all_coc_factors(funding_action_id, user_coc$coc_version_id)
    })
    
    # ------- Project Type and Target Pop filters -------------
    observeEvent(c(input$project_type_filter, input$target_population_filter), {
      pt_filter <- input$project_type_filter
      tp_filter <- input$target_population_filter
      
      pt_selectors <- if(is.null(pt_filter)) "'.pt-all'" else paste0("'.pt-all, ", paste0(".pt-", pt_filter, collapse = ", "), "'")
      tp_selectors <- if(is.null(tp_filter)) "'.tp-all'" else paste0("'.tp-all, ", paste0(".tp-", tp_filter, collapse = ", "), "'")
      
      shinyjs::runjs(sprintf("
        $('.factor-row').hide();
        $('.factor-row').filter(function() {
           var hasPT = $(this).is(%s) || %s;
           var hasTP = $(this).is(%s) || %s;
           return hasPT && hasTP;
        }).show();
        
        $('.bslib-accordion-panel').each(function() {
          var visibleChildren = $(this).find('.factor-row:visible').length;
          $(this).toggle(visibleChildren !== 0);
        });
      ", 
                             pt_selectors, tolower(as.character(is.null(pt_filter))), 
                             tp_selectors, tolower(as.character(is.null(tp_filter)))))
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    
    # ------- Render the Factors -------------
    ## Validation rules
    
    all_coc_factors_structured <- reactive({
      req(user_coc$coc_version_id)
      
      f <- all_coc_factors() 
      
      nested_data <- list()
      unique_groups <- unique(f$factor_group)
      
      for (group_name in unique_groups) {
        group_dt <- f[factor_group == group_name]
        nested_data[[group_name]] <- split(group_dt, by = "factor_subgroup")
      }
      
      nested_data
    })
    
    make_factor_row <- function(id, project_type, target_population, text, goal, points, selected, group_name) {
      iv$add_rule(paste0("goal_", id), ~if (isTRUE(nchar(.) > goal_char_limit)) glue::glue("Limited to {goal_char_limit} characters"))
      
      pt_class <- if(is.na(project_type)) "pt-all" else paste0("pt-", project_type)
      tp_class <- if(is.na(target_population)) "tp-all" else paste0("tp-", target_population)
      
      div(
        class = paste("factor-row", pt_class, tp_class),
        style = "display: flex; gap: 15px; align-items: center; padding: 8px 0; border-bottom: 1px solid #f0f0f0;",
        div(
          style = "flex: 0 0 100px; margin-bottom: 0px; display: flex; justify-content: center;",
          checkboxInput(ns(paste0("selected_", id)), label = NULL, value = selected, width = "100%")
        ),
        if(funding_action == "Renew") div(style = "flex: 1;", get_lookup_label(project_type, "project_type")) else NULL,
        div(style = "flex: 1;", get_lookup_label(target_population, "target_population")),
        div(style = "flex: 3; font-size: 0.9rem;", HTML(text)),
        div(style = "flex: 1;", textInput(ns(paste0("goal_", id)), NULL, value = goal, width = "100%", updateOn = "blur")),
        div(style = "flex: 0 0 80px;", 
            shinyWidgets::autonumericInput(
              inputId = ns(paste0("max_point_value_", id)), 
              label = NULL, 
              value = points,
              align = "center",
              width = "100%",
              decimalPlaces = 1,
              minimumValue = 0,
              maximumValue = 999.9
            ))
      )
    }
    
    subgroup_panel <- function(factor_rows, group_name, subgroup_name, all_selected) {
      contents <- list(
        div(
          style = "display: flex; gap: 15px; font-weight: bold; margin-bottom: 10px;",
          div(
            style = "flex: 0 0 100px; margin-bottom: 0px; display: flex; justify-content: center;", 
            checkboxInput(
              ns(janitor::make_clean_names(paste0(group_name, "_check_all_", subgroup_name))), 
              label = "Use in Rating?", 
              value = all_selected, 
              width = "100%"
            )
          ),
          if(funding_action == "Renew") div(style = "flex: 1;", "Project Type") else NULL,
          div(style = "flex: 1;", "Target Population"),
          div(style = "flex: 3;", "Rating Factor"),
          div(style = "flex: 1;", "Goal"),
          div(style = "flex: 0 0 80px;", "Total Points")
        ),
        hr(),
        factor_rows
      )
      
      if(subgroup_name == "NA") return(contents)
      
      bslib::accordion_panel(
        title = ifelse(subgroup_name == "NA", "", subgroup_name),
        contents
      )
    }
    
    group_panel <- function(group_name, subgroup_panels) {
      bslib::accordion_panel(
        title = group_name,
        bslib::accordion(
          !!!subgroup_panels,
          id = ns(paste0("sub_accordion_", janitor:::make_clean_names(group_name))),
          multiple = TRUE,
          open = FALSE
        )
      )
    }
    
    render_nested_factor_accordion_ui <- function(data_groups_nested) {
      if (length(data_groups_nested) == 0) return(p("No rating factors found."))
      
      accordion_items_group <- lapply(names(data_groups_nested), function(group_name) {
        group_data_subgroups <- data_groups_nested[[group_name]]
        
        subgroup_panels <- lapply(names(group_data_subgroups), function(subgroup_name) {
          subgroup_data <- group_data_subgroups[[subgroup_name]]
          
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
          
          all_subgroup_factors_selected <- allv(subgroup_data$selected, TRUE)
          subgroup_panel(factor_rows, group_name, subgroup_name, all_subgroup_factors_selected)
        })
        
        group_panel(group_name, subgroup_panels)
      })
      
      bslib::accordion(
        !!!accordion_items_group, 
        id = ns("main_accordion"), 
        multiple = TRUE,
        open = FALSE
      )
    }
    
    # -------- Check All by Group ------------------
    handle_check_all_box_functionality <- function(input) {
      all_possible_subgroups <- get_subgroups_by_funding_action(funding_action_id)
      
      lapply(seq_len(nrow(all_possible_subgroups)), function(i) {
        group <- all_possible_subgroups$factor_group[i]
        subgroup <- all_possible_subgroups$factor_subgroup[i]
        subgroup_str <- if(is.na(subgroup)) "NA" else as.character(subgroup)
        subgroup_check_all_input <- janitor::make_clean_names(paste0(group, "_check_all_", subgroup_str))
        
        observeEvent(input[[subgroup_check_all_input]], {
          new_val <- input[[subgroup_check_all_input]]
          if (is.null(new_val)) return()
          
          stored_val <- isolate(subgroup_check_all_values[[subgroup_check_all_input]])
          if (!identical(new_val, stored_val)) {
            subgroup_check_all_values[[subgroup_check_all_input]] <- new_val
            subgroup_factors <- isolate(all_coc_factors_structured())[[group]][[subgroup_str]]
            
            if (!is.null(subgroup_factors) && nrow(subgroup_factors) > 0) {
              for (factor_id in subgroup_factors$rating_factor_id) {
                checkbox_id <- paste0("selected_", factor_id)
                if (!identical(input[[checkbox_id]], new_val)) {
                  updateCheckboxInput(session, checkbox_id, value = new_val)
                }
              }
            }
          }
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
      
      observe({
        data <- all_coc_factors_structured()
        req(data)
        
        for (i in seq_along(data)) {
          group_data <- data[[i]]
          group_name <- names(data)[i]
          
          for (subgroup_name in names(group_data)) {
            subgroup_data <- group_data[[subgroup_name]]
            factor_ids <- subgroup_data$rating_factor_id
            
            if (length(factor_ids) == 0 || all(is.na(factor_ids))) next
            
            factor_selections <- sapply(factor_ids, function(id) input[[paste0("selected_", id)]])
            if (any(sapply(factor_selections, is.null))) next
            
            parent_should_be_checked <- all(unlist(factor_selections))
            subgroup_check_all_input <- janitor::make_clean_names(paste0(group_name, "_check_all_", subgroup_name))
            current_parent_val <- isolate(input[[subgroup_check_all_input]])
            
            if (!identical(current_parent_val, parent_should_be_checked)) {
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
    
    
    # -------- Add Custom Factor ------------------
    iv_custom <- shinyvalidate::InputValidator$new()
    iv_custom$add_rule("custom_text", sv_required())
    iv_custom$add_rule("custom_text", ~ if(. %in% all_coc_factors()$rating_factor_text) "You already have a rating factor with this text.")
    iv_custom$add_rule("custom_goal", ~ if (isTRUE(nchar(.) > goal_char_limit)) "Limited to 10 characters")
    
    observeEvent(input$add_custom_factor, {
      showModal(
        modalDialog(
          title = "Additional Rating Factor",
          if(funding_action == "Renew") 
            selectInput(
              inputId = ns("custom_pt"), label = "Project Type",
              choices = c("Select an option below" = "", get_labelled_lookups("project_type")[MAIN_PROJECT_TYPES]), multiple = TRUE
            ) else NULL,
          selectInput(
            inputId = ns("custom_tp"), label = "Target Population",
            choices = c("Select an option below" = "", get_labelled_lookups("target_population")[c("DV", "General", "NA")]), multiple = TRUE
          ),

          textInput(ns("custom_text"), label = "Rating Factor*", placeholder = "Enter custom factor text"),
          textInput(ns("custom_goal"), label = "Factor/Goal", placeholder = "Enter goal"),
          
          shinyWidgets::autonumericInput(
            inputId = ns("custom_points"),
            label = HTML("Total Point Value*<br><p style='font-size: 0.8em'; margin-bottom: 0px;>(can be negative)</span>"), 
            value = NA,
            align = "center",
            decimalPlaces = 1,
            minimumValue = -999.9,
            maximumValue = 999.9
          ),
          
          hidden(
            p(id = ns("custom_factor_helper"), "A negative value represents the maximum number of points you can deduct from a project for this factor")
          ),
          
          footer = tagList(
            actionButton(ns("submit_custom_factor"), "Submit", class = "btn-primary"),
            actionButton(ns("cancel_custom_factor"), "Cancel")
          )
        )
      )
    }, ignoreInit = TRUE)
    
    observeEvent(input$custom_points, {
      shinyjs::toggle(id = "custom_factor_helper", condition = input$custom_points < 0)
    })
    observeEvent(input$cancel_custom_factor, {
      iv_custom$disable()
      removeModal()
    })
    
    observeEvent(input$submit_custom_factor, {
      iv_custom$enable()
      req(iv_custom$is_valid())
      iv_custom$disable()
      removeModal()
      
      updated_selected_rating_factors <- data.table(
        coc_version_id = user_coc$coc_version_id,
        selected = TRUE,
        goal = input$custom_goal,
        max_point_value = as.numeric(ifelse(is.null(input$custom_points), NA, input$custom_points)),
        created_by = user_coc$username
      )
      
      pt_tp_combo <- expand.grid(
        list(
          project_type = if(funding_action == "Renew") as.integer(input$custom_pt) else NA,
          target_population = as.integer(input$custom_tp)
        )
      )
      
      custom_factor_data <- data.table(
        funding_action = funding_action_id,
        coc_version_id = user_coc$coc_version_id,
        rating_factor_text = input$custom_text,
        factor_group = other_factor_group_id,
        factor_subgroup = NA,
        selected = TRUE,
        goal = input$custom_goal,
        max_point_value = ifelse(is.null(input$custom_points), NA, input$custom_points),
        created_by = user_coc$username
      ) |> cbind(pt_tp_combo)
      
      inserted_custom_factor_info <- NULL
      
      pool::poolWithTransaction(get_db_pool(), function(p) {
        inserted_custom_factor_info <- insert_custom_factor_to_db(
          p,
          custom_factor_data |>
            fselect(funding_action, coc_version_id, rating_factor_text, factor_group, goal, max_point_value, created_by, project_type, target_population)
        )
        if(length(inserted_custom_factor_info) > 0 && isTruthy(inserted_custom_factor_info)){
          dbAppendTable(
            p, 
            'selected_rating_factors',
            updated_selected_rating_factors |> cbind(inserted_custom_factor_info)
          )
        }
      })
      
      showNotification("Custom rating factor added!", type = 'message')
      
      refresh_trigger(refresh_trigger() + 1)
      
      user_coc[[paste0("customized_rating_factors_updated_", funding_action)]] <- user_coc[[paste0("customized_rating_factors_updated_", funding_action)]] + 1
    })
    
    # ------- Save filters to User settings --------------
    observeEvent(input$project_type, {
      req(!is.null(user_coc$coc_version_id) & nav_control() == 'rating')
      update_user_coc_setting(user_coc, "project_type", input$project_type)
    }, ignoreInit = TRUE)
    
    observeEvent(input$target_population, {
      req(!is.null(user_coc$coc_version_id) & nav_control() == 'rating')
      update_user_coc_setting(user_coc, "target_population", input$target_population)
    }, ignoreInit = TRUE)
    
    
    # ------- Auto-Save Engine ---------------
    # all_coc_factors_rv Stores the 'last known good' state from DB
    # we are essentially maintaining three versions of the truth:
    # The Database: The ultimate source of truth.
    # The UI State: What the user has typed (but maybe not saved yet).
    # The Baseline: What we *think* is currently in the database.
    # After a save to the db, we can update all_coc_factors_rv, rather than re-pulling from the db
    all_coc_factors_rv <- reactiveVal(NULL) 
    
    # 1. Update baseline whenever data is fetched from the DB
    observeEvent(all_coc_factors(), {
      data <- all_coc_factors()
      req(fnrow(data))
      
      all_coc_factors_rv(
        data |>
          fsubset(!is.na(rating_factor_id), rating_factor_id, selected, goal, max_point_value, version_id)
      )
    }, priority = 10)
    
    inputs_to_track <- reactive({
      factors <- all_coc_factors_rv()
      req(nrow(factors) > 0)
      
      input_names <- lapply(input_prefixes, paste0, "_", factors$rating_factor_id) |> unlist()
      req(all(input_names %in% names(input)))
      
      s <- lapply(input_names, function(i) {
        val <- input[[i]]
        if(is.null(val)) NA else val
      })
      names(s) <- input_names
      s
    })
    
    # 3. Difference Engine: Find only what changed
    rating_factors_to_save <- reactive({
      raw_inputs <- inputs_to_track()
      req(raw_inputs)
      
      base <- all_coc_factors_rv()
      req(fnrow(base) > 0)
      
      updated_rating_factors <- get_rating_data_to_save(raw_inputs, base, "rating_factor_id", input_prefixes)
      if(is.null(updated_rating_factors)) return(NULL)
      
      updated_rating_factors |>
        fmutate(
          created_by = user_coc$username,
          coc_version_id = user_coc$coc_version_id
        ) |>
        fselect(
          rating_factor_id, 
          coc_version_id,
          selected, 
          goal, 
          max_point_value, 
          created_by,
          version_id
        )
    }) |> debounce(2000) # wait 2 seconds for additional changes
    
    # 5. Auto-Save Observer
    observeEvent(rating_factors_to_save(), {
      to_save <- rating_factors_to_save()
      req(to_save)
      req(fnrow(to_save) > 0, iv$is_valid())
      
      # Update the db
      needs_refresh <- update_selected_rating_factors_db(get_db_pool(), to_save)
      
      if (!needs_refresh) {
        # SUCCESS
        # Update all_coc_factors_rv in memory to match what we just saved 
        #    and increment version_ids so next save works.
        all_coc_factors_rv()[
          to_save, 
          on = "rating_factor_id", 
          `:=`(
            selected = as.integer(i.selected),
            goal = i.goal,
            max_point_value = i.max_point_value,
            version_id = version_id + 1
          )
        ]
        
        user_coc[[paste0("customized_rating_factors_updated_", funding_action)]] <- user_coc[[paste0("customized_rating_factors_updated_", funding_action)]] + 1
      } else {
        # COLLISION OR ERROR (save_to_db already showed notification)
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
      id = "presence",
      user_coc = user_coc,
      # Record is the CoC Version
      record_id = reactive({ user_coc$coc_version_id }),
      active = active
    )
  })
}