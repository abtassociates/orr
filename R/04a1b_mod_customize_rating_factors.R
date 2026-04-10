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
      sidebar = sidebar(
        title = "Filters",
        id = ns("sidebar"),
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
mod_customize_rating_factors_server <- function(id, user_coc, funding_action, nav_control) {
  # The server logic here is identical in structure to the renewal/expansion module,
  # differing only by the `funding_action` filter ('New').
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    funding_action_id <- get_lookup_refid(funding_action, "funding_action")
    other_factor_group_id <- get_other_factor_group_id(funding_action_id) #used to ensure the custom factor goes in the Other group
    
    refresh_trigger <- reactiveVal(0)
    # Counter for unique IDs for custom factor rows
    custom_factor_counter <- reactiveVal(0)
    # Store observers for remove buttons to manage them
    custom_factor_observers <- reactiveValues()
    subgroup_check_all_values <- reactiveValues()

    all_coc_factors <- reactive({
      req(funding_action, user_coc$coc_version_id, refresh_trigger())
      
      get_all_coc_factors(funding_action_id, user_coc$coc_version_id)
    })
    
    all_coc_factors_filtered <- reactive({
      f <- all_coc_factors()
      if(!is.null(input$project_type_filter)) f <- f[project_type %in% c(input$project_type_filter, NA)]
      if(!is.null(input$target_population_filter)) f <- f[target_population %in% c(input$target_population_filter, NA)]
      
      f
    })
    
    all_coc_factors_structured <- reactive({
      req(user_coc$coc_version_id)
      
      f <- all_coc_factors_filtered()
      nested_data <- list()
      unique_groups <- unique(f$factor_group)
      
      for (group_name in unique_groups) {
        group_dt <- f[factor_group == group_name]
        nested_data[[group_name]] <- split(group_dt, by = "factor_subgroup")
      }
      
      nested_data
    })
    
    get_col_widths <- function(funding_action, adding_custom_factor = FALSE) {
      if(funding_action == "Renew")
        breakpoints(
          sm = c(1, 1, 1, 5, 2, 2),
          md = c(1, 1, 1, 5, 2, 2),
          lg = c(1, 1, 1, 5, 2, 2),
          xl = c(1, 1, 1, 5, 2, 2),
          xxl = c(1, 1, 1, 7, 1, 1)
        )
      else if(!adding_custom_factor)
        breakpoints(
          sm = c(1, 1, 6, 2, 2),
          md = c(1, 1, 6, 2, 2),
          lg = c(1, 1, 6, 2, 2),
          xl = c(1, 1, 6, 2, 2),
          xxl = c(1, 1, 8, 1, 1)
        )
      else c(1, 1, 6, 2, 2)
    }
    
    render_nested_factor_accordion_ui <- function(ns, funding_action = "Renew", data_groups_nested, placeholder_text = "No rating factors found.") {
      if (length(data_groups_nested) == 0) {
        return(p(placeholder_text))
      }
      
      accordion_items_group <- lapply(names(data_groups_nested), function(group_name) {
        group_data_subgroups <- data_groups_nested[[group_name]]
        
        sub_accordion_items <- lapply(names(group_data_subgroups), function(subgroup_name) {
          subgroup_data <- group_data_subgroups[[subgroup_name]]
          
          factor_rows <- if(allNA(subgroup_data$rating_factor_id))
            NULL
          else
            purrr::pmap(
              list(
                subgroup_data$rating_factor_id, 
                subgroup_data$project_type, 
                subgroup_data$target_population, 
                subgroup_data$rating_factor_text, 
                subgroup_data$goal, 
                subgroup_data$max_point_value, 
                subgroup_data$selected
              ), function(id, project_type, target_population, text, goal, points, selected) {
                row_items <- list(
                  checkboxInput(ns(paste0("select_", id)), label = NULL, value = selected),
                  if(funding_action == "Renew") div(get_lookup_label(project_type, ref_type = "project_type")) else NULL,
                  div(get_lookup_label(target_population, ref_type = "target_population")),
                  div(text),
                  textInput(ns(paste0("goal_", id)), label = NULL, value = goal),
                  numericInput(ns(paste0("points_", id)), min = 1, label = NULL, value = points, step = 0.1)
                )
                
                iv$add_rule(paste0("goal_", id), ~
                  if (isTRUE(nchar(.) > 10)) "Limited to 10 characters"
                )
                iv$add_rule(paste0("points_", id), sv_between(0, 999.9))
                
                layout_columns(
                  id = ns(paste0("rows_items_", gsub(" ", "-", group_name))),
                  col_widths = get_col_widths(funding_action),
                  !!!purrr::compact(row_items)
                )
              }
            ) # end purrr::pmap
          
          all_subgroup_factors_selected <- nrow(subgroup_data) == nrow(subgroup_data[selected == TRUE])
          
          header_items <- list(
            div(
              tags$b("Use in rating?"),
              checkboxInput(
                ns(janitor:::make_clean_names(paste0(group_name, "_check_all_", subgroup_name))),
                label = NULL,
                value = all_subgroup_factors_selected
              )
            ),
            # Use if() normally; compact() will remove the NULLs later
            if(funding_action == "Renew") tags$b("Project Type") else NULL,
            tags$b("Target Population", style="word-wrap: normal;"),
            tags$b("Rating Factor"),
            tags$b("Factor/\nGoal", style="word-wrap: normal;"),
            tags$b("Max Point Value")
          )
          
          bslib::accordion_panel(
            title = ifelse(subgroup_name == "NA", "", subgroup_name),
            layout_columns(
              col_widths = get_col_widths(funding_action),
              !!!purrr::compact(header_items)
            ),
            hr(),
            if(allNA(subgroup_data$rating_factor_id)) NULL else factor_rows,
            
            # Add a placeholder for custom factors ---
            # This div will only be added for the specific subgroup.
            if (group_name == "Other and Local Criteria") {
              div(id = ns("custom_factors_placeholder"))
            }
          )
        }) # end subaccordion items
        
        bslib::accordion_panel(
          title = group_name,
          bslib::accordion(
            !!!sub_accordion_items,
            id = ns(paste0("sub_accordion_", janitor:::make_clean_names(group_name))),
            multiple = TRUE,
            open = names(group_data_subgroups)[1]
          )
        )
      }) #end group accordion items
      
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
        subgroup_check_all_input <- janitor:::make_clean_names(paste0(group, "_check_all_", subgroup))
        
        observeEvent(input[[subgroup_check_all_input]], {
          new_val <- input[[subgroup_check_all_input]]

          if (is.null(new_val)) return()
          
          stored_val <- isolate(subgroup_check_all_values[[subgroup_check_all_input]])
          is_initialized <- !is.null(stored_val)
          
          # Only update children if the user clicked (value changed from what we last recorded)
          if(!identical(new_val, stored_val) && is_initialized) {
            subgroup_check_all_values[[subgroup_check_all_input]] <- new_val
            
            subgroup_factors <- isolate(all_coc_factors_structured())[[group]][[toString(subgroup)]]
            
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
            subgroup_check_all_input <- janitor:::make_clean_names(paste0(group_name, "_check_all_", subgroup_name))

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
    iv <- shinyvalidate::InputValidator$new()
    iv$enable()
    
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
    iv_custom <- shinyvalidate::InputValidator$new()
    # validate that rating_factor_text is not empty
    iv_custom$add_rule("custom_text", sv_required())
    iv_custom$add_rule("custom_text", ~ if(. %in% all_coc_factors()$rating_factor_text) "You already have a rating factor with this text.")
    ## validate that max point value of >= 0
    iv_custom$add_rule("custom_points", sv_gte(0))
    iv_custom$add_rule("custom_goal", ~
                  if (isTRUE(nchar(.) > 10)) "Limited to 10 characters"
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
          textInput(ns("custom_text"), label = "Rating Factor", placeholder = "Enter custom factor text"),
          # Factor/Goal - short text
          textInput(ns("custom_goal"), label = "Factor/Goal", placeholder = "Enter goal"),
          # Max Point Value - numeric
          numericInput(ns("custom_points"), min = 1, label = "Max Point Value", value = 0, step = 0.1),
          
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
      
      refresh_trigger(\(x) x + 1)
      
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
    
    
    fts <- reactiveVal(0)
    factors_to_save <- fts %>% debounce(1000)
    col_names_to_track <- c("selected", "goal","max_point_value")
    observeEvent(user_coc$coc_version_id, {
      req(all_coc_factors())
      for(f in all_coc_factors()$rating_factor_id) {
        # browser()
        # factors_to_save[[f$rating_factor_id]] <- sapply(
        #   col_names_to_track, 
        #   function(c) f[[c]]
        # )
        # 

        inputs <- paste0(c("select_", "goal_", "points_"), f)
        inputs <- setNames(inputs, col_names_to_track)
        
        lapply(col_names_to_track, function(c) {
          col_name <- inputs[[c]]
          observeEvent(input[[col_name]], {
            req(iv$is_valid())
            browser()
            fts(\(x) x + 1)
          })
        })
      }
    })
    
    observeEvent(factors_to_save(), {
      save_data()
    })
    save_data <- function() {
      
      updated_selected_rating_factors <- rbindlist(lapply(all_coc_factors()$rating_factor_id, function(id) {
        data.table(
          rating_factor_id = id,
          coc_version_id = user_coc$coc_version_id,
          selected = isTRUE(input[[paste0("select_", id)]]),
          goal = as.character(input[[paste0("goal_", id)]]),
          max_point_value = as.numeric(input[[paste0("points_", id)]]),
          created_by = user_coc$username,
          version_id = all_coc_factors()[rating_factor_id == id]$version_id
        )
      }))
      
      needs_refresh2 <- pool::poolWithTransaction(get_db_pool(), function(p) {
        update_selected_rating_factors_db(p, updated_selected_rating_factors)
      })
      
      # if(is.null(inserted_custom_factor_info) || needs_refresh2)
      refresh_trigger(\(x) x + 1)
      if(!needs_refresh2)
        user_coc$customized_rating_factors_updated <- user_coc$customized_rating_factors_updated + 1
    }
    # observeEvent(input$save_factors, {
    #   save_data()
    # }, ignoreInit = TRUE)
  }) # end
}
