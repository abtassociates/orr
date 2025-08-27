# /modules/mod_rating_criteria.R

#-------------------------------------------------------------------------------
# 1. Main Rating Criteria Module
# - This module acts as the container for the entire "Customize Rating Criteria" page.
# - It uses bslib::navset_card_tab to create the three main sections.
#-------------------------------------------------------------------------------

#' @title mod_rating_criteria_ui
#'
#' @description UI for the main rating criteria page.
#' @param id The module's unique ID.
#' @noRd
mod_rating_criteria_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Customize Rating Criteria",
    value = id,
    em("On this page, you can select and customize the criteria used to evaluate your CoC's projects. First, select the CoC-specific threshold requirements that all projects must meet. Then, for both Renewal/Expansion and New projects, select the rating factors you wish to apply and customize their point values."),
    navset_tab(
      id = "rating_criteria_subtabs",
      header = list(
        br(),
        br(),
        conditionalPanel(
          condition = glue::glue("input.rating_criteria_subtabs == '{ns('renewal_factors')}' || input.rating_criteria_subtabs == '{ns('new_factors')}'"),
          # Use bslib::layout_column_wrap for responsive spacing
          fluidRow(
            # Left spacer column (adjust width as needed, e.g., 2, 3, or 4 for smaller/larger margins)
            column(4),
            # Main content column (adjust width to be 12 - (left_spacer + right_spacer))
            column(4,
                   bslib::layout_column_wrap(
                     width = 1/2, # Each dropdown takes half of this 8-column space
                     selectInput(
                       inputId = ns("project_type"),
                       label = "Select project type",
                       choices = get_labelled_lookups("project_type")[main_project_types],
                       multiple = TRUE,
                       selected = main_project_types # Pre-select all for initial state
                     ),
                     selectInput(
                       inputId = ns("target_population"),
                       label = "Select special populations",
                       choices = get_labelled_lookups("target_population")[c("DV", "General")],
                       multiple = TRUE,
                       selected = c("DV", "General") # Pre-select all for initial state
                     )
                   )
            ),
            # Right spacer column (matches left for centering)
            column(4)
          ),
          hr() # Add a horizontal rule below the filters for visual separation
        )
      ),
      mod_coc_thresholds_ui(ns("coc_thresholds")),
      mod_renewal_factors_ui(ns("renewal_factors")),
      mod_new_factors_ui(ns("new_factors")) 
    )
  )
}

#' @title mod_rating_criteria_server
#'
#' @description Server logic for the main rating criteria page.
#' @param id The module's unique ID.
#' @param user_coc contains coc_instance_id to capture user-selected version of the ORR
#' @noRd
mod_rating_criteria_server <- function(id, user_coc) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Call sub-modules for each tab
    mod_coc_thresholds_server("coc_thresholds", user_coc$coc_instance_id)
    
    mod_renewal_factors_server(
      "renewal_factors", 
      user_coc$coc_instance_id, 
      reactive(input$project_type), 
      reactive(input$target_population)
    )
    
    mod_new_factors_server(
      "new_factors", 
      user_coc$coc_instance_id, 
      reactive(input$target_population)
    )
  })
}


#-------------------------------------------------------------------------------
# 2. CoC Thresholds Sub-Module
# - This module allows users to select which CoC-specific thresholds apply.
# - Selections are saved to the `selected_thresholds` table.
#-------------------------------------------------------------------------------

#' @title mod_coc_thresholds_ui
#' @noRd
mod_coc_thresholds_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "CoC Thresholds Requirements",
    value = id,
    card(
      em("Select the CoC Thresholds that all projects must meet to be considered for funding. HUD Thresholds are mandatory and not shown here."),
      uiOutput(ns("threshold_checkboxes_ui")) %>% withSpinner(),
      actionButton(ns("save_thresholds"), "Save CoC Threshold Selections", icon = icon("save"), class = "btn-primary")
    )
  )
}

#' @title mod_coc_thresholds_server
#' @noRd
mod_coc_thresholds_server <- function(id, user_coc, all_thresholds) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      all_thresholds = data.table(),
      selected_thresholds = c()
    )
    
    # Fetch all available CoC thresholds
    observe({
      req(DB_CON)
      rv$all_thresholds <- get_db_query(
        "SELECT threshold_id, threshold_text 
        FROM thresholds
        WHERE type = 'CoC' ORDER BY threshold_id"
      )
    })
    
    # Fetch currently selected thresholds for the active profile
    observe({
      req(user_coc$coc_instance_id)
      
      selected_q <- "SELECT threshold_id FROM selected_thresholds WHERE coc_instance_id = $1"
      selected_data <- get_db_query(selected_q, params = list(user_coc$coc_instance_id))
      
      rv$selected_thresholds <- selected_data$threshold_id
    })
    
    # Dynamically render the checkboxes based on available and selected data
    output$threshold_checkboxes_ui <- renderUI({
      req(nrow(rv$all_thresholds) > 0)
      
      checkboxGroupInput(
        inputId = ns("threshold_selection"),
        label = "CoC Threshold Requirements",
        choices = setNames(rv$all_thresholds$threshold_id, rv$all_thresholds$threshold_text),
        width = "100%",
        selected = rv$selected_thresholds
      )
    })
    
    # Save logic for threshold selections
    observeEvent(input$save_thresholds, {
      req(user_coc$coc_instance_id, username())
      
      current_selection <- as.integer(input$threshold_selection)
      previous_selection <- rv$selected_thresholds
      
      to_add <- setdiff(current_selection, previous_selection)
      to_remove <- setdiff(previous_selection, current_selection)
      
      tryCatch({
        # Add new selections
        if (length(to_add) > 0) {
          add_df <- data.frame(
            threshold_id = to_add,
            coc_instance_id = user_coc$coc_instance_id,
            created_by = username()
          )
          DBI::dbAppendTable(DB_CON, "selected_thresholds", add_df)
        }
        
        # Remove deselected items
        if (length(to_remove) > 0) {
          remove_q <- "DELETE FROM selected_thresholds WHERE coc_instance_id = $1 AND threshold_id = ANY($2)"
          dbExecute(DB_CON, remove_q, params = list(user_coc$coc_instance_id, to_remove))
        }
        
        # Update reactive value to reflect saved state
        rv$selected_thresholds <- current_selection
        
        shiny::showNotification("CoC Thresholds saved successfully.", type = "message")
      }, error = function(e) {
        shiny::showNotification(paste("Error saving thresholds:", e$message), type = "error")
      })
    })
  })
}


fetch_and_structure_rating_factors <- function(funding_action_type, coc_instance_id, selected_project_types, selected_target_populations) {
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
        SELECT rf.rating_factor_id, rf.project_type, rf.target_population, rf.rating_factor_text, rf.goal AS default_goal,
               rf.max_point_value AS default_points, fg.factor_group, fsg.factor_subgroup
        FROM rating_factors rf
        JOIN factor_groups fg ON rf.factor_group = fg.factor_group_id
        JOIN factor_subgroups fsg ON rf.factor_subgroup = fsg.factor_subgroup_id
        JOIN funding_actions fa ON rf.funding_action = fa.funding_action_id
        WHERE fa.funding_action IN ($1)
      "

  all_factors <- get_db_query(all_factors_q, params = list(funding_action_values))
  if(!is.null(selected_project_types)) all_factors <- all_factors[project_type %in% selected_project_types]
  if(!is.null(selected_target_populations)) all_factors <- all_factors[target_population %in% selected_target_populations]
  
  # Update rating_factor_text to include project_type and target_population
  all_factors <- all_factors %>%
    join(
      lookups[reference_type == "project_type", .("project_type_value" = value, reference_id)], 
      on = c("project_type" = "reference_id")
    ) %>%
    join(
      lookups[reference_type == "target_population", .("target_population_value" = value, reference_id)], 
      on = c("target_population" = "reference_id")
    ) %>%
    fmutate(
      rating_factor_text = glue::glue("{project_type_value} ({target_population_value}) - {rating_factor_text}")
    )
  
  selected_factors_q <- "
        SELECT rating_factor_id, goal, max_point_value
        FROM selected_rating_factors
        WHERE coc_instance_id = $1
      "
  selected_factors <- get_db_query(selected_factors_q, params = list(coc_instance_id))
  
  # Merge them to get the final state for the UI
  if (nrow(selected_factors) > 0) {
    merged_data <- data.table::merge(all_factors, selected_factors, by = "rating_factor_id", all.x = TRUE)
    merged_data[, selected := !is.na(max_point_value)]
    merged_data[is.na(goal), goal := default_goal]
    merged_data[is.na(max_point_value), max_point_value := default_points]
  } else {
    merged_data <- all_factors
    setnames(merged_data, c("default_goal", "default_points"), c("goal", "max_point_value"))
    merged_data[, selected := FALSE]
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

render_nested_factor_accordion_ui <- function(id_prefix, ns, data_groups_nested, placeholder_text = "No rating factors found.") {
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
          subgroup_data$rating_factor_text, 
          subgroup_data$goal, 
          subgroup_data$max_point_value, 
          subgroup_data$selected
        ), function(id, text, goal, points, selected) {
          fluidRow(
            column(1, checkboxInput(ns(paste0(id_prefix, "select_", id)), label = NULL, value = selected)),
            column(6, p(text)),
            column(2, textInput(ns(paste0(id_prefix, "goal_", id)), label = NULL, value = goal)),
            column(2, numericInput(ns(paste0(id_prefix, "points_", id)), label = NULL, value = points, step = 1)),
            column(1) # spacer
          )
        }
      )
        
      bslib::accordion_panel(
        title = subgroup_name,
        fluidRow(
          column(1, tags$b("Select")),
          column(6, tags$b("Rating Factor")),
          column(2, tags$b("Factor/Goal")),
          column(2, tags$b("Max Point Value")),
          column(1)
        ),
        hr(),
        factor_rows
      )
    })
    
    bslib::accordion_panel(
      title = group_name,
      bslib::accordion(
        !!!sub_accordion_items,
        # Ensure unique ID for inner accordions using id_prefix
        id = ns(paste0(id_prefix, "sub_accordion_", make.names(group_name))),
        multiple = TRUE,
        open = names(group_data_subgroups)[1]
      )
    )
  })
  
  bslib::accordion(
    !!!accordion_items_group,
    # Ensure unique ID for main accordion using id_prefix
    id = ns(paste0(id_prefix, "main_accordion")),
    multiple = TRUE,
    open = names(data_groups_nested)[1]
  )
}

#-------------------------------------------------------------------------------
# 3. RENEWAL/EXPANSION Project Factors Sub-Module
# - Displays rating factors applicable to Renewal and Expansion projects.
# - Allows customization of 'Goal' and 'Max Point Value'.
#-------------------------------------------------------------------------------

#' @title mod_renewal_factors_ui
#' @noRd
mod_renewal_factors_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Renewal/Expansion Project Rating Criteria",
    value = id,
    card(
      uiOutput(ns("renewal_factors_ui")) %>% withSpinner(),
      actionButton(ns("save_renewal_factors"), "Save Renewal/Expansion Criteria", icon = icon("save"), class = "btn-primary")
    )
  )
}

#' @title mod_renewal_factors_server
#' @noRd
mod_renewal_factors_server <- function(id, coc_instance_id, selected_project_types, selected_target_populations) {
  # This server function would be nearly identical to the new_factors_server,
  # but with a different filter on `funding_action`. A helper function could be
  # created to avoid code duplication. For clarity here, it is written out.
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    renewal_expand_factors_data <- reactive({
      req(user_coc$coc_instance_id)
      fetch_and_structure_rating_factors(
        "Renew", 
        user_coc$coc_instance_id, 
        selected_project_types(), 
        selected_target_populations()
      )
    })
    
    
    # Dynamically render the UI for rating factors using accordions
    # Dynamically render the UI for rating factors using nested accordions
    output$renewal_factors_ui <- renderUI({
      data_groups_nested <- renewal_expand_factors_data() # This now returns the nested list
      if (length(data_groups_nested) == 0) return(p("No rating factors found for Renewal/Expansion projects."))

      render_nested_factor_accordion_ui(
        id_prefix = "renewal_", # Unique prefix for Renewal/Expand inputs
        ns = ns,
        data_groups_nested = data_groups_nested,
        placeholder_text = "No rating factors found for Renewal/Expansion projects."
      )
    })
    
    # Save Logic for Renewal/Expansion Factors
    observeEvent(input$save_renewal_factors, {
      # This logic would be very similar to the thresholds save logic, but more complex.
      # It would iterate through all rendered factor inputs (e.g., input$select_1, input$goal_1, etc.),
      # compare against the database state, and perform INSERTS, UPDATES, or DELETES on the
      # `selected_rating_factors` table. A helper function is highly recommended here to manage
      # the complexity of parsing dynamic input IDs.
      
      # Example pseudo-code for save logic:
      # 1. Get all factor IDs that were rendered.
      # 2. Loop through each ID:
      # 3.   is_selected <- input[[paste0("select_", id)]]
      # 4.   is_in_db <- id %in% previously_saved_ids
      # 5.   IF is_selected AND NOT is_in_db -> INSERT new record with input values.
      # 6.   IF is_selected AND is_in_db -> UPDATE existing record with input values.
      # 7.   IF NOT is_selected AND is_in_db -> DELETE record.
      # 8. Wrap in a transaction.
      
      shiny::showNotification("Renewal/Expansion criteria save logic is complex and for demonstration purposes. Full implementation would occur here.", type = "message")
    })
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
      uiOutput(ns("new_factors_ui")),
      actionButton(ns("save_new_factors"), "Save New Project Criteria", icon = icon("save"), class = "btn-primary")
    )
  )
}

#' @title mod_new_factors_server
#' @noRd
mod_new_factors_server <- function(id, coc_instance_id, selected_target_populations) {
  # The server logic here is identical in structure to the renewal/expansion module,
  # differing only by the `funding_action` filter ('New').
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    new_factors_data <- reactive({
      req(user_coc$coc_instance_id)
      fetch_and_structure_rating_factors(
        "New", 
        user_coc$coc_instance_id, 
        selected_target_populations = selected_target_populations()
      )
    })
    
    output$new_factors_ui <- renderUI({ # Assuming you have a UI output for 'new' factors
      data_groups_nested <- new_factors_data()
      render_nested_factor_accordion_ui(
        id_prefix = "new_", # Unique prefix for New factors inputs
        ns = ns,
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
