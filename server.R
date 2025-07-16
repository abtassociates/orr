function(input, output, session) {
  # Reactive values
  selected_coc <- reactiveVal(NULL)
  projects_data <- reactiveVal(NULL)
  
  # Hide all panels except CoC selection initially
  observe({
    hideTab("nav", "review_projects")
    hideTab("nav", "rating_criteria")
    hideTab("nav", "rate_renewal")
    hideTab("nav", "rate_new")
    
    if (!is.null(selected_coc()) && selected_coc() != "") {
      showTab("nav", "review_projects")
      showTab("nav", "rating_criteria")
      showTab("nav", "rate_renewal")
      showTab("nav", "rate_new")
    }
  })
  
  # CoC selection and navigation
  observeEvent(input$next_btn, {
    if (input$coc_select != "") {
      selected_coc(input$coc_select)
      updateNavbarPage(session, "nav", selected = "review_projects")
      
      # Initialize projects data
      filtered_data <- hic_data %>%
        filter(CoC_Code == input$coc_select) %>%
        mutate(
          DV_Renewal = NA_character_,
          Grant_Number = NA_character_,
          CoC_Funding_Requested = NA_real_,
          Funding_Action = ifelse(McKinney_Vento == "No", "Ignore", NA_character_)
        )
      projects_data(filtered_data)
    }
  })
  
  # Update organization filter choices when CoC is selected
  observe({
    req(projects_data())
    orgs <- unique(projects_data()$Organization_Name)
    updateSelectInput(session, "filter_org",
                     choices = c("All", sort(orgs)))
  })
  
  # Filtered projects data
  filtered_projects <- reactive({
    req(projects_data())
    data <- projects_data()

    # First filter out "Ignore" projects unless specifically requested
    if (!("Ignore" %in% input$filter_funding_action)) {
      data <- data %>% filter(is.na(Funding_Action) | Funding_Action != "Ignore")
    }
    
    # Apply filters
    if (!("All" %in% input$filter_funding_action) && length(input$filter_funding_action) > 0) {
      data <- data %>% filter(Funding_Action %in% input$filter_funding_action)
    }
    
    if (!("All" %in% input$filter_dv_renewal) && length(input$filter_dv_renewal) > 0) {
      data <- data %>% filter(DV_Renewal %in% input$filter_dv_renewal)
    }
    
    if (!("All" %in% input$filter_project_type) && length(input$filter_project_type) > 0) {
      data <- data %>% filter(Project_Type %in% input$filter_project_type)
    }
    
    if (!("All" %in% input$filter_target_pop) && length(input$filter_target_pop) > 0) {
      data <- data %>% filter(Target_Population %in% input$filter_target_pop)
    }
    
    if (!("All" %in% input$filter_org) && length(input$filter_org) > 0) {
      data <- data %>% filter(Organization_Name %in% input$filter_org)
    }
    
    data
  })
  
  # Projects table
  output$projects_table <- renderDT({
    req(filtered_projects())
    data <- filtered_projects() %>%
      select(-CoC_Code)  # Remove CoC Code column

    # Define which columns should be green (editable by user)
    user_columns <- c("DV_Renewal", "Grant_Number", "CoC_Funding_Requested", "Funding_Action")
    
    dt <- datatable(
      data,
      editable = "cell",
      filter = "top",
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        orderClasses = TRUE,
        columnDefs = list(
          list(
            targets = which(names(data) %in% user_columns),
            className = 'green-background'
          ),
          list(
            targets = which(names(data) == "Funding_Action") - 1,
            render = JS(
              "function(data, type, row, meta) {
                if (type === 'display') {
                  return data === null ? '' : data;
                }
                return data;
              }"
            )
          )
        )
      )
    )
    
    dt
  })

  # Update projects data when cell is edited
  observeEvent(input$projects_table_cell_edit, {
    req(projects_data())
    info <- input$projects_table_cell_edit
    str(info)
    
    # Get the current data
    data <- projects_data()
    
    # Get the row index in the filtered view
    row_idx <- info$row
    
    # Get the actual row index in the full dataset
    actual_row <- which(data$Project_Name == filtered_projects()$Project_Name[row_idx])
    
    # Adjust column index since we removed CoC_Code
    col_name <- names(filtered_projects())[info$col + 1]
    data[actual_row, col_name] <- info$value
    
    projects_data(data)
  }, ignoreInit = TRUE)
  
  # Update projects data when cell is edited
  observeEvent(input$cell_edit, {
    req(projects_data())
    print("cell was edited")
    data <- projects_data()
    # Adjust column index since we removed CoC_Code
    actual_col <- input$cell_edit$col + 1
    if (actual_col >= which(names(data) == "CoC_Code")) {
      actual_col <- actual_col + 1
    }
    data[input$cell_edit$row + 1, actual_col] <- input$cell_edit$value
    projects_data(data)
  }, ignoreInit = TRUE)

  # Reactive value for custom thresholds
  custom_thresholds <- reactiveVal(character(0))
  
  # Add new threshold
  observeEvent(input$add_threshold_btn, {
    showModal(modalDialog(
      title = "Add New Threshold Requirement",
      textInput("new_threshold", "Requirement Text"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_threshold", "Save", class = "btn-primary")
      )
    ))
  })
  
  # Save new threshold
  observeEvent(input$save_threshold, {
    req(input$new_threshold)
    current <- custom_thresholds()
    custom_thresholds(c(current, input$new_threshold))
    updateCheckboxGroupInput(session, "coc_thresholds",
                           choices = c(input$coc_thresholds, input$new_threshold),
                           selected = c(input$coc_thresholds, input$new_threshold))
    removeModal()
  })
  
  # Generate rating factors UI
  output$rating_factors_ui <- renderUI({
    # Define base factors
    base_factors <- list(
      "Performance Measures" = c(
        "Length of Stay",
        "Exits to Permanent Housing"
      ),
      "Serve High Needs Populations" = c(
        "Coordinated Assessment Score",
        "Project focuses on chronically homeless people"
      )
    )
    
    # Filter by project type and population
    selected_project_types <- if ("All" %in% input$rating_project_type_filter) {
      project_types
    } else {
      input$rating_project_type_filter
    }
    
    selected_populations <- if ("All" %in% input$rating_population_filter) {
      target_populations
    } else {
      input$rating_population_filter
    }
    
    # Generate UI elements
    tagList(
      accordion(
        lapply(names(base_factors), function(group) {
          accordion_panel(
            title = group,
            lapply(base_factors[[group]], function(factor) {
              lapply(selected_project_types, function(pt) {
                lapply(selected_populations, function(pop) {
                  div(
                    style = "margin-bottom: 15px; padding: 10px; border-bottom: 1px solid #eee;",
                    fluidRow(
                      column(4, 
                             checkboxInput(
                               paste0("factor_", make.names(paste(factor, pt, pop))),
                               paste0(factor, " (", pt, " - ", pop, ")"),
                               value = TRUE
                             )
                      ),
                      column(4,
                             numericInput(
                               paste0("goal_", make.names(paste(factor, pt, pop))),
                               "Factor/Goal",
                               value = 80,
                               min = 0,
                               max = 100
                             )
                      ),
                      column(4,
                             numericInput(
                               paste0("points_", make.names(paste(factor, pt, pop))),
                               "Max Point Value",
                               value = 20,
                               min = 0,
                               max = 100
                             )
                      )
                    )
                  )
                })
              })
            })
          )
        })
      ),
      div(
        style = "margin-top: 15px;",
        actionButton("add_rating_factor", "Add New Rating Factor", class = "btn-primary")
      )
    )
  })
  
  # New Project Rating Factors UI
  output$new_project_rating_factors_ui <- renderUI({
    # Define base factors for new projects
    new_project_factors <- list(
      "Experience" = c(
        "Experience with proposed population",
        "Basic program design"
      ),
      "Implementation" = c(
        "Timeline feasibility",
        "Financial capacity"
      )
    )
    
    selected_populations <- if ("All" %in% input$new_rating_population_filter) {
      target_populations
    } else {
      input$new_rating_population_filter
    }
    
    tagList(
      accordion(
        lapply(names(new_project_factors), function(group) {
          accordion_panel(
            title = group,
            lapply(new_project_factors[[group]], function(factor) {
              lapply(selected_populations, function(pop) {
                div(
                  style = "margin-bottom: 15px; padding: 10px; border-bottom: 1px solid #eee;",
                  fluidRow(
                    column(6, 
                           checkboxInput(
                             paste0("new_factor_", make.names(paste(factor, pop))),
                             paste0(factor, " (", pop, ")"),
                             value = TRUE
                           )
                    ),
                    column(6,
                           numericInput(
                             paste0("new_points_", make.names(paste(factor, pop))),
                             "Max Point Value",
                             value = 20,
                             min = 0,
                             max = 100
                           )
                    )
                  )
                )
              })
            })
          )
        })
      ),
      div(
        style = "margin-top: 15px;",
        actionButton("add_new_project_factor", "Add New Factor", class = "btn-primary")
      )
    )
  })
  
  # Add new rating factor
  observeEvent(input$add_rating_factor, {
    showModal(modalDialog(
      title = "Add New Rating Factor",
      textInput("new_factor_text", "Factor Text"),
      selectInput("new_factor_group", "Factor Group",
                 choices = c("Performance Measures", "Serve High Needs Populations")),
      numericInput("new_factor_goal", "Default Factor/Goal", value = 80),
      numericInput("new_factor_points", "Default Max Points", value = 20),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_rating_factor", "Save", class = "btn-primary")
      )
    ))
  })
  
  # Add new project rating factor
  observeEvent(input$add_new_project_factor, {
    showModal(modalDialog(
      title = "Add New Project Rating Factor",
      textInput("new_project_factor_text", "Factor Text"),
      numericInput("new_project_factor_points", "Default Max Points", value = 20),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_new_project_factor", "Save", class = "btn-primary")
      )
    ))
  })

  # Update project selection dropdowns
  observe({
    req(projects_data())

    # Get renewal/expansion projects
    renewal_projects <- projects_data() %>%
      filter(Funding_Action %in% c("Renew", "Expand")) %>%
      pull(Project_Name)
    
    # Get new projects
    new_projects <- projects_data() %>%
      filter(Funding_Action == "New") %>%
      pull(Project_Name)
    
    updateSelectInput(session, "rate_project_select",
                     choices = c("Select a project" = "", sort(renewal_projects)))
    
    updateSelectInput(session, "rate_new_project_select",
                     choices = c("Select a project" = "", sort(new_projects)))
  }, priority = 1)
  
  # Project info sidebar
  output$project_info_sidebar <- renderUI({
    req(input$rate_project_select)
    project_data <- projects_data() %>%
      filter(Project_Name == input$rate_project_select)
    
    div(
      p(strong("Organization:"), project_data$Organization_Name),
      p(strong("Project Type:"), project_data$Project_Type),
      p(strong("Target Population:"), project_data$Target_Population),
      p(strong("Grant Amount:"), 
        formatC(project_data$CoC_Funding_Requested, format="f", 
                digits=2, big.mark=","))
    )
  })
  
  # New project info sidebar
  output$new_project_info_sidebar <- renderUI({
    req(input$rate_new_project_select)
    project_data <- projects_data() %>%
      filter(Project_Name == input$rate_new_project_select)
    
    div(
      p(strong("Organization:"), project_data$Organization_Name),
      p(strong("Project Type:"), project_data$Project_Type),
      p(strong("Target Population:"), project_data$Target_Population),
      p(strong("Requested Amount:"), 
        formatC(project_data$CoC_Funding_Requested, format="f", 
                digits=2, big.mark=","))
    )
  })
  
  # HUD Requirements UI
  output$hud_requirements <- renderUI({
    req(input$rate_project_select)
    
    hud_reqs <- c(
      "Project is eligible under 24 CFR part 578",
      "Project has capacity to meet regulatory requirements",
      "Project quality thresholds are met",
      "Match requirements are met"
    )
    
    lapply(hud_reqs, function(req) {
      div(
        style = "margin-bottom: 15px;",
        radioButtons(
          paste0("hud_req_", make.names(req)),
          req,
          choices = c("Yes" = "yes", "No" = "no"),
          selected = character(0),
          inline = TRUE
        )
      )
    })
  })
  
  # CoC Requirements UI
  output$coc_requirements <- renderUI({
    req(input$rate_project_select, input$coc_thresholds)
    
    lapply(input$coc_thresholds, function(req) {
      div(
        style = "margin-bottom: 15px;",
        radioButtons(
          paste0("coc_req_", make.names(req)),
          req,
          choices = c("Yes" = "yes", "No" = "no"),
          selected = character(0),
          inline = TRUE
        )
      )
    })
  })
  
  # Project Rating Factors UI
  output$project_rating_factors <- renderUI({
    req(input$rate_project_select)
    project_data <- projects_data() %>%
      filter(Project_Name == input$rate_project_select)
    
    # Get applicable rating factors based on project type and population
    # This should match the factors defined in the Customize Rating Criteria tab
    
    accordion(
      accordion_panel(
        "Performance Measures",
        div(
          style = "padding: 15px;",
          numericInput("los_score", "Length of Stay Score",
                      value = NA, min = 0, max = 100),
          numericInput("exits_ph_score", "Exits to Permanent Housing Score",
                      value = NA, min = 0, max = 100)
        )
      ),
      accordion_panel(
        "Serve High Needs Populations",
        div(
          style = "padding: 15px;",
          numericInput("assessment_score", "Coordinated Assessment Score",
                      value = NA, min = 0, max = 100),
          numericInput("ch_score", "Chronic Homeless Focus Score",
                      value = NA, min = 0, max = 100)
        )
      )
    )
  })
  
  # Rating Summary UI
  output$renewal_rating_summary <- renderUI({
    req(input$rate_project_select)
    
    card(
      card_header("Rating Summary"),
      div(
        style = "padding: 15px;",
        h4("Threshold Requirements"),
        tags$ul(
          tags$li(
            "HUD Requirements: ",
            span(style = "color: green;", "Pass")
          ),
          tags$li(
            "CoC Requirements: ",
            span(style = "color: red;", "Fail")
          )
        ),
        h4("Rating Scores"),
        tags$ul(
          tags$li("Performance Measures: 75/100"),
          tags$li("Serve High Needs: 85/100")
        ),
        h3("Total Score: 160/200")
      )
    )
  })

    # Alternative Rating table
  output$alternative_rating_table <- renderDT({
    req(projects_data())
    
    # Get only projects that can be rated (not "Ignore")
    ratable_projects <- projects_data() %>%
      filter(!is.na(Funding_Action), Funding_Action != "Ignore") %>%
      mutate(
        Project_ID = row_number(),  # Add Project ID
        HUD_Threshold = NA_character_,  # Add threshold columns
        CoC_Threshold = NA_character_,
        Rating_Score = NA_real_
      ) %>%
      select(
        Project_ID,
        Grant_Number,
        Funding_Action,
        Project_Name,
        Organization_Name,
        Project_Type,
        Target_Population,
        HUD_Threshold,
        CoC_Threshold,
        Rating_Score
      )
    
    datatable(
      ratable_projects,
      editable = list(
        target = "cell",
        disable = list(columns = c(0:6)),  # Disable editing for first 7 columns
        type = list(
          HUD_Threshold = 'select',
          CoC_Threshold = 'select'
        ),
        options = list(
          HUD_Threshold = c("Yes", "No"),
          CoC_Threshold = c("Yes", "No")
        )
      ),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        columnDefs = list(
          list(
            targets = 7:9,  # Last 3 columns
            className = 'green-background'
          )
        )
      )
    )
  })

  # Update alternative rating data when cell is edited
  observeEvent(input$alternative_rating_table_cell_edit, {
    info <- input$alternative_rating_table_cell_edit
    str(info)
    
    # Get the current data
    data <- projects_data()
    
    # Get the row from the filtered/displayed data
    edited_row <- info$row + 1
    
    # Update the appropriate column based on what was edited
    col_idx <- info$col
    if (col_idx == 7) {  # HUD Threshold
      data$HUD_Threshold[edited_row] <- info$value
    } else if (col_idx == 8) {  # CoC Threshold
      data$CoC_Threshold[edited_row] <- info$value
    } else if (col_idx == 9) {  # Rating Score
      # Ensure the rating score is between 0 and 100
      score <- as.numeric(info$value)
      if (!is.na(score) && score >= 0 && score <= 100) {
        data$Rating_Score[edited_row] <- score
      }
    }
    
    # Update the reactive value
    projects_data(data)
  }, ignoreInit = TRUE)

  # Funding ceiling calculations and displays
  output$ard_display <- renderText({
    req(selected_coc())
    ard_value <- ard_data$Total_ARD[ard_data$CoC_Code == selected_coc()]
    paste("ARD: $", format(ard_value, big.mark=",", scientific=FALSE))
  })
  
  output$dv_bonus_display <- renderText({
    req(selected_coc())
    bonus_value <- ard_data$DV_Bonus[ard_data$CoC_Code == selected_coc()]
    paste("DV Bonus: $", format(bonus_value, big.mark=",", scientific=FALSE))
  })
  
  output$coc_bonus_display <- renderText({
    req(selected_coc())
    bonus_value <- ard_data$CoC_Bonus[ard_data$CoC_Code == selected_coc()]
    paste("CoC Bonus: $", format(bonus_value, big.mark=",", scientific=FALSE))
  })
  
  output$tier1_display <- renderText({
    req(selected_coc())
    tier1_value <- ard_data$Tier_1[ard_data$CoC_Code == selected_coc()]
    paste("Tier 1: $", format(tier1_value, big.mark=",", scientific=FALSE))
  })
  
  output$adjusted_ard_display <- renderText({
    req(selected_coc())
    tier1_value <- ard_data$Tier_1[ard_data$CoC_Code == selected_coc()]
    adjusted_ard <- tier1_value/0.9
    paste("Adjusted ARD: $", format(adjusted_ard, big.mark=",", scientific=FALSE))
  })
  
  output$yhdp_ard_display <- renderText({
    req(selected_coc())
    ard_value <- ard_data$Total_ARD[ard_data$CoC_Code == selected_coc()]
    tier1_value <- ard_data$Tier_1[ard_data$CoC_Code == selected_coc()]
    adjusted_ard <- tier1_value/0.9
    yhdp_ard <- ard_value - adjusted_ard
    paste("YHDP ARD: $", format(yhdp_ard, big.mark=",", scientific=FALSE))
  })
  
  output$tier2_display <- renderText({
    req(selected_coc())
    tier1_value <- ard_data$Tier_1[ard_data$CoC_Code == selected_coc()]
    coc_bonus <- ard_data$CoC_Bonus[ard_data$CoC_Code == selected_coc()]
    dv_bonus <- ard_data$DV_Bonus[ard_data$CoC_Code == selected_coc()]
    adjusted_ard <- tier1_value/0.9
    tier2 <- (adjusted_ard * 0.1) + coc_bonus + dv_bonus
    paste("Tier 2: $", format(tier2, big.mark=",", scientific=FALSE))
  })

  # Priorities table
  priorities_data <- reactiveVal(
    data.frame(
      Population = c(
        "All Families", "DV Families", "Chronically Homeless Families",
        "Veteran Families", "Parenting Youth",
        "All Individuals", "DV Individuals", "Chronically Homeless Individuals",
        "Veteran Individuals", "Single Youth"
      ),
      Enabled = TRUE,  # New column to track enabled/disabled state
      stringsAsFactors = FALSE
    )
  )

  output$priorities_table <- renderDT({
    data <- data.frame(
      Population = c(
        "All Families", "DV Families", "Chronically Homeless Families",
        "Veteran Families", "Parenting Youth",
        "All Individuals", "DV Individuals", "Chronically Homeless Individuals",
        "Veteran Individuals", "Single Youth"
      )
    )
    
    # Create columns for each project type
    project_types <- c("PSH", "RRH", "TH", "TH+RRH")
    
    for(pt in project_types) {
      data[[paste0(pt, "_Beds")]] <- NA_real_
      data[[paste0(pt, "_Funding")]] <- NA_real_
      data[[paste0(pt, "_Priority")]] <- NA_character_
    }
    
    # Create the header structure
    header <- list(
      Population = 'Population',
      PSH = c('Project Type', 'PSH'),
      RRH = c('Project Type', 'RRH'),
      TH = c('Project Type', 'TH'),
      'TH+RRH' = c('Project Type', 'TH+RRH')
    )
    
    datatable(
      data,
      selection = 'none',
      rownames = FALSE,
      container = tags$table(
        class = 'display',
        tags$thead(
          tags$tr(
            tags$th(rowspan = 2, 'Population'),
            lapply(project_types, function(pt) {
              tags$th(colspan = 3, pt)
            })
          ),
          tags$tr(
            lapply(rep(c("Beds", "Funding", "Priority"), length(project_types)), function(col) {
              tags$th(col)
            })
          )
        )
      ),
      editable = list(
        target = 'cell',
        disable = list(columns = c(0))
      ),
      options = list(
        pageLength = 10,
        dom = 't'
      )
    ) %>%
    formatStyle(
      'Population',
      target = 'row' #,
      # backgroundColor = JS(sprintf(
      #   "function(data, type, row, meta) {
      #     return $('#population_toggles-%s:checked').length ? 'white' : '#f5f5f5';
      #   }",
      #   1:10
      # ))
    )
  })
  
  # Handle population toggles
  observeEvent(input$population_toggles, {
    dataTableProxy("priorities_table") %>% 
      replaceData(priorities_data())
  })

  # Update priorities data when cell is edited
  observeEvent(input$priorities_table_cell_edit, {
    info <- input$priorities_table_cell_edit
    data <- priorities_data()
    
    # Update the value
    data[info$row + 1, info$col + 1] <- info$value
    
    priorities_data(data)
  })
}