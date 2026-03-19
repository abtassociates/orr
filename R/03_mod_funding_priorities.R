pop_grp_toggles <- expand.grid(
  pop = get_labelled_lookups("target_population", lookup_col = "value_long"),
  grp = get_labelled_lookups("population_group", lookup_col = "value_long")
) |>
  qDT() |>
  fmutate(
    pop_txt = gsub("Domestic Violence", "DV", names(pop)),
    grp_txt = names(grp)
  ) |>
  fsubset(!pop_txt %in% c("Not Applicable", "Human Immunodeficiency Virus")) |>
  setorder(-grp, pop) |>
  fmutate(
    full_text = fcase(
      pop_txt == "Youth" & grp_txt == "Families", "Parenting Youth",
      pop_txt == "Youth" & grp_txt == "Individuals", "Single Youth",
      default = paste(pop_txt, grp_txt)
    )
  )

mod_funding_priorities_ui <- function(id) {
  ns <- NS(id)

  coc_bonus_opportunities <- COC_NOFO_OPPORTUNITIES[
    bonus_type == "CoC Bonus", 
    setNames(coc_nofo_opportunity_id, full_text)
  ]
  
  dv_bonus_opportunities <- COC_NOFO_OPPORTUNITIES[
    bonus_type == "DV Bonus", 
    setNames(coc_nofo_opportunity_id, full_text)
  ]
  
  funding_input <- function(id, label) {
    shinyWidgets::autonumericInput(
      ns(id), 
      label, 
      value = "0",
      currencySymbol = "$", 
      currencySymbolPlacement = "p", 
      decimalPlaces = 0, 
      style="font-size:1em;"
    )
  }
  
  # Funding Ceilings + Priorities
  nav_panel(
    "Funding Ceilings + Priorities",
    value = id,
    icon = icon("usd"),
    card(
      min_height=300,
      card_header("General Funding Information"),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        funding_input("total_ard", "Annual Renewal Demand (ARD)"),
        funding_input("coc_bonus", "CoC Bonus"),
        funding_input("tier_1", "Tier 1"),
        funding_input("adjusted_ard", "Adjusted ARD"),
        funding_input("yhdp_ard", "YHDP ARD"),
        funding_input("tier_2", "Tier 2"),
        funding_input("dv_bonus", "DV Bonus"),
        funding_input("dv_ard", "DV ARD")
      )
    ),
    card(
      min_height=300,
      card_header("FY2026 HUD CoC Program NOFO Opportunities"),
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_title("Project Types to Consider for CoC Bonus"),
          layout_columns(
            col_widths = c(6, 6),
            checkboxGroupInput(
              ns("coc_bonus_types_1"),
              NULL,
              choices = coc_bonus_opportunities %>% head(length(.)/2) #needs to be %>% instead of |>
            ),
            checkboxGroupInput(
              ns("coc_bonus_types_2"),
              NULL,
              choices = coc_bonus_opportunities %>% tail(length(.)/2) #needs to be %>% instead of |>
            )
          )
        ),
        card(
          card_title("Project Types to Consider for DV Bonus"),
          checkboxGroupInput(
            ns("dv_bonus_types"),
            NULL,
            choices = dv_bonus_opportunities
          )
        )
      ),
      card_footer(
        style = "display: flex; justify-content: space-between; align-items: center;",
        actionButton(ns("save_opportunities"), "Save CoC Nofo Opportunities", icon = icon("save"), class="btn-primary")
      )
    ), # end coc nofo opportunities card
    card(
      card_header("Funding Ceilings and Priorities by Project Type and Population"),
      fill = FALSE,
      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = "22%",
          checkboxGroupInput(
            ns("population_toggles"),
            label = "Enable/Disable Populations",
            choices = pop_grp_toggles$full_text,
            selected =  c("General Families", "General Individuals", "Single Youth")
          )
        ),
        div(
          id = ns("priorities_help"),
          helpText("Double-click a cell to edit")
        ),
        DTOutput(ns("priorities_table"))
      )
    )
  )
}

mod_funding_priorities_server <- function(id, nav_control, user_coc, parent_session, module_returns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh_trigger <- reactiveValues(
      coc_nofo_opportunities = 0,
      coc_funding_priorities = 0
    )
    coc_nofo_opportunities <- reactiveVal()
    coc_funding_priorities <- reactiveVal()
    formatted_coc_funding_priorities <- reactiveVal()
    
    # Populate Funding Info -----------
    ard_field_names <- c(
      "total_ard",
      "tier_1",
      "tier_2",
      "adjusted_ard",
      "yhdp_ard",
      "dv_ard",
      "coc_bonus",
      "dv_bonus"
    )
    
    hud_ard_coc_data <- reactive({
      HUD_ARD_REPORT[coc == user_coc$coc] |>
        fmutate(
          tier_2 = estimated * 0.1 + coc_bonus + dv_bonus,
          adjusted_ard = round(tier_1/0.9, 0),
          yhdp_ard = estimated - adjusted_ard,
          dv_ard = as.numeric(NA)
        ) |>
        frename(estimated = "total_ard")
    })
    
    observeEvent(user_coc$coc, {
      lapply(ard_field_names, function(i) {
        updateAutonumericInput(
          session, 
          i, 
          value = hud_ard_coc_data()[[i]]
        )
        if(i != "dv_ard") shinyjs::disable(i)
      })
    })
    
    
    # Priorities ------------
    format_coc_funding_priorities <- function(coc_funding_priorities_db) {
      coc_funding_priorities_db |>
        # 1. Need to get this into app-ready data structure, e.g.
        #                           Population PSH_Beds PSH_Funding PSH_Priority RRH_Beds RRH_Funding RRH_Priority TH_Beds TH_Funding TH_Priority TH+RRH_Beds TH+RRH_Funding TH+RRH_Priority
        #                               <char>    <num>       <num>       <char>    <num>       <num>       <char>   <num>      <num>      <char>       <num>          <num>          <char>
        # 1:                     All Families       NA          NA         <NA>       NA          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        # 2:                      DV Families       NA          NA         <NA>       NA          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        # 3:    Chronically Homeless Families       NA          NA         <NA>       10          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        # 4:                 Veteran Families       NA        5000         <NA>       NA          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        # 5:                  Parenting Youth       NA          NA         <NA>       NA          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        # 6:                  All Individuals       NA          NA         <NA>       NA          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        # 7:                   DV Individuals       NA          NA         <NA>       NA          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        # 8: Chronically Homeless Individuals       NA          NA         <NA>       NA          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        # 9:              Veteran Individuals       NA          NA         <NA>       NA          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        # 10:                     Single Youth      NA          NA         <NA>       NA          NA         <NA>      NA         NA        <NA>          NA             NA            <NA>
        
        # Get full "Population" text (target_pop + pop_grp)
        join(
          pop_grp_toggles |> fselect(Population = full_text, pop, grp),
          on = c("target_population" = "pop", "population_group" = "grp"),
          how = "right"
        ) |>
        fselect(Population, project_type, beds, funding, priority) |>
        # For each Population, get all main project types, still in long format
        join(
          expand.grid(
            Population = pop_grp_toggles$full_text,
            project_type = LOOKUPS[reference_type == "project_type" & value %in% MAIN_PROJECT_TYPES]$reference_id
          ),
          how = "right"
        ) %>%
        # convert project type number to label
        fmutate(
          project_type = get_lookup_label(project_type, 'project_type'),
          priority = convert_to_factor(., "priority")
        ) |>
        # pivot to app data structure
        pivot(
          ids = "Population",
          how="wider",
          names = "project_type",
          transpose = "names"
        ) |>
        colorderv(
          paste0("^", gsub("+", "\\+", MAIN_PROJECT_TYPES, fixed = TRUE), "_"), 
          regex=TRUE, 
          pos="end"
        )
    }
    
    observeEvent(c(
      user_coc$coc_version_id, 
      refresh_trigger$coc_funding_priorities
    ), {
      req(user_coc$coc_version_id)
      
      ## Store priorities --------
      coc_funding_priorities(
        get_coc_funding_priorities(user_coc$coc_version_id)
      )
      
      formatted_coc_funding_priorities(
        format_coc_funding_priorities(coc_funding_priorities())
      )
      
      ## Population toggles --------
      selected_populations <- formatted_coc_funding_priorities() %>%
        dplyr::filter(dplyr::if_any(-Population, ~ !is.na(.)))
      
      updateCheckboxGroupInput(
        session,
        "population_toggles",
        selected = if(fnrow(selected_populations) > 0) selected_populations$Population else input$population_toggles
      )
    })
    
    ## CoC NOFO Opportunities ------
    observeEvent(c(
      user_coc$coc_version_id, 
      refresh_trigger$coc_nofo_opportunities
    ), {
      req(user_coc$coc_version_id)
      
      coc_nofo_opportunities(
        get_coc_nofo_opportunities(user_coc$coc_version_id)
      )
      
      selected_coc_nofo_opportunities <- coc_nofo_opportunities()[selected == 1]
      
      coc_bonus_types <- selected_coc_nofo_opportunities |>
        fsubset(bonus_type == get_lookup_refid("CoC Bonus", "bonus_type"))
      
      updateCheckboxGroupInput(
        session, 
        "coc_bonus_types_1", 
        selected = coc_bonus_types[coc_nofo_opportunity_id %in% 1:4]$coc_nofo_opportunity_id
      )
      updateCheckboxGroupInput(
        session, 
        "coc_bonus_types_2", 
        selected = coc_bonus_types[coc_nofo_opportunity_id %in% 5:8]$coc_nofo_opportunity_id
      )
      
      dv_bonus_types <- selected_coc_nofo_opportunities |>
        fsubset(bonus_type == get_lookup_refid("DV Bonus", "bonus_type"))
      
      updateCheckboxGroupInput(
        session, 
        "dv_bonus_types", 
        selected = dv_bonus_types$coc_nofo_opportunity_id
      )
    })
    
    
    # Priorities section -----------------
    output$priorities_table <- renderDT({
      req(coc_funding_priorities())
      
      show_priorities_row <- length(input$population_toggles) > 0
      
      shinyjs::toggle("priorities_help", condition = show_priorities_row)
      
      shiny::validate(need(
        show_priorities_row == TRUE,
        "Click population in the left-hand sidebar to enter priorities for that population"
      ))
      
      data <- formatted_coc_funding_priorities() |>
        fsubset(Population %in% input$population_toggles)
      
      initialize_inline_edit_table_ui(
        data = data,
        tableID = ns("priorities_table"),
        formatting = list(
          function(x) formatStyle(
            x,
            columns = seq(4, ncol(data), by = 3),  # Priority columns (every 3rd column starting from 3)
            `border-right` = "1px solid black"
          ),
          function(x) formatCurrency(
            x,
            columns = seq(3, ncol(data), by = 3),
            currency = "$", 
            mark = ",",
            digits = 0
          )
        ), 
        cols_to_disable = "Population",
        container = tags$table(
          tags$thead(
            tags$tr(
              tags$th(rowspan = 2, 'Population'),
              lapply(MAIN_PROJECT_TYPES, function(pt) {
                tags$th(colspan = 3, pt, style = "border-right: 1px solid black; text-align: center")
              })
            ),
            tags$tr(
              lapply(rep(c("Beds", "Funding", "Priority"), length(MAIN_PROJECT_TYPES)), function(col) {
                tags$th(col, style=ifelse(col == "Priority", "border-right: 1px solid black", ""))
              })
            )
          )
        ),
        options = list(
          dom = 't',
          searching = FALSE,
          info = FALSE
        ),
        filter = 'none',
        callback_js = glue::glue(
          "$(document).on('mouseenter', 'table.dataTable tbody tr', function() {{
            $(this).css('background-color', '{USER_ENTRY_BG_COLOR}');
          }});
          $(document).on('mouseleave', 'table.dataTable tbody tr', function() {{
            $(this).css('background-color', 'inherit');
          }});
        "),
        has_double_header = TRUE
      ) #end initialize_data_Table
    }, server = FALSE)
    
    priorities_table_proxy <- dataTableProxy(ns("priorities_table"),session = session)
    
    observe({
      req(formatted_coc_funding_priorities())
      replaceData(priorities_table_proxy, formatted_coc_funding_priorities(), resetPaging = FALSE)
    })
    
    
    # Update priorities data in table and db when cell is edited
    observeEvent(input$priorities_table_cell_edit, {
      info <- input$priorities_table_cell_edit
      
      # Update the full dataset 
      current_data <- formatted_coc_funding_priorities()

      # Get the population name from the row that was displayed
      # This is trickier because the view is filtered. We need to map the
      # viewed row index back to the full data index.
      displayed_data <- current_data[Population %in% input$population_toggles]
      population_to_update <- displayed_data[info$row, Population]
      full_data_row_index <- which(current_data$Population == population_to_update)
      
      # only proceed if they changed anything:
      old_val <- current_data[full_data_row_index, (info$col + 1), with=FALSE][[1]]
      req(!identical(old_val, info$value))
      
      # Update the value in the full dataset, so we can update the reactive and datatable proxy
      # The column index needs + 1 because datatable is 0 indexed
      current_data[full_data_row_index, (info$col + 1) := info$value]

      ## Update database -------------
      changed_data <- current_data[full_data_row_index, c(1, info$col + 1), with=FALSE] |>
        tidyr::pivot_longer(
          cols = -Population,
          names_to = c("project_type", ".value"), # ".value" tells it to keep the second part as column headers
          names_sep = "_"
        ) |>
        join(
          pop_grp_toggles,
          on = c("Population" = "full_text")
        ) |>
        frename(pop = target_population, grp = population_group) |>
        fmutate(
          project_type = get_lookup_refid(project_type, ref_type = "project_type"),
          priority = get_lookup_refid(priority, "ref_type" = "priority")
        ) |>
        join(
          coc_funding_priorities() |> 
            fselect(project_type, target_population, population_group, date_updated),
          on = c("project_type", "target_population", "population_group")
        )
      
      # bed, funding, or priority
      metric_name <- names(changed_data)[3]
      
      updated_coc_funding_priorities <- list(
        user_coc$coc_version_id,
        changed_data$project_type,
        changed_data$target_population,
        changed_data$population_group,
        changed_data[[metric_name]],
        user_coc$username,
        date_updated = changed_data$date_updated
      )
      
      needs_refresh <- update_coc_funding_priorities_db(
        get_db_pool(), 
        metric_name, 
        updated_coc_funding_priorities
      )
      
      # if(needs_refresh)
        refresh_trigger$coc_funding_priorities <- refresh_trigger$coc_funding_priorities + 1
      
      formatted_coc_funding_priorities(current_data)
    }) # end observeEvent
    
    
    
    # Saving Data ---------------
    ## Get data to save -------------
    get_updated_coc_nofo_opportunities <- function(params) {
      params$coc_nofo_opportunities |>
        fmutate(
          coc_version_id = params$coc_version_id,
          selected_new = coc_nofo_opportunity_id %in% params$nofo_opportunity_ids,
          created_by = params$created_by
        ) |>
        fsubset(selected_new != fcoalesce(selected, FALSE)) |>
        fselect(coc_version_id, coc_nofo_opportunity_id, selected_new, created_by, date_updated)
    }
    
    ## Save to db -------------
    observeEvent(input$save_opportunities, {
      req(user_coc$coc_version_id, user_coc$username)
      
      updated_coc_nofo_opportunities <- get_updated_coc_nofo_opportunities(
        params = list(
          coc_nofo_opportunities = coc_nofo_opportunities(),
          coc_version_id = user_coc$coc_version_id,
          nofo_opportunity_ids = as.integer(c(
            input$coc_bonus_types_1,
            input$coc_bonus_types_2,
            input$dv_bonus_types
          )),
          created_by = user_coc$username
        )
      )
      
      # update database
      needs_refresh <- FALSE
      needs_refresh <- update_coc_nofo_opportunities_db(
        get_db_pool(), 
        updated_coc_nofo_opportunities
      )
      
      # if(needs_refresh)
        refresh_trigger$coc_nofo_opportunities = refresh_trigger$coc_nofo_opportunities + 1
    })
    
  })
}
