mod_ranking_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Ranking",
    value = "ranking",

    # 1. Top Summary Widgets
    layout_columns(
      fill = FALSE,
      mod_ranking_widget_ui(ns("coc_bonus")),
      mod_ranking_widget_ui(ns("tier_1")),
      mod_ranking_widget_ui(ns("tier_2")),
      mod_ranking_widget_ui(ns("dv_bonus")),
      mod_ranking_widget_ui(ns("exceeding"))
    ),
    
    actionButton(
      ns("btn_conduct_ranking"), 
      "Conduct Ranking / Reset", 
      class = "btn-primary btn-lg w-100", 
      icon = icon("calculator")
    ),
    
    
    # 4. Drag and Drop Zones
    DTOutput(ns("ui_ranked_list")) |> shinycssloaders::withSpinner(),
    
    br(),
    br(),
    DTOutput(ns("ui_exlcluded_list"))
  )
}

mod_ranking_server <- function(id, nav_control, user_coc, parent_session, module_returns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    projects <- reactive({
      req(user_coc$coc_version_id)
      
      get_db_query(
        "SELECT p.*, 
        r.rank, r.tier, s.rating_score, r.coc_funding_recommendation, 
        fp.coc_funding_priority_id, fp.beds, fp.funding, fp.priority, fp.population_group AS fp_pop_grp, 
        no.coc_nofo_opportunity_id, no.bonus_type, no.population_group AS no_pop_grp
        FROM projects p
        LEFT JOIN ranking r ON p.project_id = r.project_id AND r.coc_version_id = p.coc_version_id
        LEFT JOIN rating_scores s ON p.project_id = s.project_id
        LEFT JOIN coc_funding_priorities fp ON fp.project_type = p.project_type AND fp.target_population = p.target_population AND fp.coc_version_id = p.coc_version_id
        LEFT JOIN selected_coc_nofo_opportunities sno ON sno.coc_version_id = p.coc_version_id
        LEFT JOIN coc_nofo_opportunities no ON no.coc_nofo_opportunity_id = sno.coc_nofo_opportunity_id AND no.funding_action = p.funding_action AND no.project_type = p.project_type AND no.target_population = p.target_population
        WHERE p.coc_version_id = $1", 
        params = user_coc$coc_version_id
      ) |>
        fselect(-coc_version_id, -date_created, -date_updated, -updated_by, -amount_other_public_funding, -amount_private_funding) %>% # needs to be %>% instead of |>
        fmutate(
          funding_action = convert_to_factor(., "funding_action"),
          project_type = convert_to_factor(., "project_type"),
          target_population = convert_to_factor(., "target_population"),
          dv_renewal = factor_yesno(dv_renewal),
          mckinneyvento = factor_yesno(mckinneyvento),
          mckinneyventoyhdp = factor_yesno(mckinneyventoyhdp),
          is_dedicated_ch_fam = factor_yesno(is_dedicated_ch_fam),
          is_dedicated_ch_ind = factor_yesno(is_dedicated_ch_ind),
          is_dedicated_dv = factor_yesno(is_dedicated_dv),
          priority = convert_to_factor(., "priority")
        ) |>
        fsubset(funding_action != "Ignore")
    })
    
    mod_ranking_widget_server("coc_bonus", projects, user_coc, "Coc Bonus Funding")
    mod_ranking_widget_server("tier_1", projects, user_coc, "Tier 1 (Adj ARD * 90%)")
    mod_ranking_widget_server("tier_2", projects, user_coc, "Tier 2 (Adj ARD * 10% + CoC Bonus) + DV Bonus")
    mod_ranking_widget_server("dv_bonus", projects, user_coc, "DV Bonus Funding")
    mod_ranking_widget_server("exceeding", projects, user_coc, "Projects Exceeding Adj ARD + CoC Bonus + DV Bonus")
    
    # Reactive values to store the data and bucket limits
    rv <- reactiveValues(
      limits = list(tier1 = 0, tier2 = 0, dv = 0),
      RankedProjects = NULL
    )
    
    # ranked_project_ids <- reactiveValues(new = NULL)
    
    
    #Initialization: Fetch Limits
    observe({
      req(DB_CON) # Ensure DB connection exists
      req(user_coc$coc)
      
      df_limits <- HUD_ARD_REPORT[coc == user_coc$coc]
      
      rv$limits$tier1 <- df_limits$tier_1
      rv$limits$tier2 <- (df_limits$estimated * 0.10) + df_limits$coc_bonus # Logic from header image
      rv$limits$dv <- df_limits$dv_bonus
    })
    
    ranked_projects <- reactive({
      N <- fnrow(projects())
      
      projects() |> 
        fsubset(!funding_action %in% c("Reallocate", "Ineligible", "NOT RATED")) |>
        fmutate(
          # TEST
          rating_score = sample.int(100, size = N, replace = TRUE),
          coc_funding_requested = as.double(sample.int(600001, size = N, replace = TRUE) + 99999), #between 100k and 400k
          priority = sample(levels(priority), size = N, replace = TRUE)
        ) |>
        fmutate(
          # default funding recommendation to requested
          coc_funding_recommendation = fcoalesce(coc_funding_recommendation, coc_funding_requested)
        ) |>
        roworder(
          -priority, 
          -rating_score
        ) |>
        fmutate(
          # Straddle Logic: If the start of the project is within limit, it's Tier 1.
          # Logic: If cum_funding <= limit OR (cum_funding - requested) < limit
          cum_funding = cumsum(coc_funding_requested),
          tier = fcase(
            (cum_funding - coc_funding_requested) <= rv$limits$tier1, "Tier 1 (Adjusted ARD * 90%)",
            (cum_funding - coc_funding_requested) > rv$limits$tier1 & (cum_funding - coc_funding_requested) < rv$limits$tier1 + rv$limits$tier2, "Tier 2 (Adjusted ARD * 10% + CoC Bonus) + DV Bonus",
            default = "Projects Exceeding Adjusted ARD + CoC bonus + DV Bonus"
          ),
          rank = factor(fifelse(tier == "Excluded", NA, seq_along(project_id)))
        )
    })
    
    observeEvent(ranked_projects(), {
      rv$RankedProjects <- ranked_projects()
    })
    
    
    excluded_projects <- reactive({
      projects() |> 
        fsubset(funding_action %in% c("Reallocate", "Ineligible", "NOT RATED")) |>
        fmutate(tier = "Excluded")
    })
    
    # 4. Render UI Lists
    add_tier_headers <- function(projects) {
      rbindlist(list(
        data.table(tier = "Tier 1"),
        projects[tier == "Tier 1"],
        data.table(tier = "Tier 2"),
        projects[tier == "Tier 2"]
      ), fill = TRUE)
    }
    
    render_projects_dt <- function(projects) {
      
      final <- projects |>
        fselect(
          tier,
          rank,
          priority,
          rating_score,
          bonus_type,
          funding_action,
          grant_number,
          project_type,
          target_population,
          organization_name,
          project_name,
          coc_funding_requested,
          coc_funding_recommendation,
          all_fam_beds,
          dv_fam_beds,
          ch_fam_beds,
          vet_fam_beds,
          par_youth_beds,
          all_ind_beds,
          dv_ind_beds,
          total_ch_ind_beds,
          vet_ind_beds,
          single_youth_beds,
          "100% Dedicated + or CH Fam" = is_dedicated_ch_fam,
          "100% Dedicated + or CH Ind" = is_dedicated_ch_ind,
          "100% DV" = is_dedicated_dv
        ) |>
        # add_tier_headers() |>
        frename(function(x) gsub("_", " ", toupper(x)))
        
      disabled_cols <- setdiff(
        0:(ncol(final) - 1), 
        which(names(final) == "COC FUNDING RECOMMENDATION") - 1
      )

      project_id_col_idx <- which(names(final) == "PROJECT ID") - 1
      
      # Get all column indices except coc_funding_recommendation
      datatable(
        final,
        rownames = FALSE,
        # escape = FALSE,
        editable = list(
          target = 'cell',
          disable = list(columns = disabled_cols)
        ),
        extensions = c("RowReorder", "FixedHeader", "RowGroup"),
        selection = 'none',
        options = list(
          # dom = 'tip',
          rowReorder = TRUE,
          fixedHeader = TRUE,
          rowGroup = list(
            dataSrc = which(names(final) == "TIER") - 1,  # Group by TIER column (0-indexed)
            startRender = JS("
              function(rows, group) {
                return $('<tr class=\"group\"/>').append('<td colspan=\"' + rows.columns().header().length + '\" style=\"background-color: #4CAF50; color: white; font-weight: bold; font-size: 18px; text-align: center; padding: 12px;\">' + group + '</td>');
              }"
            )
          ),
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          info = FALSE,
          columnDefs = list(
            list(targets = 0, visible = FALSE)  # Hide TIER column
          )
        ), # end options
        # for tracking reordered rows
        callback = JS(sprintf("
          table.on('row-reorder', function(e, details, edit){
            newOrder = table.rows().indexes().toArray();
            for(i of details) {
              newOrder[i.oldPosition] = i.newPosition;
            }
            
            Shiny.setInputValue('%s', newOrder, {priority: 'event'});
          });",
                              ns("newOrder")
        ))
      ) |>
        formatStyle(
          'RANK',
          target = 'row',
          backgroundColor = styleEqual(c("Tier 1", "Tier 2"), c('#4CAF50 !important', '#2196F3 !important')),
          color = styleEqual(c("Tier 1", "Tier 2"), c('white', 'white')),
          fontWeight = styleEqual(c("Tier 1", "Tier 2"), c('bold', 'bold')),
          fontSize = styleEqual(c("Tier 1", "Tier 2"), c("1.5rem","1.5rem"))
        ) |>
        formatStyle(
          'COC FUNDING RECOMMENDATION',  # Replace with your actual column name
          backgroundColor = '#90EE90',
          fontWeight = 'bold'
        ) |>
        formatCurrency(
          c('COC FUNDING REQUESTED', 'COC FUNDING RECOMMENDATION'),
          currency = "$",
          digits = 0
        )
      
    }
    
    observeEvent(input$newOrder, {
      req(rv$RankedProjects)
      req(input$newOrder)

      # Reorder data by new rank and re-rank
      new_data <- rv$RankedProjects |>
        fmutate(new_rank = input$newOrder) |>
        roworder(new_rank) |>
        fmutate(
          rank = seq_along(project_id),
          new_rank = NULL
        )
      
      # Update reactive value
      rv$RankedProjects <- new_data
      
      # Update the table display
      replaceData(proxy, new_data, resetPaging = FALSE)
    }, ignoreInit = TRUE)
    
    
    output$ui_ranked_list <- renderDT({
      req(ranked_projects())
      render_projects_dt(rv$RankedProjects)
    })
    
    proxy <- dataTableProxy(ns("ui_ranked_list"))
    
    output$ui_excluded_list <- renderDT({
      req(projects())
      
      render_projects_dt(excluded_projects())
    })
    
    
    # 6. Calculate Summaries (Dynamic based on list contents)
    
    calc_total <- function(ids) {
      if(length(ids) == 0 || is.null(rv$projects)) return(0)
      rv$projects |> 
        filter(project_id %in% ids) |> 
        summarise(tot = sum(coc_funding_requested, na.rm=TRUE)) |> 
        pull(tot)
    }
    
    output$t1_allocated <- renderText({
      amt <- calc_total(rv$tier1_ids)
      scales::dollar(amt)
    })
    
    output$t1_limit <- renderText({ scales::dollar(rv$limits$tier1) })
    
    output$t1_remaining <- renderText({
      amt <- calc_total(rv$tier1_ids)
      rem <- rv$limits$tier1 - amt
      scales::dollar(rem)
    })
    
    output$t2_allocated <- renderText({
      amt <- calc_total(rv$tier2_ids)
      scales::dollar(amt)
    })
    output$t2_limit <- renderText({ scales::dollar(rv$limits$tier2) })
    output$t2_exceeded <- renderText({
      amt <- calc_total(rv$tier2_ids)
      diff <- amt - rv$limits$tier2
      if(diff > 0) scales::dollar(diff) else "$0"
    })
    
    output$dv_allocated <- renderText({
      # Logic: Sum of DV projects in Tier 1 or Tier 2? usually DV Bonus is separate bucket
      # Assuming DV projects within Tier 1/2 count towards this.
      if(is.null(rv$projects)) return("$0")
      
      # Find ids in T1 or T2 that are DV
      active_ids <- c(rv$tier1_ids, rv$tier2_ids)
      amt <- rv$projects |> 
        fsubset(project_id %in% active_ids, target_population_text == "DV") |> 
        fsummarize(tot = sum(coc_funding_requested, na.rm=TRUE))
      
      scales::dollar(amt$tot)
    })
    
    output$dv_limit <- renderText({ scales::dollar(rv$limits$dv) })
    
    # 7. Database Write-Back (Skeleton)
    # You would add an "ObserveEvent" on a "Save" button to loop through rv$tier1_ids
    # and update the 'ranking' table with their index (Rank) and Tier name.
  })
}