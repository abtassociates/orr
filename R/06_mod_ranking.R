mod_ranking_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Ranking",
    value = "ranking",
    
    layout_columns(
      fill = FALSE,
      col_widths = c(4, 4, 4),
      actionButton(
        ns("btn_conduct_ranking"), 
        "Conduct Ranking / Reset",
        class = "btn-primary btn-lg w-100", 
        icon = icon("calculator")
      ),
      actionButton(
        ns("btn_adjust_tiers"), 
        "Adjust Tiers after Funding Changes", 
        class = "btn-info btn-lg w-100", 
        icon = icon("arrows-rotate")
      ),
      actionButton(
        ns("btn_save_ranking"), 
        "Save Ranking", 
        class = "btn-success btn-lg w-100", 
        icon = icon("save")
      )
    ),
    
    
    # 1. Top Summary Widgets
    layout_columns(
      fill = FALSE,
      mod_ranking_widget_ui(ns("coc_bonus")),
      mod_ranking_widget_ui(ns("tier_1")),
      mod_ranking_widget_ui(ns("tier_2")),
      mod_ranking_widget_ui(ns("dv_bonus")),
      mod_ranking_widget_ui(ns("exceeds"))
    ),
    
    # 4. Drag and Drop Zones
    DTOutput(ns("ui_ranked_list")),
    
    br(),
    br(),
    DTOutput(ns("ui_excluded_list")),
    br(),
    br(),
    DTOutput(ns("ui_yhdp_ren")),
    br(),
    br(),
    DTOutput(ns("ui_yhdp_oth"))
  )
}

mod_ranking_server <- function(id, nav_control, user_coc, parent_session, module_returns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    coc_ard_data <- reactive({
      get_coc_hud_ard_data(user_coc$coc)
    })
    
    get_allocated_funding <- function(condition) {
      req(fnrow(rv$ranked) > 0)
      sum(rv$ranked[eval(condition)]$coc_funding_recommendation, na.rm = TRUE)
    }
    alloc_tier1 <- reactive( get_allocated_funding(quote(tier == "Tier 1")) )
    alloc_tier2 <- reactive( get_allocated_funding(quote(tier == "Tier 2")) )
    alloc_coc <- reactive( get_allocated_funding(quote(coc_selected == TRUE)) )
    alloc_exceed <- reactive( get_allocated_funding(quote(tier == "Projects Exceeding ARD")) )
    
    alloc_dv <- reactive({
      req(fnrow(rv$ranked) > 0)
      list(
        total = sum(rv$ranked[dv_selected == TRUE]$coc_funding_recommendation, na.rm = TRUE),
        t1 = sum(rv$ranked[dv_selected == TRUE & tier == "Tier 1"]$coc_funding_recommendation, na.rm = TRUE),
        t2 = sum(rv$ranked[dv_selected == TRUE & tier == "Tier 2"]$coc_funding_recommendation, na.rm = TRUE)
      )
    })
    
    mod_ranking_widget_server("coc_bonus", alloc_coc, coc_ard_data()$coc_bonus, "CoC Bonus", bg_color = "pink", icon_name = "plus-circle")
    mod_ranking_widget_server("tier_1", alloc_tier1, coc_ard_data()$tier_1, "Tier 1 (Adj ARD * 90%)", bg_color = "blue", icon_name = "layer-group")
    mod_ranking_widget_server("tier_2", alloc_tier2, coc_ard_data()$tier_2, "Tier 2 (Adj ARD * 10% + CoC Bonus + DV Bonus)", bg_color = "orange", icon_name = "layer-group")
    mod_ranking_widget_server("dv_bonus", alloc_dv, coc_ard_data()$dv_bonus, "DV Bonus", bg_color = "brown", icon_name = "heart")
    mod_ranking_widget_server("exceeds", alloc_exceed, Inf, "Exceeding ARD", bg_color = "black", icon_name = "exclamation-triangle")
    
    # Reactive values to store the data and bucket limits
    rv <- reactiveValues(
      # limits = list(tier1 = 0, tier2 = 0, dv = 0),
      # RankedProjects = NULL,
      ranked = NULL,
      yhdp_ren = NULL,
      yhdp_oth = NULL,
      excluded = NULL
    )
    # ranked_project_ids <- reactiveValues(new = NULL)
    
    observeEvent(user_coc$coc_version_id, { process_data(force_reset = FALSE) }, ignoreInit = TRUE)
    observeEvent(input$btn_conduct_ranking, { process_data(force_reset = TRUE) })
    
    # Core Function: Recalculate Tiers and Bonuses
    recalculate_ranking <- function(dt) {
      if(is.null(dt) || nrow(dt) == 0) return(dt)
      
      dt[, rank := .I]
      
      # 1. Evaluate Bonus Eligibility (based on specs formulas)
      dt[, is_coc_eligible := grepl("New|Expand", funding_action) & (
        (project_type == "PSH" & ((total_ch_ind_beds > 0 & is_dedicated_ch_ind == "Yes") | (ch_fam_beds > 0 & is_dedicated_ch_fam == "Yes"))) |
          (project_type == "RRH" & (all_ind_beds > 0 | all_fam_beds > 0)) |
          (project_type == "TH+RRH" & (all_fam_beds > 0 | all_ind_beds > 0)) |
          (project_type == "HMIS Project") |
          (project_type == "SSO-CE")
      )]
      
      dt[, is_dv_eligible := grepl("New|Expand", funding_action) & is_dedicated_dv == "Yes" & coc_funding_recommendation >= 50000 & (
        (project_type == "RRH" & (dv_ind_beds > 0 | dv_fam_beds > 0)) |
          (project_type == "TH+RRH" & (dv_ind_beds > 0 | dv_fam_beds > 0)) |
          (project_type == "SSO-CE")
      )]
      
      dt[, bonus_eligibility := fcase(
        is_coc_eligible & is_dv_eligible, "DV and CoC",
        is_coc_eligible, "CoC Bonus",
        is_dv_eligible, "DV Bonus",
        default = ""
      )]
      
      # 2. Cumulative Funding & Tier Straddling
      dt[, cum_funding := cumsum(coc_funding_recommendation)]
      dt[, prev_cum := cum_funding - coc_funding_recommendation]
      
      dt[, tier := fcase(
        prev_cum < coc_ard_data()$tier_1, "Tier 1",
        prev_cum < (coc_ard_data()$tier_1 + coc_ard_data()$tier_2), "Tier 2",
        default = "Projects Exceeding ARD"
      )]
      
      # 3. Bonus Selection (Top-Down Availability)
      dt[, coc_selected := FALSE]
      if (any(dt$is_coc_eligible)) {
        dt[is_coc_eligible == TRUE, coc_cum := cumsum(coc_funding_recommendation)]
        dt[is_coc_eligible == TRUE & (coc_cum - coc_funding_recommendation) < coc_ard_data()$coc_bonus, coc_selected := TRUE]
      }
      
      dt[, dv_selected := FALSE]
      if (any(dt$is_dv_eligible)) {
        # Only draw from DV bucket if NOT already selected for CoC
        dt[is_dv_eligible == TRUE & coc_selected == FALSE, dv_cum := cumsum(coc_funding_recommendation)]
        dt[is_dv_eligible == TRUE & coc_selected == FALSE & (dv_cum - coc_funding_recommendation) < coc_ard_data()$dv_bonus, dv_selected := TRUE]
      }
      
      # Determine row highlight colors
      dt[, highlight := fcase(
        coc_selected, "coc",
        dv_selected, "dv",
        default = "none"
      )]
      
      return(dt)
    }
    
    # Process Initial Data on Load or Reset
    process_data <- function(force_reset = FALSE) {
      req(user_coc$coc_version_id)
      raw_data <- get_projects_to_rank(user_coc$coc_version_id)
      
      #TO DO: Remove the random generation
      # Ensure integer format and handle default values safely
      raw_data[, coc_funding_requested := as.integer(fcoalesce(coc_funding_requested, as.numeric(sample(10000:1000000, fnrow(raw_data)))))]
      raw_data[, coc_funding_recommendation := as.integer(fcoalesce(as.numeric(coc_funding_recommendation), as.numeric(coc_funding_requested)))]
      raw_data[, ` ` := as.character(icon("grip-vertical"))]
      
      raw_data <- raw_data |>
        colorder(` `)
      
      # Partition data into the 4 tables
      rv$yhdp_ren <- raw_data[mckinneyventoyhdp & funding_action == "Renew"]
      rv$yhdp_oth <- raw_data[mckinneyventoyhdp & funding_action %in% c("Replace", "Reallocate", "Expand")]
      rv$excluded <- raw_data[!mckinneyventoyhdp & funding_action %in% c("Reallocate", "Ineligible", "NOT RATED")]
      
      # Get valid ranked projects
      ranked_data <- raw_data[!mckinneyventoyhdp & !funding_action %in% c("Reallocate", "Ineligible", "NOT RATED")]
      
      # If resetting OR if projects haven't been ranked yet (rank is all NA), apply the default sort
      if (force_reset || all(is.na(ranked_data$rank))) {
        ranked_data[, sort_type := fcase(project_type %in% c("SSO-CE", "HMIS Project", "SSO"), 1, default = 2)]
        ranked_data[, sort_priority := fcase(priority == "High", 1, priority == "Medium", 2, priority == "Low", 3, default = 4)]
        
        ranked_data <- ranked_data[order(sort_type, sort_priority, -weighted_score)]
      } else {
        # Otherwise, respect the saved rank order
        ranked_data <- ranked_data[order(rank)]
      }
      
      # Apply tiers and calculations
      rv$ranked <- recalculate_ranking(ranked_data)
    }
    
    format_ranked_tbl <-function(dt) {
      cols_to_remove <- c(
        "sort_type",
        "sort_priority",
        "is_coc_eligible",
        "is_dv_eligible",
        "bonus_eligibility",
        "cum_funding",
        "prev_cum",
        "coc_selected",
        "dv_selected",
        "beds",
        "funding",
        "fp_pop_grp",
        "no_pop_grp",
        "dv_renewal",
        "mckinneyventoyhdp"
      )
      
      dt |>
        fselect(setdiff(names(dt), cols_to_remove)) |>
        frename(
          "100% Dedicated + or CH Fam" = is_dedicated_ch_fam,
          "100% Dedicated + or CH Ind" = is_dedicated_ch_ind,
          "100% DV" = is_dedicated_dv
        )
    }
    render_projects_dt <- function(final) {
      colnames <- names(final)
      
      disabled_cols <- setdiff(
        0:(ncol(final) - 1), 
        which(colnames == "coc_funding_recommendation") - 1
      )
      
      # Get all column indices except coc_funding_recommendation
      x <- datatable(
        final,
        rownames = FALSE,
        colnames = gsub("_", " ", colnames),
        escape = FALSE,
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
            dataSrc = which(colnames == "tier") - 1,  # Group by TIER column (0-indexed)
            startRender = JS("
              function(rows, group) {
                // 1. Define a default color
                var bgColor = '#6c757d'; // default gray
                
                // 2. Check the group name and assign specific colors
                // Using .includes() makes it safer in case the exact string changes slightly
                if (group.includes('Tier 1')) {
                  bgColor = 'blue';
                } else if (group.includes('Tier 2')) {
                  bgColor = 'orange';
                } else if (group.includes('Exceeding')) {
                  bgColor = 'black';
                }
                
                // 3. Return the styled row
                return $('<tr class=\"group\"/>').append(
                  '<td colspan=\"' + rows.columns().header().length + '\" ' + 
                  'style=\"background-color: ' + bgColor + '; color: white; font-weight: bold; font-size: 16px;\">' + 
                  group + '</td>'
                );
              }"
            )
          ),
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          info = FALSE,
          columnDefs = list(
            list(targets = 0, className = 'drag-handle', width = "30px"),
            list(targets = which(colnames %in% c("project_id","tier","highlight")) - 1, visible = FALSE)
          )
        ),
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
          'coc_funding_recommendation',  # Replace with your actual column name
          backgroundColor = USER_ENTRY_BG_COLOR,
          fontWeight = 'bold'
        ) |>
        formatCurrency(
          c('coc_funding_requested', 'coc_funding_recommendation'),
          currency = "$",
          digits = 0
        ) |>
        formatStyle(
          columns = c("project_name","organization_name"), # Apply to all columns
          whiteSpace = 'nowrap',
          overflow = 'hidden',
          textOverflow = 'ellipsis',
          maxWidth = '150px'        # REQUIRED: Set a max width so truncation triggers
        )
      
      if("highlight" %in% colnames)
        x <- x |>
          formatStyle(
            'highlight',  # Replace with your actual column name
            target = 'row',
            backgroundColor = styleEqual(c("dv", "coc"), c('brown', 'pink'))
          )
      
      x
    }
    
    observeEvent(input$newOrder, {
      req(rv$ranked)
      req(input$newOrder)

      # Reorder data by new rank and re-rank
      new_data <- rv$ranked |>
        fmutate(new_rank = input$newOrder) |>
        roworder(new_rank) |>
        fmutate(
          rank = seq_along(project_id),
          new_rank = NULL
        )
      
      # Update reactive value
      rv$ranked <- new_data
    }, ignoreInit = TRUE)
    
    output$ui_ranked_list <- renderDT({
      req(fnrow(rv$ranked) > 0)
      render_projects_dt(format_ranked_tbl(rv$ranked))
    }, server=FALSE)
    
    ranked_proxy <- dataTableProxy(ns("ui_ranked_list"),session = session)
    
    observe({
      req(rv$ranked)
      replaceData(ranked_proxy, format_ranked_tbl(rv$ranked), resetPaging = FALSE)
    })
    
    
    observeEvent(input$ui_ranked_list_cell_edit, {
      info <- input$ui_ranked_list_cell_edit
      new_val <- as.integer(gsub("[^0-9.]", "", info$value))
      
      if (!is.na(new_val)) {
        # Modify via a copy to trigger reactive invalidation so proxy pushes the new value visually
        tmp <- copy(rv$ranked)
        tmp[info$row, coc_funding_recommendation := new_val]
        rv$ranked <- tmp
      }
    })
      
    render_minor_dt <- function(dt) {
      if (is.null(dt) || nrow(dt) == 0) return(NULL)
      
      display_df <- dt[, .(priority, weighted_score, met_hud_thresholds, met_coc_thresholds, funding_action, project_type, target_population, organization_name, project_name, coc_funding_requested, coc_funding_recommendation)]
      frename(display_df, met_hud_thresholds = "Met HUD Thresholds", met_coc_thresholds = "Met CoC Thresholds")
      
      datatable(
        display_df, 
        rownames = FALSE, 
        options = list(dom = 't', paging = FALSE, scrollY = NULL)
      ) |> formatCurrency(c('coc_funding_requested', 'coc_funding_recommendation'), currency = "$", digits = 0)
    }
    output$ui_yhdp_ren_list <- renderDT({ render_minor_dt(rv$yhdp_ren) })
    output$ui_yhdp_oth_list <- renderDT({ render_minor_dt(rv$yhdp_oth) })
    output$ui_excluded_list <- renderDT({ render_minor_dt(rv$excluded) })
    
    
    observeEvent(input$btn_adjust_tiers, {
      req(rv$ranked)
      rv$ranked <- recalculate_ranking(copy(rv$ranked))
      showNotification("Tiers and Bonuses recalculated successfully.", type = "message")
    })
    
    observeEvent(input$btn_save_ranking, {
      req(user_coc$coc_version_id)
      all_rankings <- rbindlist(list(
        rv$ranked[, .(project_id, rank, tier, coc_funding_recommendation)],
        rv$yhdp_ren[, .(project_id, rank = NA_integer_, tier = "YHDP", coc_funding_recommendation)],
        rv$yhdp_oth[, .(project_id, rank = NA_integer_, tier = "YHDP", coc_funding_recommendation)],
        rv$excluded[, .(project_id, rank = NA_integer_, tier = "Excluded", coc_funding_recommendation)]
      ), fill = TRUE)
      
      all_rankings[, coc_version_id := user_coc$coc_version_id]
      all_rankings[, created_by := user_coc$username]
      
      update_ranking_db(get_db_pool(), all_rankings)
    })
  })
}