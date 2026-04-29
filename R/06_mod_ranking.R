mod_ranking_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Ranking",
    value = "ranking",
    
    div(
      class = "d-flex justify-content-between align-items-end mb-3",
      
      # Left Side: Buttons (Normal size, placed next to each other)
      layout_columns(
        fill = FALSE,
        col_widths = c(6, 6),
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
      )
    ),
    
    # 1. Top Summary Widgets
    layout_columns(
      fill = FALSE,
      mod_ranking_widget_ui(ns("coc_bonus")) |> shinycssloaders::withSpinner(),
      mod_ranking_widget_ui(ns("tier_1")) |> shinycssloaders::withSpinner(),
      mod_ranking_widget_ui(ns("tier_2")) |> shinycssloaders::withSpinner(),
      mod_ranking_widget_ui(ns("dv_bonus")) |> shinycssloaders::withSpinner(),
      mod_ranking_widget_ui(ns("exceeds")) |> shinycssloaders::withSpinner()
    ),
    
    bslib::accordion(
      id = ns("funding_analysis_accordion"),
      open = FALSE,
      bslib::accordion_panel(
        "Funding Analysis Table",
        icon = icon("table"),
        uiOutput(ns("ui_funding_analysis"))
      )
    ),
    
    shinyWidgets::virtualSelectInput(
      inputId = ns("hidden_cols"),
      label = "Hidden Columns:",
      choices = NULL, # Populated dynamically in server
      multiple = TRUE,
      width = "200px",
      showValueAsTags = TRUE,
      placeholder = "No columns hidden"
    ),
    
    # 4. Drag and Drop Zones
    DTOutput(ns("ui_ranked_list")) |> shinycssloaders::withSpinner(),
    br(),
    br(),
    h5(id = ns("excluded_tbl_title"), "Projects Not Selected for Funding"),
    DTOutput(ns("ui_excluded_list")) |> shinycssloaders::withSpinner() #,
    # br(),
    # br(),
    # DTOutput(ns("ui_yhdp_ren")),
    # br(),
    # br(),
    # DTOutput(ns("ui_yhdp_oth"))
  )
}

mod_ranking_server <- function(id, nav_control, user_coc, parent_session, help_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    unspec_types <- c("HMIS", "HMIS Project", "OPH", "SSO", "SSO-CE", "SSO-Host Homes", "SH")
    
    priority_levels <- get_labelled_lookups("priority")
    
    unspecified_id <- get_lookup_refid("Unspecified", "priority")
    
    coc_ard_data <- reactive({
      req(user_coc$coc)
      get_coc_hud_ard_data(user_coc)
    })
    
    # Reactive values to store the data and bucket limits
    rv <- reactiveValues(
      # limits = list(tier1 = 0, tier2 = 0, dv = 0),
      # RankedProjects = NULL,
      ranked = NULL,
      # yhdp_ren = NULL,
      # yhdp_oth = NULL,
      excluded = NULL
    )
    
    ceilings_priorities <- reactive({
      get_ceilings_priorities(user_coc$coc_version_id)
    })
    
    observeEvent(ceilings_priorities(), {
      cp <- ceilings_priorities()
      shinyjs::toggle("funding_analysis_accordion", condition = !allNA(cp$priority) || !allNA(cp$ceil_beds) || !allNA(cp$ceil_fund))
    })
    
    target_pop_combos <-  expand.grid(
      target_population = LOOKUPS[reference_type == "target_population"]$value,
      population_group = LOOKUPS[reference_type == "population_group"]$value
    ) |>
      fsubset(!target_population %in% c("NA", "HIV")) |>
      fmutate(
        target_pop_combo = paste0(target_population, "_", population_group),
        bed_prefix = fcase(
          target_pop_combo == "General_Individual", "all_ind",
          target_pop_combo == "DV_Individual", "dv_ind",
          target_pop_combo == "CH_Individual", "ch_ind",
          target_pop_combo == "Veteran_Individual", "vet_ind",
          target_pop_combo == "Youth_Individual", "single_youth",
          target_pop_combo == "General_Family", "all_fam",
          target_pop_combo == "DV_Family", "dv_fam",
          target_pop_combo == "CH_Family", "ch_fam",
          target_pop_combo == "Veteran_Family", "vet_fam",
          target_pop_combo == "Youth_Family", "par_youth"
        )
      )
    
    # Helper for the Funding Analysis Matrix
    get_funding_analysis_data <- function(dt) {
      combos <- c("all_fam" = "All Families", "dv_fam" = "DV Families", 
                  "ch_fam" = "Chronically Homeless Families", "vet_fam" = "Veteran Families", 
                  "par_youth" = "Parenting Youth", "all_ind" = "All Individuals", 
                  "dv_ind" = "DV Individuals", "ch_ind" = "Chronically Homeless Individuals", 
                  "vet_ind" = "Veteran Individuals", "single_youth" = "Single Youth")
      
      bed_cols <- c(all_fam="all_fam_beds", dv_fam="dv_fam_beds", ch_fam="ch_fam_beds", vet_fam="vet_fam_beds", 
                    par_youth="par_youth_beds", all_ind="all_ind_beds", dv_ind="dv_ind_beds", 
                    ch_ind="total_ch_ind_beds", vet_ind="vet_ind_beds", single_youth="single_youth_beds")
      
      p_types <- c("PSH", "RRH", "TH", "TH+RRH")
      
      # For matrix, we report beds & funding specifically for valid Tier 1 and Tier 2 projects 
      dt_sub <- dt[tier %in% c("Tier 1", "Tier 2") & is_over_target == FALSE]
      
      res <- data.table(Population = unname(combos))
      
      for (pt in p_types) {
        alloc_col <- character(length(combos))
        pct_col <- character(length(combos))
        
        for (i in seq_along(combos)) {
          cb <- names(combos)[i]
          
          alloc_b <- 0
          alloc_f <- 0
          
          dt_pt <- dt_sub[project_type == pt]
          dt_all_pt <- dt[project_type == pt] # Use un-subset dt to safely locate ceiling maxes 
          
          ceil_b <- NA_real_
          ceil_f <- NA_real_
          
          # Grab applicable Ceilings 
          if (nrow(dt_all_pt) > 0) {
            c_b_vals <- dt_all_pt[[paste0("ceil_beds_", cb)]]
            c_f_vals <- dt_all_pt[[paste0("ceil_fund_", cb)]]
            if (!all(is.na(c_b_vals))) ceil_b <- max(c_b_vals, na.rm = TRUE)
            if (!all(is.na(c_f_vals))) ceil_f <- max(c_f_vals, na.rm = TRUE)
          }
          
          if (nrow(dt_pt) > 0) {
            for (r in seq_len(nrow(dt_pt))) {
              p_beds <- DT::coerceValue(dt_pt[[ bed_cols[[cb]] ]][r], 0L)
              p_fund <- DT::coerceValue(dt_pt$coc_funding_recommendation[r], 0L)
              t_beds <- DT::coerceValue(dt_pt$total_beds[r], 0L)
              
              r_beds <- sapply(names(bed_cols), function(x) DT::coerceValue(dt_pt[[ bed_cols[[x]] ]][r], 0L))
              
              has_dv <- (r_beds[["dv_fam"]] > 0) || (r_beds[["dv_ind"]] > 0)
              subpops <- c("ch_fam", "vet_fam", "par_youth", "ch_ind", "vet_ind", "single_youth")
              has_subpop <- (has_dv || sum(unlist(r_beds[subpops])) > 0)
              
              primary_combos <- character(0)
              
              # Distribute logic (Mirrors recalculate_ranking exclusion checks)
              if (!has_subpop) {
                if (r_beds[["all_fam"]] > 0) primary_combos <- c(primary_combos, "all_fam")
                if (r_beds[["all_ind"]] > 0) primary_combos <- c(primary_combos, "all_ind")
                if (length(primary_combos) == 0) primary_combos <- c("all_fam", "all_ind")
              } else if (has_dv) {
                if (r_beds[["dv_fam"]] > 0) primary_combos <- c(primary_combos, "dv_fam")
                if (r_beds[["dv_ind"]] > 0) primary_combos <- c(primary_combos, "dv_ind")
              } else {
                for (scb in subpops) {
                  if (t_beds > 0 && r_beds[[scb]] >= 0.5 * t_beds) {
                    primary_combos <- c(primary_combos, scb)
                  }
                }
              }
              
              if (p_beds > 0 || (sum(unlist(r_beds)) == 0 && cb %in% primary_combos)) {
                alloc_b <- alloc_b + p_beds
                alloc_f <- alloc_f + p_fund
              }
            }
          }
          
          alloc_col[i] <- paste0(alloc_b, " Beds<br>", scales::dollar(alloc_f, accuracy=1))
          
          pct_b <- if (is.na(ceil_b) || ceil_b == 0) "-" else scales::percent(alloc_b / ceil_b, accuracy=0.1)
          pct_f <- if (is.na(ceil_f) || ceil_f == 0) "-" else scales::percent(alloc_f / ceil_f, accuracy=0.1)
          
          pct_col[i] <- if (pct_b == "-" && pct_f == "-") "-" else paste0(pct_b, "<br>", pct_f)
        }
        
        res[[paste0(pt, "_Allocated")]] <- alloc_col
        res[[paste0(pt, "_PctCeil")]] <- pct_col
      }
      return(res)
    }
    
    output$ui_funding_analysis <- renderUI({
      req(rv$ranked)
      dt <- get_funding_analysis_data(rv$ranked)
      
      sketch <- htmltools::withTags(table(
        class = "display",
        thead(
          tr(
            th(rowspan = 2, style = "vertical-align: middle;", ""),
            th(colspan = 2, style = "text-align:center; background-color:black; color:white; border: 1px solid white;", "PSH"),
            th(colspan = 2, style = "text-align:center; background-color:black; color:white; border: 1px solid white;", "RRH"),
            th(colspan = 2, style = "text-align:center; background-color:black; color:white; border: 1px solid white;", "TH"),
            th(colspan = 2, style = "text-align:center; background-color:black; color:white; border: 1px solid white;", "TH+RRH")
          ),
          tr(
            th(style = "background-color:black; color:white; text-align:center; border: 1px solid #444;", "Allocated"), 
            th(style = "background-color:black; color:white; text-align:center; border: 1px solid #444;", "% of Ceiling"),
            th(style = "background-color:black; color:white; text-align:center; border: 1px solid #444;", "Allocated"), 
            th(style = "background-color:black; color:white; text-align:center; border: 1px solid #444;", "% of Ceiling"),
            th(style = "background-color:black; color:white; text-align:center; border: 1px solid #444;", "Allocated"), 
            th(style = "background-color:black; color:white; text-align:center; border: 1px solid #444;", "% of Ceiling"),
            th(style = "background-color:black; color:white; text-align:center; border: 1px solid #444;", "Allocated"), 
            th(style = "background-color:black; color:white; text-align:center; border: 1px solid #444;", "% of Ceiling")
          )
        )
      ))
      
      DT::datatable(
        dt,
        escape = FALSE,
        rownames = FALSE,
        selection = "none",
        container = sketch,
        options = list(
          dom = 't',
          ordering = FALSE,
          paging = FALSE,
          columnDefs = list(
            list(className = 'dt-center', targets = 1:8),
            list(className = 'dt-right', targets = 0)
          )
        )
      ) |>
        DT::formatStyle(
          columns = 1:9,
          borderRight = '1px solid #ccc'
        ) |>
        # Add visual highlighting similar to the uploaded image for cells > 0
        DT::formatStyle(
          columns = c(2,4,6,8), # The 'Allocated' columns
          backgroundColor = DT::styleRow(1:10, rep(c('#a8c4e5', 'white'), 5)) # Example light blue striping to visualize rows better
        )
    })
    
    get_allocated_funding <- function(condition) {
      req(fnrow(rv$ranked) > 0)
      sum(rv$ranked[eval(condition)]$coc_funding_recommendation, na.rm = TRUE)
    }
    alloc_tier1 <- reactive( get_allocated_funding(quote(tier == "Tier 1")) )
    alloc_tier2 <- reactive( get_allocated_funding(quote(tier == "Tier 2")) )
    alloc_coc <- reactive( get_allocated_funding(quote(coc_selected == TRUE)) )
    alloc_exceed <- reactive( get_allocated_funding(quote(tier == "Projects Exceeding ARD Adj")) )
    
    alloc_dv <- reactive({
      req(fnrow(rv$ranked) > 0)
      list(
        total = sum(rv$ranked[dv_selected == TRUE]$coc_funding_recommendation, na.rm = TRUE),
        t1 = sum(rv$ranked[dv_selected == TRUE & tier == "Tier 1"]$coc_funding_recommendation, na.rm = TRUE),
        t2 = sum(rv$ranked[dv_selected == TRUE & tier == "Tier 2"]$coc_funding_recommendation, na.rm = TRUE)
      )
    })
    
    mod_ranking_widget_server("coc_bonus", alloc_coc, coc_ard_data, "CoC Bonus", icon_name = "plus-circle")
    mod_ranking_widget_server("tier_1", alloc_tier1, coc_ard_data, "Tier 1 (Adj ARD * 90%)", icon_name = "layer-group")
    mod_ranking_widget_server("tier_2", alloc_tier2, coc_ard_data, "Tier 2 (Adj ARD * 10% + CoC Bonus + DV Bonus)", icon_name = "layer-group")
    mod_ranking_widget_server("dv_bonus", alloc_dv, coc_ard_data, "DV Bonus", icon_name = "heart")
    mod_ranking_widget_server("exceeds", alloc_exceed, coc_ard_data, "Exceeding ARD Adj", icon_name = "exclamation-triangle")
    
    # ranked_project_ids <- reactiveValues(new = NULL)
    
    observeEvent(c(user_coc$coc_version_id, user_coc$projects_updated), { 
      process_data(force_reset = FALSE) 
    }, ignoreInit = TRUE)
    
    # Core Function: Recalculate Tiers and Bonuses
    check_over_targets <- function(dt) {
      # Map project types to integer indexes (1 through N)
      unique_ptypes <- fct_unique(dt$project_type)
      
      # Pre-allocate tracking matrices (Rows = Project Type, Cols = Populations)
      # Matrix indexing in R is nearly instantaneous compared to environment lookups
      tracker_beds <- matrix(
        0, 
        nrow = length(unique_ptypes), 
        ncol = fnrow(target_pop_combos), 
        dimnames = list("project_type" = unique_ptypes, "target_pop_combo" = target_pop_combos$target_pop_combo)
      )
      tracker_fund <- copy(tracker_beds)
      
      dt[, is_over_target := FALSE]
      
      for (i in seq_row(dt)) {
        pt_i <- dt$project_type[i]
        p_fund <- fcoalesce(dt$coc_funding_recommendation[i], DT::coerceValue(0, dt$coc_funding_recommendation))
        t_beds <- fcoalesce(dt$total_beds[i], DT::coerceValue(0, dt$total_beds))
        
        # Extract current project beds into a fast, named numeric vector
        p_beds <- c(
          all_fam      = fcoalesce(dt$all_fam_beds[i], 0L),
          dv_fam       = fcoalesce(dt$dv_fam_beds[i], 0L),
          ch_fam       = fcoalesce(dt$ch_fam_beds[i], 0L),
          vet_fam      = fcoalesce(dt$vet_fam_beds[i], 0L),
          par_youth    = fcoalesce(dt$par_youth_beds[i], 0L),
          all_ind      = fcoalesce(dt$all_ind_beds[i], 0L),
          dv_ind       = fcoalesce(dt$dv_ind_beds[i], 0L),
          ch_ind       = fcoalesce(dt$total_ch_ind_beds[i], 0L),
          vet_ind      = fcoalesce(dt$vet_ind_beds[i], 0L),
          single_youth = fcoalesce(dt$single_youth_beds[i], 0L)
        )
        
        has_dv <- p_beds["dv_fam"] > 0 || p_beds["dv_ind"] > 0
        subpops <- c("ch_fam", "vet_fam", "par_youth", "ch_ind", "vet_ind", "single_youth")
        has_subpop <- has_dv || sum(p_beds[subpops]) > 0
        
        subpops_50 <- subpops[p_beds[subpops] >= 0.5 * t_beds & t_beds > 0]
        relevant_subpops <- subpops[p_beds[subpops] > 0]
        
        check_mode <- ""
        primary_combos <- character(0)
        
        if (!has_subpop) {
          check_mode <- "only_all"
          if (p_beds["all_fam"] > 0) primary_combos <- c(primary_combos, "all_fam")
          if (p_beds["all_ind"] > 0) primary_combos <- c(primary_combos, "all_ind")
          if (length(primary_combos) == 0) primary_combos <- c("all_fam", "all_ind")
        } else if (has_dv) {
          check_mode <- "dv"
          if (p_beds["dv_fam"] > 0) primary_combos <- c(primary_combos, "dv_fam")
          if (p_beds["dv_ind"] > 0) primary_combos <- c(primary_combos, "dv_ind")
        } else if (length(subpops_50) > 0) {
          check_mode <- "subpop_50"
          primary_combos <- subpops_50
        } else {
          check_mode <- "all_relevant"
        }
        
        check_exceeds <- function(cb) {
          cb_i <- whichv(target_pop_combos$bed_prefix, cb) # Matrix Col Index
          
          c_beds <- dt[[paste0("ceil_beds_", cb)]][i]
          c_fund <- dt[[paste0("ceil_fund_", cb)]][i]
          
          u_beds <- tracker_beds[pt_i, cb_i]
          u_fund <- tracker_fund[pt_i, cb_i]
          
          exceeds_beds <- !is.na(c_beds) && ((u_beds + p_beds[cb]) > c_beds)
          exceeds_fund <- !is.na(c_fund) && ((u_fund + p_fund) > c_fund)
          
          return(isTruthy(exceeds_beds || exceeds_fund))
        }
        
        is_excluded <- FALSE
        
        if (check_mode %in% c("only_all", "dv", "subpop_50")) {
          for (cb in primary_combos) {
            if (check_exceeds(cb)) { is_excluded <- TRUE; break }
          }
        } else if (check_mode == "all_relevant" && length(relevant_subpops) > 0) {
          all_exceeded <- TRUE
          for (cb in relevant_subpops) {
            if (!check_exceeds(cb)) { all_exceeded <- FALSE; break }
          }
          if (all_exceeded) is_excluded <- TRUE
        }
        
        if (is_excluded) {
          dt$is_over_target[i] <- TRUE
        } else {
          # If project is kept, update our tracking matrices instantly
          for (cb in target_pop_combos$bed_prefix) {
            if (p_beds[cb] > 0 || (sum(p_beds) == 0 && cb %in% primary_combos)) {
              cb_i <- whichv(target_pop_combos$bed_prefix, cb)
              tracker_beds[pt_i, cb_i] <- tracker_beds[pt_i, cb_i] + p_beds[cb]
              tracker_fund[pt_i, cb_i] <- tracker_fund[pt_i, cb_i] + p_fund
            }
          }
        }
      }
      return(dt)
    }
    
    recalculate_ranking <- function(dt) {
      if(is.null(dt) || nrow(dt) == 0) return(dt)
      
      dt <- if(!allNA(ceilings_priorities()$ceil_beds) && !allNA(ceilings_priorities()$ceil_fund))
        check_over_targets(dt)
      else
        dt |> fmutate(is_over_target = FALSE)
      
      # Rank only non-excluded rows correctly by sequence
      dt[is_over_target == FALSE, rank := seq_len(.N)]
      
      # 2. Cumulative Funding & Tier Straddling only on valid projects
      dt[is_over_target == FALSE, cum_funding := cumsum(coc_funding_recommendation)]
      dt[is_over_target == FALSE, prev_cum := cum_funding - coc_funding_recommendation]
      
      dt[is_over_target == FALSE, tier := fcase(
        prev_cum < coc_ard_data()$tier_1, "Tier 1", # this allows straddles
        cum_funding <= (coc_ard_data()$tier_1 + coc_ard_data()$tier_2), "Tier 2", # this doesn't allow straddles
        default = "Projects Exceeding ARD Adj"
      )]
      
      dt[is_over_target == FALSE, straddle_amount := fifelse(
        prev_cum < coc_ard_data()$tier_1 & cum_funding > coc_ard_data()$tier_1, 
        cum_funding - coc_ard_data()$tier_1,
        0
      )]
      
      dt[is_over_target == FALSE, coc_selected := FALSE]
      if (any(dt$is_coc_eligible, na.rm=TRUE)) {
        dt[is_coc_eligible == TRUE, coc_cum := cumsum(coc_funding_recommendation)]
        dt[is_coc_eligible == TRUE & (coc_cum - coc_funding_recommendation) < coc_ard_data()$coc_bonus, coc_selected := TRUE]
      }
      
      dt[is_over_target == FALSE, dv_selected := FALSE]
      if (any(dt$is_dv_eligible, na.rm=TRUE)) {
        dt[is_dv_eligible == TRUE & coc_selected == FALSE, dv_cum := cumsum(coc_funding_recommendation)]
        dt[is_dv_eligible == TRUE & coc_selected == FALSE & (dv_cum - coc_funding_recommendation) < coc_ard_data()$dv_bonus, dv_selected := TRUE]
      }
      
      dt[is_over_target == FALSE, highlight := fcase(coc_selected, "coc", dv_selected, "dv", default = "none")]
      
      return(dt)
    } #end recalc ranking
    
    calculate_priority <- function(dt) {
      # 2. Fetch Priorities and convert lookup IDs to standard string values
      
      if (!allNA(ceilings_priorities()$priority)) {
        # Pivot Wide for Priorities
        wide_prio <- ceilings_priorities() |>
          pivot(
            ids = "project_type",
            values = "priority",
            how = "wider",
            names = c("target_population", "population_group"),
            # na.rm=TRUE,
            # FUN = "max",
            transpose = TRUE,
            fill = unspecified_id
          ) %>%
          setnames(colnames(.)[-1], paste0("prio_", colnames(.)[-1])) |>
          replace_NA(unspecified_id)
  
        # Pivot Wide for Bed Ceilings
        wide_beds <- ceilings_priorities() |>
          pivot(
            ids = "project_type",
            values = "ceil_beds",
            how = "wider",
            names = c("target_population", "population_group"),
            transpose = TRUE
          ) %>%
          setnames(colnames(.)[-1], paste0("ceil_beds_", colnames(.)[-1]))

        # Pivot Wide for Funding Ceilings
        wide_fund <- ceilings_priorities() |>
          pivot(
            ids = "project_type",
            values = "ceil_fund",
            how = "wider",
            names = c("target_population", "population_group"),
            transpose = TRUE
          ) %>%
          setnames(colnames(.)[-1], paste0("ceil_fund_", colnames(.)[-1]))
        
        dt <- dt |>
          join(wide_prio, on = "project_type") |>
          join(wide_beds, on = "project_type") |>
          join(wide_fund, on = "project_type")
      } else {
        return(
          dt |> 
            fmutate(
              priority = factor(unspecified_id, levels = priority_levels, labels = names(priority_levels))
            )
        )
      }
      
      # Ensure target columns exist safely
      dt[, total_beds := all_fam_beds + all_ind_beds]
      
      # Assign Priority Based on Bed Distribution
      dt[, priority := if(allNA(ceilings_priorities()$priority)) unspecified_id else 
        fcase(
        project_type %in% unspec_types, unspecified_id,
        
        # If they have any DV beds, use DV priority
        dv_fam_beds > 0 | dv_ind_beds > 0, pmax(prio_DV_Family, prio_DV_Individual, na.rm = TRUE),
        
        # If any of their sub-pop beds are at least 50% of total beds, use highest priority of those sub-pop
        (ch_fam_beds >= 0.5 * total_beds & total_beds > 0) | (vet_fam_beds >= 0.5 * total_beds & total_beds > 0) |
          (par_youth_beds >= 0.5 * total_beds & total_beds > 0) | (total_ch_ind_beds >= 0.5 * total_beds & total_beds > 0) |
          (vet_ind_beds >= 0.5 * total_beds & total_beds > 0) | (single_youth_beds >= 0.5 * total_beds & total_beds > 0),
        pmax(
          fifelse(ch_fam_beds >= 0.5 * total_beds & total_beds > 0, prio_CH_Family, unspecified_id),
          fifelse(vet_fam_beds >= 0.5 * total_beds & total_beds > 0, prio_Veteran_Family, unspecified_id),
          fifelse(par_youth_beds >= 0.5 * total_beds & total_beds > 0, prio_Youth_Family, unspecified_id),
          fifelse(total_ch_ind_beds >= 0.5 * total_beds & total_beds > 0, prio_CH_Individual, unspecified_id),
          fifelse(vet_ind_beds >= 0.5 * total_beds & total_beds > 0, prio_Veteran_Individual, unspecified_id),
          fifelse(single_youth_beds >= 0.5 * total_beds & total_beds > 0, prio_Youth_Individual, unspecified_id), na.rm = TRUE),
        
        #If sum of their sub-pop beds is at least 50%, use "Unspecified"
        (ch_fam_beds + total_ch_ind_beds + vet_fam_beds + vet_ind_beds + par_youth_beds + single_youth_beds) >= 0.5 * total_beds & total_beds > 0, unspecified_id,
        
        # If sum of their sub-pop beds is less than 50%, use general pop priority
        all_fam_beds > all_ind_beds, prio_General_Family,
        all_ind_beds > all_fam_beds, prio_General_Individual,
        all_fam_beds == all_ind_beds & total_beds > 0, pmax(prio_General_Family, prio_General_Individual, na.rm = TRUE),
        default = unspecified_id
      )] %>%
        fmutate(priority = convert_to_factor(., "priority"))
      
      cols_to_drop <- paste0("prio_", target_pop_combos$target_pop_combo)
      dt[, (cols_to_drop) := NULL]
      
      dt <- dt |> 
        fmutate(
          priority = factor(priority, levels = priority_levels, labels = names(priority_levels))
        )
      
      return(dt)
    } # end calculate_priority
    
    # Process Initial Data on Load or Reset
    process_data <- function(force_reset = FALSE) {
      req(user_coc$coc_version_id)
      
      raw_data <- get_projects_to_rank(user_coc$coc_version_id) |>
        calculate_priority()
      
      if(IN_DEV_MODE) {
        raw_data[, coc_funding_requested := fcoalesce(coc_funding_requested, coerceValue(sample(10000:1000000, .N), coc_funding_requested))]
        raw_data[, coc_funding_recommendation := fcoalesce(coc_funding_recommendation, coerceValue(coc_funding_requested, coc_funding_recommendation))]
        raw_data[, weighted_score := fcoalesce(weighted_score, DT::coerceValue(sample(100, .N), weighted_score))]
        raw_data[, met_hud_thresholds := fcoalesce(coerceValue(met_hud_thresholds, 0L), 1L)]
        raw_data[, met_coc_thresholds := fcoalesce(coerceValue(met_coc_thresholds, 0L), 1L)]
        # raw_data[, met_hud_thresholds := fcoalesce(coerceValue(met_hud_thresholds, 0L), sample(0:1, .N, replace=TRUE))]
        # raw_data[, met_coc_thresholds := fcoalesce(coerceValue(met_coc_thresholds, 0L), sample(0:1, .N, replace=TRUE))]
      }
      
      # add empty column to front for drag-and-drop control
      raw_data <- raw_data |>
        fmutate(` ` = as.character(icon("grip-vertical"))) |>
        colorder(` `) |>
        fmutate(
          # 1. Evaluate Bonus Eligibility only on valid projects
          is_coc_eligible = funding_action %in% c("New","Expand") & (
            (project_type == "PSH" & ((total_ch_ind_beds > 0 & is_dedicated_ch_ind == "Yes") | (ch_fam_beds > 0 & is_dedicated_ch_fam == "Yes"))) |
            (project_type %in% c("RRH", "TH+RRH") & (all_ind_beds > 0 | all_fam_beds > 0)) |
            project_type %in% c("HMIS Project", "SSO-CE")
          ),
          
          is_dv_eligible = funding_action %in% c("New","Expand") & 
            is_dedicated_dv == "Yes" & coc_funding_recommendation >= 50000 & (
              (project_type %in% c("RRH", "TH+RRH") & (dv_ind_beds > 0 | dv_fam_beds > 0)) |
              project_type == "SSO-CE"
            ),
          
          bonus_eligibility = fcase(
            !funding_action %in% c("New","Expand"), "N/A",
            is_coc_eligible & is_dv_eligible, "DV and CoC",
            is_coc_eligible, "CoC Bonus",
            is_dv_eligible, "DV Bonus",
            funding_action %in% c("New","Expand"), "New, Bonus-Ineligible"
          ),
          
          met_hud_thresholds = fcoalesce(met_hud_thresholds, 0L),
          met_coc_thresholds = fcoalesce(met_coc_thresholds, 0L),
          
          unmet_thresholds = met_hud_thresholds != 1 | met_coc_thresholds != 1,
          
          ineligible = funding_action %in% c("Reallocate", "Ineligible", "NOT RATED", "Ignore") |
            unmet_thresholds |
            bonus_eligibility == "New, Bonus-Ineligible"
        )
      
      # Partition data
      # rv$yhdp_ren <- raw_data[mckinneyventoyhdp & funding_action == "Renew"]
      # rv$yhdp_oth <- raw_data[mckinneyventoyhdp & funding_action %in% c("Replace", "Reallocate", "Expand")]
      rv$excluded <- raw_data |>
        fsubset(ineligible == TRUE) |>
        fmutate(rank = fcase(
          unmet_thresholds, "Unmet thresholds",
          bonus_eligibility == "New, Bonus-Ineligible",  "New, Bonus-Ineligible",
          default = "Ineligible"
        ))
      
      # Get valid ranked projects 
      ranked_data <- raw_data[ineligible == FALSE]
      
      # Step 1: Default Sorted Logic Pre-Ranking
      ranked_data <- if (force_reset || all(is.na(ranked_data$rank))) {
        # sort special project types to the top
        ranked_data |>
          fmutate(sort_project_type = project_type %in% c("SSO-CE", "HMIS Project", "SSO", "HMIS", "SSO-Host Homes")) |>
          roworder(-sort_project_type, -priority, -weighted_score)
      } else {
        ranked_data |> 
          roworder(rank)
      }
      
      # Apply Ceilings and subsequent Tiers
      ranked_data <- recalculate_ranking(ranked_data)
      
      # Flag Over-Target and merge back to excluded safely
      over_target <- ranked_data[is_over_target == TRUE]
      if (nrow(over_target) > 0) {
        over_target[, tier := "Excluded (Over Target)"]
        over_target[, rank := "Over Target"] # Converted intentionally to character to display in dt
        # Make sure rv$excluded has a character rank row so they combine
        if (!is.character(rv$excluded$rank)) rv$excluded[, rank := as.character(rank)]
        rv$excluded <- rbindlist(list(rv$excluded, over_target), fill = TRUE, use.names = TRUE)
      }
      
      rv$excluded < rv$excluded |>
        colorder(rank, priority, pos = "after")
      
      # Store clean ranked rows to UI
      rv$ranked <- ranked_data |>
        fsubset(is_over_target == FALSE) |>
        colorder(rank, priority, pos = "after") # move priority after rank
      
    } # end process_data
    
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
        "mckinneyventoyhdp",
        paste0("ceil_beds_", target_pop_combos$target_pop_combo),
        paste0("ceil_fund_", target_pop_combos$target_pop_combo),
        "total_beds",
        "bonus_type",
        "unmet_thresholds",
        "ineligible"
      )
      
      dt |>
        fselect(setdiff(names(dt), cols_to_remove)) |>
        frename(
          "100% Dedicated + or CH Fam" = is_dedicated_ch_fam,
          "100% Dedicated + or CH Ind" = is_dedicated_ch_ind,
          "100% DV" = is_dedicated_dv
        ) |>
        fmutate(
          met_hud_thresholds = factor_yesno(met_hud_thresholds),
          met_coc_thresholds = factor_yesno(met_coc_thresholds)
        )
    }
    
    structural_cols <- c("project_id", "tier", "highlight", "coc_cum", "sort_project_type", "is_over_target", "straddle_amount", " ")
    
    table_styles <- function(dt, type = "main") {
      dt <- dt |>
        formatStyle(
          'coc_funding_recommendation', 
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
      
      if(type == "main")
        dt <- dt |>
          formatStyle(
            columns = 'highlight',  # Replace with your actual column name
            target = 'row',
            backgroundColor = styleEqual(c("dv", "coc"), c(brandr::get_brand_color("dv_bonus"), brandr::get_brand_color("coc_bonus"))),
            color = styleEqual(c("coc", "dv"), c('white', 'white'))
          ) |>
          formatStyle(
            columns = 'straddle_amount',
            target = 'row',
            outline = styleInterval(
              cuts = 0,
              values = c('none', '2px solid var(--brand-tier_2)') # [<=0 style, >0 style]
            )
          )
      
      dt
    }
    render_projects_dt <- function(final, type = "main", show_beds = FALSE) {
      colnames <- names(final)
      
      disabled_cols <- setdiff(
        0:(ncol(final) - 1), 
        which(colnames == "coc_funding_recommendation") - 1
      )
      
      bed_cols <- grep("bed", colnames, ignore.case = TRUE) - 1
      
      columnDefs = list(
        list(targets = 0, className = 'drag-handle', width = "30px"),
        # hide structural fields and, initially, bed fields
        list(targets = which(colnames %in% structural_cols) - 1, visible = FALSE),
        list(targets = bed_cols, visible = FALSE)
      )
      straddle_col_idx <- which(colnames == "straddle_amount") - 1
      funding_col_idx  <- which(colnames == "coc_funding_recommendation") - 1
      
      
      
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
          scrollY = NULL,
          rowReorder = list(selector = 'td.drag-handle', update = FALSE), # Grab by icon
          fixedHeader = TRUE,
          rowGroup = if(type == "main") list(
            dataSrc = which(colnames == "tier") - 1,  # Group by TIER column (0-indexed)
            startRender = JS("
              function(rows, group) {
                // Map the group name to the CSS classes we added in custom.css
                var grpClass = '';
                if (group.includes('Tier 1')) { grpClass = 'group-tier_1'; } 
                else if (group.includes('Tier 2')) { grpClass = 'group-tier_2'; } 
                else if (group.includes('Exceeding')) { grpClass = 'group-exceeds'; }
                
                return $('<tr class=\"group_header ' + grpClass + '\"/>').append('<td colspan=\"100%\">' + group + '</td>');
              }"
            )
          ),
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          info = FALSE,
          columnDefs = columnDefs,
          # Inside your datatable options list:
          rowCallback = if(type == "main") JS(sprintf("
            function(row, data, displayNum, displayIndex, dataIndex) {
              var straddleIdx = %d;
              var fundingIdx = %d;
              debugger;
              var straddle = parseFloat(data[straddleIdx]);
              var funding = parseFloat(data[fundingIdx]);
              
              if (straddle > 0 && funding > 0) {
                var tier1 = funding - straddle;
                var pct = (tier1 / funding) * 100;
                
                var fmt = new Intl.NumberFormat('en-US', {style: 'currency', currency: 'USD', maximumFractionDigits: 0});
                
                var titleText = '⚖️ STRADDLING PROJECT\\n' +
                                'This project spans the Tier 1 / Tier 2 boundary.\\n\\n' +
                                '• Portion allocated to Tier 1: ' + fmt.format(tier1) + ' (' + pct.toFixed(1) + '%%)\\n' +
                                '• Portion allocated to Tier 2: ' + fmt.format(straddle) + ' (' + (100 - pct).toFixed(1) + '%%)';
                var titleTextCSS = titleText.replace(/\\n/g, '\\\\A ');

                // Apply class and the CSS Variable to the ROW
                $(row).addClass('straddle-row');
                
                var ghostLabel = '⬇️ ' + fmt.format(straddle) + ' spills over into Tier 2';
                row.style.setProperty('--t1-pct', pct + '%%');
                row.style.setProperty('--straddle-label', '\"' + titleTextCSS + '\"');
              }
            }", straddle_col_idx, funding_col_idx
          ))
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
        table_styles(type = type)
      
      x
    }
    
    
    
    # HANDLE COLUMN TOGGLE
    # user cannot show/hide these. Always hidden
    observe({
      req(rv$ranked)
      col_names <- names(format_ranked_tbl(rv$ranked))
      
      toggleable_cols <- setdiff(col_names, structural_cols)
      
      bed_cols <- grep("bed", toggleable_cols, ignore.case = TRUE, value = TRUE)
      general_cols <- setdiff(toggleable_cols, bed_cols)
      
      # Create a flat list of choices, swapping all bed fields for the single word "Beds"
      choices <- c("BED FIELDS", general_cols)
      
      shinyjs::show("hidden_cols")
      shinyWidgets::updateVirtualSelect(
        session = session,
        inputId = "hidden_cols",
        choices = setNames(choices, gsub("_", "", toupper(choices))),
        selected = "BED FIELDS" # Initially hide Beds
      )
    })
    
    # 2. Proxy observer: Fast client-side hiding/showing of columns
    observeEvent(input$hidden_cols, {
      req(rv$ranked)
      col_names <- names(format_ranked_tbl(rv$ranked))
      toggleable_cols <- setdiff(col_names, structural_cols)
      
      bed_cols <- grep("bed", toggleable_cols, ignore.case = TRUE, value = TRUE)
      general_cols <- setdiff(toggleable_cols, bed_cols)
      
      selected_hidden <- if(is.null(input$hidden_cols)) character(0) else input$hidden_cols
      
      # Build the final list of actual columns to hide
      cols_to_hide <- character(0)
      
      # Expand the "Beds" shortcut into the actual column names
      if ("BED FIELDS" %in% selected_hidden) {
        cols_to_hide <- c(cols_to_hide, bed_cols)
      }
      
      # Add any general columns the user selected
      cols_to_hide <- c(cols_to_hide, intersect(selected_hidden, general_cols))
      cols_to_show <- setdiff(toggleable_cols, cols_to_hide)
      
      # Apply fast UI changes via Proxy
      if (length(cols_to_show) > 0) DT::showCols(ranked_proxy, which(col_names %in% cols_to_show) - 1)
      if (length(cols_to_hide) > 0) DT::hideCols(ranked_proxy, which(col_names %in% cols_to_hide) - 1)
      
    }, ignoreNULL = FALSE)
    
    
    observeEvent(input$newOrder, {
      req(rv$ranked)
      
      # Reorder data by new rank and re-rank
      new_data <- copy(rv$ranked) |>
        fmutate(new_rank = input$newOrder) |>
        roworder(new_rank) |>
        fmutate(
          rank = seq_along(project_id),
          new_rank = NULL
        )
      
      # Recalculate completely handles the new ranks and tiers based on the new array order
      rv$ranked <- recalculate_ranking(new_data)
    }, ignoreInit = TRUE)
    
    output$ui_ranked_list <- renderDT({
      req(fnrow(rv$ranked) > 0)
      render_projects_dt(format_ranked_tbl(rv$ranked), show_beds = isTruthy(input$toggle_beds))
    }, server=FALSE)
    
    ranked_proxy <- dataTableProxy("ui_ranked_list",session = session)
    observe({
      req(rv$ranked)
      replaceData(ranked_proxy, format_ranked_tbl(rv$ranked), resetPaging = FALSE)
    })
    
    observeEvent(input$ui_ranked_list_cell_edit, {
      info <- input$ui_ranked_list_cell_edit
      new_val <- as.integer(gsub("[^0-9.]", "", info$value))
      
      if (!is.na(new_val)) {
        # Create a copy so Shiny knows it changed and redraws the value via Proxy
        tmp <- copy(rv$ranked)
        p_id <- tmp[info$row, project_id]
        tmp[project_id == p_id, coc_funding_recommendation := new_val]
        rv$ranked <- tmp
      }
    })
      
    # render_minor_dt <- function(dt) {
    #   if (is.null(dt) || nrow(dt) == 0) return(NULL)
    #   
    #   display_df <- dt |>
    #     fselect(rank, priority, weighted_score, met_hud_thresholds, met_coc_thresholds, funding_action, project_type, target_population, organization_name, project_name, coc_funding_requested, coc_funding_recommendation)
    #   
    #   datatable(
    #     display_df, 
    #     rownames = FALSE, 
    #     options = list(
    #       dom = 't', 
    #       paging = FALSE, 
    #       scrollY = NULL
    #     )
    #   ) |>
    #     table_styles()
    # }
    # output$ui_yhdp_ren_list <- renderDT({ render_minor_dt(rv$yhdp_ren) })
    # output$ui_yhdp_oth_list <- renderDT({ render_minor_dt(rv$yhdp_oth) })
    output$ui_excluded_list <- renderDT({ 
      tbl_has_rows <- isTruthy(fnrow(rv$excluded) > 0)
      shinyjs::toggle(id = "excluded_tbl_title", condition = tbl_has_rows)
      req(tbl_has_rows)
      render_projects_dt(format_ranked_tbl(rv$excluded), type = "excluded", show_beds = isTruthy(input$toggle_beds)) 
    })
    
    observeEvent(input$btn_adjust_tiers, {
      req(rv$ranked)
      rv$ranked <- recalculate_ranking(copy(rv$ranked))
      showNotification("Tiers and Bonuses recalculated successfully.", type = "message")
    })
    
    observeEvent(input$btn_save_ranking, {
      req(user_coc$coc_version_id)
      all_rankings <- rbindlist(list(
        rv$ranked[, .(project_id, rank, tier, coc_funding_recommendation)],
        # rv$yhdp_ren[, .(project_id, rank = NA_integer_, tier = "YHDP", coc_funding_recommendation)],
        # rv$yhdp_oth[, .(project_id, rank = NA_integer_, tier = "YHDP", coc_funding_recommendation)],
        rv$excluded[, .(project_id, rank = NA_integer_, tier = "Excluded", coc_funding_recommendation)]
      ), fill = TRUE)
      
      all_rankings[, coc_version_id := user_coc$coc_version_id]
      all_rankings[, created_by := user_coc$username]
      
      update_ranking_db(get_db_pool(), all_rankings)
    })
  })
}