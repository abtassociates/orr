mod_ranking_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Ranking",
    value = "ranking",
    
    layout_columns(
      fill = FALSE,
      col_widths = c(3,3,3,3),
      actionButton(
        ns("conduct_ranking"), 
        "Generate Ranking", 
        class = "btn-info btn-lg w-100", 
        icon = icon("ranking-star")
      ),
      actionButton(
        ns("btn_adjust_tiers"), 
        "Adjust Tiers after Funding Changes", 
        class = "btn-info btn-lg w-100", 
        icon = icon("arrows-rotate"),
        disabled = TRUE
      ),
      actionButton(
        ns("btn_save_ranking"), 
        "Save Ranking", 
        class = "btn-success btn-lg w-100", 
        icon = icon("save"),
        disabled = TRUE
      ),
      actionButton(
        ns("btn_export_ranking"), 
        "Export Ranking", 
        class = "btn-success btn-lg w-100", 
        icon = icon("file-export"),
        disabled = TRUE
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
    
    bslib::accordion(
      id = ns("funding_analysis_accordion"),
      open = FALSE,
      bslib::accordion_panel(
        "Funding Analysis Table",
        icon = icon("table"),
        uiOutput(ns("ui_funding_analysis"))
      )
    ),
    
    br(),
    div(
      id = ns("table_info"),
      
      shinyWidgets::virtualSelectInput(
        inputId = ns("hidden_cols"),
        label = "Hidden Columns:",
        choices = NULL, # Populated dynamically in server
        multiple = TRUE,
        width = "200px",
        showValueAsTags = TRUE,
        placeholder = "No columns hidden"
      ),
      
      div(
        id = ns("legend"),
        # The Striped Box
        div(),
        
        # The Label (Text and Equals)
        span("= Straddle Project")
      )
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
    
    tier1_id <- get_lookup_refid("Tier 1", "tier")
    tier2_id <- get_lookup_refid("Tier 2", "tier")
    tier3_id <- get_lookup_refid("Projects Exceeding ARD Adj", "tier")
    tier4_id <- get_lookup_refid("Excluded", "tier")
    
    # when user makes updates to the app, we update this to flag for the user they should run the ranking
    ranking_needs_refresh <- reactiveVal(FALSE)
    
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
      dt_sub <- dt[tier %in% c(tier1_id, tier2_id) & is_over_target == FALSE]
      
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
    
    get_allocated_funding <- function(id, condition) {
      reactive({
        req(fnrow(rv$ranked) > 0)
        dt <- rv$ranked[eval(condition)]
        
        allocated <- if(id == "dv_bonus")
          list(
            total = if(fnrow(dt) > 0) fsum(dt$coc_funding_recommendation) else 0,
            tier1 = if(fnrow(dt) > 0) fsum(dt[tier == tier1_id]$coc_funding_recommendation) else 0,
            tier2 = if(fnrow(dt) > 0) fsum(dt[tier == tier2_id]$coc_funding_recommendation) else 0
          )
        else {
          if(fnrow(dt) == 0) 
            0
          else if(id == "tier_2") {
            dt_t2_reg <- dt[tier == tier2_id]
            list(
              straddle = fsum(dt$straddle_amount),
              tier2 = if(fnrow(dt_t2_reg) > 0) fsum(dt_t2_reg$coc_funding_recommendation) else 0
            )
          } else
            fsum(dt$coc_funding_recommendation)
        }
        
        allocated
      })
    }
    alloc_tier1 <- get_allocated_funding("tier_1", quote(tier == tier1_id))
    alloc_tier2 <- get_allocated_funding("tier_2", quote((tier == tier2_id & project_id != "PLACEHOLDER_T2") | straddle_amount > 0))
    alloc_coc <- get_allocated_funding("coc_bonus", quote(coc_selected == TRUE))
    alloc_exceed <- get_allocated_funding("exceeds", quote(tier == tier3_id))
    alloc_dv <- get_allocated_funding("dv_bonus", quote(dv_selected == TRUE))
    
    mod_ranking_widget_server("coc_bonus", alloc_coc, coc_ard_data, "CoC Bonus")
    mod_ranking_widget_server("tier_1", alloc_tier1, coc_ard_data, "Tier 1 (Adj ARD * 90%)")
    mod_ranking_widget_server("tier_2", alloc_tier2, coc_ard_data, "Tier 2 (Adj ARD * 10% + CoC Bonus + DV Bonus)")
    mod_ranking_widget_server("dv_bonus", alloc_dv, coc_ard_data, "DV Bonus")
    mod_ranking_widget_server("exceeds", alloc_exceed, coc_ard_data, "Exceeding ARD Adj")
    
    observeEvent(c(user_coc$coc_version_id, user_coc$projects_updated, user_coc$rating_updated, user_coc$priorities_and_ceilings_updated), { 
      ranking_needs_refresh(TRUE)
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
      
      dt <- dt |> fsubset(project_id != "PLACEHOLDER_T2")
      
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
        prev_cum < coc_ard_data()$tier_1, tier1_id, # this allows straddles
        cum_funding <= (coc_ard_data()$tier_1 + coc_ard_data()$tier_2), tier2_id, # this doesn't allow straddles
        default = tier3_id
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
      
      dt[is_over_target == FALSE, bonus_highlight := fcase(coc_selected, "coc", dv_selected, "dv", default = "none")]

      # Create a dummy Tier 2 row if no Tier 2 projects
      if (!anyv(dt$tier, tier2_id)) {
        dummy <- dt[1, ]
        dummy[1, ] <- NA
        dummy$tier <- tier2_id
        dummy$project_id <- "PLACEHOLDER_T2"
        dummy$is_over_target <- FALSE
        dummy$ineligible <- FALSE
        
        dt <- rbind(dt, dummy)
      }
      
      dt <- dt |>
        roworder(tier, rank)
      
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
    
    ranked_projects_db <- reactive({
      get_projects_to_rank(user_coc$coc_version_id) |>
        fmutate(total_beds = all_fam_beds + all_ind_beds)
    })
    
    # Process Initial Data on Load or Reset
    process_data <- function(force_reset = FALSE) {
      req(user_coc$coc_version_id)
      
      shinyjs::enable("btn_adjust_tiers")
      shinyjs::enable("btn_save_ranking")
      shinyjs::enable("btn_export_ranking")
      
      raw_data <- ranked_projects_db()
      
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
          
          unmet_thresholds = met_hud_thresholds == FALSE | met_coc_thresholds == FALSE,
          
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
      ranked_data <- raw_data |>
        fsubset(ineligible == FALSE) |>
        calculate_priority()
      
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
      if (fnrow(over_target) > 0) {
        over_target[, tier := tier4_id]
        over_target[, rank := "Over Target"] # Converted intentionally to character to display in dt
        # Make sure rv$excluded has a character rank row so they combine
        if (!is.character(rv$excluded$rank)) rv$excluded[, rank := as.character(rank)]
        rv$excluded <- rbindlist(list(rv$excluded, over_target), fill = TRUE, use.names = TRUE)
      }
      
      rv$excluded <- rv$excluded |>
        fmutate(priority = "Unspecified") |>
        colorder(rank, priority, pos = "after")
      
      rv$ranked <- ranked_data |>
        fsubset(is_over_target == FALSE) |>
        colorder(rank, priority, pos = "after") # move priority after rank
      
      ranking_needs_refresh(FALSE)
      
      updateActionButton(
        session,
        "conduct_ranking",
        label = "Regenerate Ranking"
      )
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
        "ineligible",
        "version_id"
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
    
    structural_cols <- c("project_id", "tier", "bonus_highlight", "coc_cum", "sort_project_type", "is_over_target", "straddle_amount")
    
    table_styles <- function(dt, type = "main") {
      dt <- dt |>
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
            'coc_funding_recommendation', 
            backgroundColor = USER_ENTRY_BG_COLOR,
            fontWeight = 'bold'
          ) |>
          formatStyle(
            columns = 'bonus_highlight',  # Replace with your actual column name
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
      else
        dt <- dt |>
          formatStyle(
            columns = "coc_funding_recommendation",
            valueColumns = "rank",
            backgroundColor = styleEqual("Over Target", USER_ENTRY_BG_COLOR),
            fontWeight = 'bold'
          )
      
      dt
    }
    
    render_projects_dt <- function(final, type = "main") {
      shiny::validate(
        need(
          !ranking_needs_refresh(), 
          "Data has been updated. Click 'Conduct Ranking' to update."
        )
      )
      
      colnames <- names(final)
      
      disabled_cols <- setdiff(
        0:(ncol(final) - 1), 
        which(colnames == "coc_funding_recommendation") - 1
      )
      
      bed_cols <- grep("bed", colnames, ignore.case = TRUE) - 1
      
      columnDefs <- list(
        list(
          targets = 0,
          className = if (type == "excluded") "hidden" else "drag-handle",
          width = "30px",
          visible = type != "excluded"
        ),
        list(targets = which(colnames %in% structural_cols) - 1, visible = FALSE),
        list(targets = bed_cols, visible = FALSE)
      )
      
      straddle_col_idx <- which(colnames == "straddle_amount") - 1
      funding_col_idx  <- which(colnames == "coc_funding_recommendation") - 1
      project_id_col_idx <- which(colnames == "project_id") - 1
      
      final <- final %>%
        fmutate(tier = convert_to_factor(., "tier"))
      
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
            
            # Disable the native "No group" fallback header completely
            emptyDataGroup = JS("null"), 
            
            startRender = JS("
              function(rows, group) {
                // Map the group name to the CSS classes we added in custom.css
                var grpClass = '';
                if (group == 'Tier 1') { grpClass = 'group-tier_1'; } 
                else if (group == 'Tier 2') { grpClass = 'group-tier_2'; } 
                else if (group == 'Projects Exceeding ARD Adj') { grpClass = 'group-exceeds'; }
                
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
              // Add Tier 2 placeholder row if needed
              var projectIdIdx = %d; // Update this to your project_id column index
              if (data[projectIdIdx] === 'PLACEHOLDER_T2') {
                $(row).addClass('tier2-placeholder');
                $(row).find('td').removeClass('drag-handle');
                $('td', row).empty().html('📂 No projects in Tier 2. Drag here to reassign.')
                            .attr('colspan', '100%%');
                $('td', row).not(':first').remove(); // Hide other cells
              }
        
        
              var straddleIdx = %d;
              var fundingIdx = %d;
              
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
            }", project_id_col_idx, straddle_col_idx, funding_col_idx
          ))
        ),
        # for tracking reordered rows
        callback = JS(sprintf("
          table.on('row-reorder', function(e, details, edit) {
            if (!details || details.length === 0) return;
        
            // Calculate new order
            var newOrder = table.rows().indexes().toArray();
            for(var i of details) {
              newOrder[i.oldPosition] = i.newPosition;
            }
        
            // Find the nearest group header above the drop
            var draggedRow = edit.triggerRow.node();
            var headerAbove = $(draggedRow).prevAll('tr.group_header').first();
            var target_tier = headerAbove.length > 0 ? headerAbove.text().trim() : 'Tier 1';
            
            // Send to Shiny
            Shiny.setInputValue('%s', {
              order: newOrder,
              target_tier: target_tier
            }, {priority: 'event'});
          });",
          ns("reorderEvent")
        ))
      ) |>
        table_styles(type = type)
      
      shinyjs::addClass(id = "table_info", class = "flex-show")
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
    
    # If user drags and drops to reorder
    observeEvent(input$reorderEvent, {
      req(input$reorderEvent)
      
      reordered_df <- copy(rv$ranked) |>
        fmutate(rank = unlist(input$reorderEvent$order) + 1) |>
        roworder(rank)

      if(fnrow(reordered_df |> fsubset(tier == tier2_id & project_id != "PLACEHOLDER_T2")) > 0) {
        reordered_df <- reordered_df |> 
          fsubset(project_id != "PLACEHOLDER_T2")
      }

      new_data <- recalculate_ranking(reordered_df)
      
      rv$ranked <- new_data |> fsubset(!(ineligible | is_over_target))
      rv$excluded <- new_data |> fsubset(ineligible | is_over_target)
    }, ignoreInit = TRUE)
    
    output$ui_ranked_list <- renderDT({
      dt <- rv$ranked
      req(fnrow(dt) > 0)
      
      render_projects_dt(format_ranked_tbl(dt))
    }, server=TRUE)
    
    ranked_proxy <- dataTableProxy("ui_ranked_list",session = session)
    observeEvent(rv$ranked, {
      dt <- format_ranked_tbl(rv$ranked)
      replaceData(ranked_proxy, dt, rownames = FALSE, resetPaging = FALSE)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

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
      
    # output$ui_yhdp_ren_list <- renderDT({ render_minor_dt(rv$yhdp_ren) })
    # output$ui_yhdp_oth_list <- renderDT({ render_minor_dt(rv$yhdp_oth) })
    output$ui_excluded_list <- renderDT({ 
      dt <- isolate(rv$excluded)
      tbl_has_rows <- isTruthy(fnrow(dt) > 0)
      shinyjs::toggle(id = "excluded_tbl_title", condition = tbl_has_rows)
      req(tbl_has_rows)
      
      render_projects_dt(format_ranked_tbl(dt), type = "excluded") 
    })
    
    excluded_proxy <- dataTableProxy("ui_excluded_list",session = session)
    observeEvent(rv$excluded, {
      dt <- format_ranked_tbl(rv$excluded)
      replaceData(excluded_proxy, dt, rownames = FALSE, resetPaging = FALSE)
    })
    
    observeEvent(input$conduct_ranking, {
      raw_data <- ranked_projects_db()
      if(IN_DEV_MODE || tolower(user_coc$username) == "alex.silverman@abtglobal.com") {
        raw_data[, coc_funding_requested := fcoalesce(coc_funding_requested, coerceValue(sample(10000:1000000, .N), coc_funding_requested))]
        raw_data[, coc_funding_recommendation := fcoalesce(coc_funding_recommendation, coerceValue(coc_funding_requested, coc_funding_recommendation))]
        raw_data[, weighted_score := fcoalesce(weighted_score, DT::coerceValue(sample(100, .N), weighted_score))]
        raw_data[, met_hud_thresholds := TRUE]
        raw_data[, met_coc_thresholds := TRUE]
        # raw_data[, met_hud_thresholds := as.logical(fcoalesce(DT::coerceValue(met_hud_thresholds, 0L), sample(0:1, .N, replace=TRUE)))]
        # raw_data[, met_coc_thresholds := as.logical(fcoalesce(DT::coerceValue(met_coc_thresholds, 0L), sample(0:1, .N, replace=TRUE)))]
        raw_data[, rating_complete := 1]
      }

      
      if(allNA(raw_data$weighted_score) || 
         allNA(raw_data$coc_funding_requested) || 
         (allNA(raw_data$met_hud_thresholds) && allNA(raw_data$met_coc_thresholds))
      ) {
        sendSweetAlert(
          title = "Missing Ranking Info!",
          text = "You are missing rating scores, thresholds, and/or CoC funding requested amounts",
          type = "error"
        )
        return(FALSE)
      }
      
      if(!isTruthy(all(raw_data$rating_complete))) {
        confirmSweetAlert(
          inputId = ns("confirm_incomplete_ratings"),
          title = "One or more project ratings incomplete!",
          text = "One or more projects' rating and/or threshold is not marked as complete. Continue?",
          type = "warning"
        )
        return(FALSE)
      }
      
      process_data(force_reset = FALSE) 
    })
    
    observeEvent(input$confirm_incomplete_ratings, {
      req(input$confirm_incomplete_ratings)
      process_data(force_reset = FALSE) 
    })
    
    observeEvent(input$btn_adjust_tiers, {
      req(rv$ranked)
      rv$ranked <- recalculate_ranking(copy(rv$ranked))
      showNotification("Tiers and Bonuses recalculated successfully.", type = "message")
    })
    
    observeEvent(input$btn_save_ranking, {
      req(user_coc$coc_version_id)
      
      all_rankings <- collapse::rowbind(rv$ranked[project_id != "PLACEHOLDER_T2"], rv$excluded, fill=TRUE, idcol = TRUE) |>
        fmutate(
          rank = fifelse(.id == 2, NA_integer_, as.integer(rank)),
          coc_version_id = user_coc$coc_version_id,
          created_by = user_coc$username
        ) |>
        fselect(project_id, coc_version_id, rank, tier, coc_funding_recommendation, created_by, version_id)
      
      update_ranking_db(get_db_pool(), all_rankings)
    })
    
    observeEvent(input$btn_export_ranking, {
      req(user_coc$coc_version_id, rv$ranked)
      
      sendSweetAlert(session = session, title = "Coming soon!", "The Ranking Export feature is coming soon!")
    })
  })
}