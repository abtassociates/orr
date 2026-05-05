mod_ranking_widget_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("widget_box"))
}

mod_ranking_widget_server <- function(id, allocated, coc_ard_data, title) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$widget_box <- renderUI({
      total <- coc_ard_data()[[id]]
      alloc_data <- allocated()
      
      # ----------------------------------------------------------------------
      # 1. Data parsing, Breakdowns, & Progress Bar Flag
      # ----------------------------------------------------------------------
      show_progress_bar <- TRUE
      breakdown_ui <- div(class="breakdown")
      
      if (id == "exceeds") {
        alloc <- if(!length(alloc_data)) 0 else fcoalesce(as.integer(alloc_data), 0L)
        breakdown_ui <- NULL
      } else if (id == "dv_bonus") {
        # DV BONUS (Requires Tier 1 / Tier 2 breakdown)
        alloc <- fcoalesce(as.integer(alloc_data$total), 0L)
        alloc_t1 <- fcoalesce(as.integer(alloc_data$t1), 0L)
        alloc_t2 <- fcoalesce(as.integer(alloc_data$t2), 0L)
        
        if(!length(alloc_t1)) alloc_t1 <- 0
        if(!length(alloc_t2)) alloc_t2 <- 0
        
        breakdown_ui <- div(
          class="breakdown",
          p(paste("Allocated to Tier 1:", scales::dollar(alloc_t1))),
          p(paste("Allocated to Tier 2:", scales::dollar(alloc_t2)))
        )
        
      } else if (id == "tier_2") {
        # TIER 2 (Requires Straddle check)
        alloc <- fcoalesce(as.integer(alloc_data$tier2), 0L)
        straddle <- fcoalesce(as.integer(alloc_data$straddle), 0L)
        alloc <- alloc + straddle
        
        if(straddle > 0) {
          breakdown_ui <- div(
            class = "breakdown",
            p(paste0("From Straddle: ", scales::dollar(straddle)))
          )
        }
        
      } else {
        # STANDARD / DEFAULT (Tier 1, CoC Bonus, etc.)
        alloc <- if(!length(alloc_data)) 0 else fcoalesce(as.integer(alloc_data), 0L)
      }
      
      # ----------------------------------------------------------------------
      # 2. TITLE FORMATTING
      # ----------------------------------------------------------------------
      title_parts <- strsplit(title, " \\(")[[1]]
      
      title_block <- tagList(
        div(class = "value-box-title", title_parts[1]),
        div(class = "subtitle", if (length(title_parts) > 1) paste0("(", title_parts[2]) else HTML("&nbsp;"))
      )
      
      # ----------------------------------------------------------------------
      # 3. BUILD THE MAIN VALUE UI
      # ----------------------------------------------------------------------
      if (id == "exceeds") {
        # --- NO PROGRESS BAR
        val_ui <- div(scales::dollar(alloc))
        
      } else {
        # --- PROGRESS BAR GENERATION ---
        diff <- total - alloc
        pct <- if(total == 0) 0 else (alloc / total) * 100
        pct_width <- min(max(pct, 0), 100)
        
        if(id == "tier_2") {
          straddle_pct <- if(total == 0) 0 else (straddle / total) * 100
          straddle_pct_width <- min(max(straddle_pct, 0), 100)
        }
        
        # Determine Status
        if (alloc == total && total > 0) {
          status_type <- "success"
          bar_label <- "FULLY ALLOCATED"
          icon_name <- "circle-check"
          
        } else if (diff > 0) {
          status_type <- "remaining"
          bar_label <- paste("REMAINING:", scales::dollar(diff))
          icon_name <- NULL
          
        } else if (id == "tier_1" && diff < 0) {
          status_type <- "straddle"
          bar_label <- paste("STRADDLE AMOUNT:", scales::dollar(abs(diff)))
          icon_name <- "triangle-exclamation"
          pct_width <- 100 
          
        } else if (diff < 0) {
          status_type <- "danger"
          bar_label <- paste("OVERALLOCATED:", scales::dollar(abs(diff)))
          icon_name <- "circle-exclamation"
          pct_width <- 100 
          
        } else {
          status_type <- "remaining"
          bar_label <- paste("REMAINING:", scales::dollar(diff))
          icon_name <- NULL
        }
        
        # Build Progress Bar HTML
        val_ui <- div(
          # Min/Max Labels
          div(
            class = "progress-label",
            span(scales::dollar(alloc)),
            span(scales::dollar(total))
          ),
          
          # Combined Status Bar
          div(
            class = paste("combined-status-bar", paste0("status-", status_type)),
            role = "progressbar",
            `aria-valuenow` = round(pct_width),
            `aria-valuemin` = 0,
            `aria-valuemax` = 100,
            `aria-valuetext` = bar_label, 
            
            # Fill Background
            div(
              class = "bar-fill",
              style = paste0("width: ", pct_width, "%;"),
              `aria-hidden` = "true"
            ),
            
            if(id == "tier_2") div(
              class="straddle-bar-fill",
              style = paste0("width: ", straddle_pct_width, "%;"),
              `aria-hidden` = "true"
            ) else NULL,
            
            # Overlaid Text (using shiny::icon)
            div(
              class = "bar-label",
              `aria-hidden` = "true",
              if(!is.null(icon_name)) icon(icon_name) else NULL,
              bar_label
            )
          )
        )
      }
      
      # ----------------------------------------------------------------------
      # 4. RENDER VALUE BOX
      # ----------------------------------------------------------------------
      bslib::value_box(
        id = ns(paste0(id, "-widget")),
        title = title_block,
        value = tagList(
          breakdown_ui,
          val_ui
        ),
        theme = bslib::value_box_theme(bg = glue::glue("var(--brand-{id})"), fg = "white"),
        height = "250px"
      )
    })
  })
}