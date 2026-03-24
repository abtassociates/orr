mod_ranking_widget_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("widget_box"))
}

mod_ranking_widget_server <- function(id, allocated, total, title, bg_color = "blue", icon_name = "piggy-bank") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$widget_box <- renderUI({
      
      # Handle if allocated is a list (like DV Bonus) or a single number
      alloc_data <- allocated()
      if (is.list(alloc_data)) {
        alloc <- fcoalesce(as.integer(alloc_data$total), 0L)
        alloc_t1 <- fcoalesce(as.integer(alloc_data$t1), 0L)
        alloc_t2 <- fcoalesce(as.integer(alloc_data$t2), 0L)
        
        # Create UI elements for the breakdown
        breakdown_ui <- tagList(
          p(paste("Allocated to Tier 1:", scales::dollar(alloc_t1)), style = "margin-bottom: 0; font-size: 0.85em; opacity: 0.9;"),
          p(paste("Allocated to Tier 2:", scales::dollar(alloc_t2)), style = "margin-bottom: 0.2rem; font-size: 0.85em; opacity: 0.9;")
        )
      } else {
        alloc <- fcoalesce(as.integer(alloc_data), 0L)
        breakdown_ui <- NULL
      }
      
      # Split title for styling: e.g., "Tier 1" (bold) and "(Adj ARD * 90%)" (small)
      title_parts <- strsplit(title, " \\(")[[1]]
      main_title <- title_parts[1]
      
      title_block <- if (length(title_parts) > 1) {
        sub_title <- paste0("(", title_parts[2])
        tagList(
          div(main_title, style = "font-size: 1.3em; font-weight: 900; line-height: 1.1;"),
          div(sub_title, style = "font-size: 0.75em; font-weight: normal; opacity: 0.9; margin-top: 3px;")
        )
      } else {
        div(main_title, style = "font-size: 1.3em; font-weight: 900;")
      }
      
      # Limit and Remaining logic (Handle Inf for Exceeds bucket)
      if (is.infinite(total)) {
        limit_ui <- NULL
        sub_text <- NULL
      } else {
        is_exceeded <- alloc > total
        diff_amt <- abs(total - alloc)
        show_warning <- is_exceeded && total > 0
        
        limit_ui <- p(paste("Limit:", scales::dollar(total)), class = "ranking-limit")
        
        sub_text <- if (show_warning) {
          p(
            icon("triangle-exclamation"), 
            paste("Overallocated by:", scales::dollar(diff_amt)), 
            class = "ranking-exceeded"
          )
        } else {
          p(paste("Remaining:", scales::dollar(diff_amt)), class = "ranking-remaining")
        }
      }
      
      bslib::value_box(
        title = title_block,
        value = scales::dollar(alloc),
        breakdown_ui,
        limit_ui,
        sub_text,
        showcase = icon(icon_name),
        theme = bslib::value_box_theme(bg = bg_color, fg = "white")
      )
    })
  })
}