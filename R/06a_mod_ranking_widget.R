mod_ranking_widget_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("widget_box"))
}

mod_ranking_widget_server <- function(id, allocated, coc_ard_data, title, icon_name = "piggy-bank") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$widget_box <- renderUI({
      total <- coc_ard_data()[[id]]
      
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
      
      title_block <- div(main_title)
      
      if (length(title_parts) > 1) {
        sub_title <- paste0("(", title_parts[2])
        title_block <- tagList(
          div(main_title),
          div(class="subtitle", sub_title)
        )
      }
      
      # Limit and Remaining logic (Handle Inf for Exceeds bucket)
      show_warning <- FALSE
      if (is.infinite(total)) {
        limit_ui <- NULL
        sub_text <- NULL
        value_box_class <- NULL
      } else {
        diff_amt <- total - alloc
        
        limit_ui <- p(class="value-box-value", "Eligible For:", span(class="val", scales::dollar(total)))
        
        if (diff_amt < 0) {
          if(id == "tier_1") {
            sub_text <- p(
              icon("triangle-exclamation"), 
              paste("Straddle Amount:", scales::dollar(abs(diff_amt))), 
              class = "ranking-remaining overallocated text-warning"
            )
          } else {
            sub_text <- p(
              icon("triangle-exclamation"), 
              paste("Overallocated by:", scales::dollar(abs(diff_amt))), 
              class = "ranking-remaining overallocated text-danger"
            )
          }
        } else if(diff_amt == 0) {
          sub_text <- p(
            icon("check", class="success"), 
            "Fully Allocated", 
            class = "ranking-remaining success"
          )
        } else {
          sub_text <- p(
            paste("Remaining:", scales::dollar(diff_amt)), 
            class = "ranking-remaining"
          )
        }
      }
      
      bslib::value_box(
        title = title_block,
        value = p("Allocated: ", span(class="val", scales::dollar(alloc))),
        breakdown_ui,
        limit_ui,
        sub_text,
        # showcase = icon(icon_name),
        theme = bslib::value_box_theme(bg = glue::glue("var(--brand-{id})"), fg = "white"),
        height = "250px"
      )
    })
  })
}