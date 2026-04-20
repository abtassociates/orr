mod_download_rating_ui <- function(id) {
  ns <- NS(id)
  
  # --- Compact Dropdown Header for Downloads ---
  card_header(
    class = "d-flex justify-content-between align-items-center",
    "Project Rating",
    div(
      class = "dropdown",
      tags$button(
        class = "btn btn-outline-primary dropdown-toggle btn-sm",
        type = "button",
        `data-bs-toggle` = "dropdown",
        icon("download"), " Download Reports"
      ),
      tags$ul(
        class = "dropdown-menu dropdown-menu-end",
        tags$li(downloadLink(ns("dl_current"), "Current Project Report Card (HTML)", class = "dropdown-item")),
        tags$li(downloadLink(ns("dl_blank"), "Blank Rating Template (HTML)", class = "dropdown-item")),
        tags$hr(class="dropdown-divider"),
        tags$li(downloadLink(ns("dl_all_tabular"), "All Projects Summary (CSV)", class = "dropdown-item"))
      )
    )
  )
}

mod_download_rating_server <- function(id, user_coc, selected_project, funding_action, factors_and_scores_for_project) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -------- Download Handlers --------------
    
    ## Helper to generate RMarkdown for HTML dynamically -----
    
    # build_report <- function(data, project_name, total, max_pts, file) {
    #   tempReport <- file.path(tempdir(), "report_card.Rmd")
    #   
    #   # Note the use of results='asis' in the chunk, which allows us to use a loop 
    #   # to dynamically generate Headers and Tables for each Factor Group.
    #   rmd_content <- c(
    #     "---",
    #     paste0("title: 'Rating Report Card: ", project_name, "'"),
    #     "output: ",
    #     "  html_document:",
    #     "    theme: flatly", # Gives it a clean, modern Bootstrap look
    #     "params:",
    #     "  report_data: NA", 
    #     "---",
    #     "```{r setup, include=FALSE}",
    #     "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    #     "```",
    #     "",
    #     "## **Overall Score:** <span style='color:blue;'>", total, " out of ", max_pts, "</span>",
    #     "***",
    #     "",
    #     "```{r, results='asis'}",
    #     "library(dplyr)",
    #     "library(knitr)",
    #     "",
    #     "# Split the data by Factor Group",
    #     "grouped_data <- split(params$report_data, params$report_data$factor_group)",
    #     "",
    #     "for (group_name in names(grouped_data)) {",
    #     "  group_df <- grouped_data[[group_name]]",
    #     "  ",
    #     "  # Calculate subtotals for this specific group",
    #     "  subtotal_score <- sum(as.numeric(group_df$rating_score), na.rm = TRUE)",
    #     "  subtotal_max <- sum(as.numeric(group_df$max_point_value), na.rm = TRUE)",
    #     "  ",
    #     "  # Print an HTML Header for the group and its subtotal",
    #     "  cat(sprintf('### %s (Subtotal: %s / %s)\\n\\n', group_name, subtotal_score, subtotal_max))",
    #     "  ",
    #     "  # Select and rename columns for a clean table",
    #     "  table_output <- group_df %>%",
    #     "    select(",
    #     "      `Subgroup` = factor_subgroup,",
    #     "      `Factor` = rating_factor_text, ",
    #     "      `Goal` = goal, ",
    #     "      `Performance` = performance, ",
    #     "      `Score` = rating_score, ",
    #     "      `Max Pts` = max_point_value",
    #     "    ) %>%",
    #     "    # Convert NA to blanks for cleaner reading",
    #     "    mutate(across(everything(), ~ifelse(is.na(.), '', .))) %>%",
    #     "    kable(format = 'markdown')",
    #     "  ",
    #     "  # Print the table",
    #     "  print(table_output)",
    #     "  ",
    #     "  # Add a horizontal line separator between groups",
    #     "  cat('\\n\\n***\\n\\n')",
    #     "}",
    #     "```"
    #   )
    #   
    #   writeLines(rmd_content, tempReport)
    #   
    #   rmarkdown::render(
    #     tempReport, 
    #     output_file = file,
    #     params = list(report_data = data), 
    #     envir = new.env(parent = globalenv())
    #   )
    # }
    build_report <- function(data, project_name, total, max_pts, file) {
      tempReport <- file.path(tempdir(), "report_card.Rmd")
      
      rmd_content <- c(
        "---",
        paste0("title: ' '"), # Hide default title, we will build a custom one
        "output: ",
        "  html_document:",
        "    theme: default",
        "params:",
        "  report_data: NA", 
        "---",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
        "```",
        "",
        "<!-- CUSTOM STYLING TO MIMIC SHINY APP UI -->",
        "<style>",
        "  /* Force browsers to print the background colors to PDF */",
        "  @media print {",
        "    body { -webkit-print-color-adjust: exact; print-color-adjust: exact; }",
        "  }",
        "  body { ",
        "    font-family: system-ui, -apple-system, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; ",
        "    color: #212529;",
        "  }",
        "  /* Overall Score Card Style */",
        "  .overall-score-card {",
        "    background-color: #f8f9fa; border-left: 5px solid #0d6efd;",
        "    padding: 1.5rem; border-radius: 0.5rem; margin-bottom: 2rem;",
        "    box-shadow: 0 .125rem .25rem rgba(0,0,0,.075);",
        "    display: flex; justify-content: space-between; align-items: center;",
        "  }",
        "  .overall-score-card h2 { margin: 0; font-size: 1.5rem; color: #212529; font-weight: 600; }",
        "  .overall-score-card .subtitle { color: #6c757d; font-size: 1rem; font-weight: normal; margin-top: 5px;}",
        "  .score-badge {",
        "    background-color: #0d6efd; color: white; padding: 0.5rem 1rem;",
        "    border-radius: 0.5rem; font-weight: bold; font-size: 1.25rem;",
        "  }",
        "  /* Accordion Header Mimic */",
        "  .accordion-mimic {",
        "    background-color: #e7f1ff; color: #0c63e4;",
        "    padding: 1rem 1.25rem; margin-top: 2rem; margin-bottom: 1rem;",
        "    border-radius: 0.375rem; font-weight: 600; font-size: 1.1rem;",
        "    display: flex; justify-content: space-between; align-items: center;",
        "    border: 1px solid #b6d4fe;",
        "  }",
        "  /* Table Styling */",
        "  .table thead th { ",
        "    background-color: #f8f9fa; color: #495057; ",
        "    font-size: 0.85rem; text-transform: uppercase; ",
        "    border-bottom: 2px solid #dee2e6; ",
        "  }",
        "  .table td { vertical-align: middle; }",
        "</style>",
        "",
        "<!-- MAIN HEADER CONTENT -->",
        "<div class='overall-score-card'>",
        "  <div>",
        "    <h2>Rating Report Card</h2>",
        paste0("    <div class='subtitle'>Project: <strong>", project_name, "</strong></div>"),
        "  </div>",
        paste0("  <div class='score-badge'>", total, " / ", max_pts, " Points</div>"),
        "</div>",
        "",
        "<!-- DYNAMIC GROUPS AND TABLES -->",
        "```{r, results='asis'}",
        "library(dplyr)",
        "library(knitr)",
        "",
        "# Split the data by Factor Group",
        "grouped_data <- split(params$report_data, params$report_data$factor_group)",
        "",
        "for (group_name in names(grouped_data)) {",
        "  group_df <- grouped_data[[group_name]]",
        "  ",
        "  # Calculate subtotals",
        "  subtotal_score <- sum(as.numeric(group_df$rating_score), na.rm = TRUE)",
        "  subtotal_max <- sum(as.numeric(group_df$max_point_value), na.rm = TRUE)",
        "  ",
        "  # Print the 'Accordion-style' Header",
        "  cat(sprintf('<div class=\"accordion-mimic\"><span>%s</span><span>Subtotal: %s / %s</span></div>\\n', ",
        "              group_name, subtotal_score, subtotal_max))",
        "  ",
        "  # Prepare Table Data",
        "  table_output <- group_df %>%",
        "    select(",
        "      `Subgroup` = factor_subgroup,",
        "      `Factor` = rating_factor_text, ",
        "      `Goal` = goal, ",
        "      `Performance` = performance, ",
        "      `Score` = rating_score, ",
        "      `Max Pts` = max_point_value",
        "    ) %>%",
        "    mutate(across(everything(), ~ifelse(is.na(.), '', .))) %>%",
        "    # kable with raw HTML format to inject Bootstrap table classes",
        "    kable(format = 'html', table.attr = 'class=\"table table-sm table-bordered table-striped\"')",
        "  ",
        "  print(table_output)",
        "}",
        "```"
      )
      
      writeLines(rmd_content, tempReport)
      
      rmarkdown::render(
        tempReport, 
        output_file = file,
        params = list(report_data = data), 
        envir = new.env(parent = globalenv())
      )
    }
    
    ## 1. Download Current Project ------------
    output$dl_current <- downloadHandler(
      filename = function() {
        req(selected_project())
        paste0("Report_Card_", gsub("[^A-Za-z0-9]", "_", selected_project()$project_name), ".html")
      },
      content = function(file) {
        df <- factors_and_scores_for_project()
        req(df, selected_project())
        
        # Use currently active data
        # Overwrite scores/performance with what is currently typed in the UI
        df$rating_score <- sapply(df$selected_rating_factor_id, function(id) input[[paste0("rating_score_", id)]])
        df$performance <- sapply(df$selected_rating_factor_id, function(id) input[[paste0("performance_", id)]])
        
        total <- sum(as.numeric(df$rating_score), na.rm = TRUE)
        max_pts <- sum(as.numeric(df$max_point_value), na.rm = TRUE)
        
        build_report(df, selected_project()$project_name, total, max_pts, file)
      }
    )
    
    ## 2. Download Blank Template------------
    output$dl_blank <- downloadHandler(
      filename = function() {
        "Blank_Rating_Template.html"
      },
      content = function(file) {
        df <- factors_and_scores_for_project()
        req(df)
        
        # Create a blank version of the current criteria
        df$rating_score <- NA
        df$performance <- NA
        
        max_pts <- fsum(as.numeric(df$max_point_value))
        
        build_report(df, "BLANK TEMPLATE", 0, max_pts, file)
      }
    )
    
    ## 3. Download All Projects Summary (Tabular CSV) ------------
    output$dl_all_tabular <- downloadHandler(
      filename = function() {
        paste0("All_Projects_Ratings_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(user_coc$coc_version_id)
        all_data <- get_all_rating_factors_and_scores(user_coc$coc_version_id, funding_action)
        write.csv(all_data, file, row.names = FALSE)
      }
    )
  })
  
}