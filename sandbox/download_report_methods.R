# ##################################
# ## --- Grid ---------
# # Pros: Super lightweight
# # Cons: report looks awful
# ################################### 
# library(grid)
# library(gridExtra)
# library(gtable)
# library(dplyr)
# 
# build_report <- function(data, project_name, total, max_pts, file) {
# 
#   # --- Helper Function: Manual Word Wrap ---
#   # gridExtra tables won't wrap text, so we must insert \n manually
#   wrapper <- function(x, width) {
#     if (is.na(x) | x == "") return("")
#     paste(strwrap(x, width = width), collapse = "\n")
#   }
# 
#   # Prepare the PDF device
#   pdf(file, width = 8.5, height = 11)
# 
#   # Start drawing at the top of the page
#   y_pos <- 0.95
# 
#   # --- 1. Draw the Header Box ---
#   # Blue Background
#   grid.rect(x = 0.5, y = y_pos, width = 0.9, height = 0.08,
#             gp = gpar(fill = "#4A90E2", col = NA))
#   # Title Text
#   grid.text("Rating Report Card", x = 0.1, y = y_pos + 0.015,
#             just = "left", gp = gpar(col = "white", fontsize = 18, fontface = "bold"))
#   # Score Text
#   grid.text(paste0(total, " / ", max_pts, " Points"), x = 0.9, y = y_pos + 0.015,
#             just = "right", gp = gpar(col = "white", fontsize = 14, fontface = "bold"))
#   # Project Name
#   grid.text(paste0("Project: ", project_name), x = 0.1, y = y_pos - 0.015,
#             just = "left", gp = gpar(col = "white", fontsize = 10))
# 
#   y_pos <- y_pos - 0.08
# 
#   # --- 2. Process Groups ---
#   grouped_data <- split(data, data$factor_group)
# 
#   for (group_name in names(grouped_data)) {
#     df <- grouped_data[[group_name]]
# 
#     # Calculate Subtotals
#     sub_score <- fsum(df$rating_score)
#     sub_max <- fsum(df$max_point_value)
# 
#     # Draw Group Header Row
#     y_pos <- y_pos - 0.02
#     grid.rect(x = 0.5, y = y_pos, width = 0.9, height = 0.03,
#               gp = gpar(fill = "#EBF5FB", col = "#4A90E2", lwd = 0.5))
#     grid.text(group_name, x = 0.06, y = y_pos, just = "left",
#               gp = gpar(fontsize = 10, fontface = "bold", col = "#2E86C1"))
#     grid.text(paste0("Subtotal: ", sub_score, " / ", sub_max), x = 0.94, y = y_pos,
#               just = "right", gp = gpar(fontsize = 10, fontface = "bold", col = "#2E86C1"))
# 
#     y_pos <- y_pos - 0.02
# 
#     # Format the table data
#     table_df <- df %>%
#       mutate(
#         Factor = sapply(rating_factor_text, wrapper, width = 50), # WRAP TEXT HERE
#         Goal = sapply(goal, wrapper, width = 15),
#         Performance = sapply(performance, wrapper, width = 15)
#       ) %>%
#       select(Subgroup = factor_subgroup, Factor, Goal, Perf = Performance,
#              Score = rating_score, Max = max_point_value)
# 
#     # Create Table Graphical Object (Grob)
#     # Theme defines the look of the table
#     tt <- ttheme_default(
#       base_size = 8,
#       padding = unit(c(2, 2), "mm"),
#       core = list(fg_params = list(hjust=0, x=0.05),
#                   bg_params = list(fill = c("white", "#F9F9F9"), col=NA)),
#       colhead = list(fg_params = list(fontface="bold", hjust=0, x=0.05))
#     )
# 
#     tg <- tableGrob(table_df, rows = NULL, theme = tt)
# 
#     # Check if table fits on page (Basic implementation)
#     # If y_pos is too low, you'd normally call grid.newpage()
#     # Draw the table
#     pushViewport(viewport(x = 0.5, y = y_pos, width = 0.9, just = "top"))
#     grid.draw(tg)
#     popViewport()
# 
#     # Move y_pos down based on table height
#     y_pos <- y_pos - (convertHeight(sum(tg$heights), "npc", valueOnly = TRUE) + 0.02)
#   }
# 
#   dev.off()
# }
# 
# 
# ######################################
# # HTML ----------------
# # Easy + lightweight, but not a PDF
# ##################################
# build_report <- function(data, project_name, total, max_pts, file) {
#   tempReport <- file.path(tempdir(), "report_card.Rmd")
# 
#   # Ensure total is a number (fallback to 0 if NA)
#   safe_total <- ifelse(is.na(total), 0, total)
# 
#   rmd_content <- c(
#     "---",
#     paste0("title: ' '"),
#     "output: ",
#     "  html_document:",
#     "    theme: default",
#     "params:",
#     "  report_data: NA",
#     "---",
#     "```{r setup, include=FALSE}",
#     "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
#     "```",
#     "",
#     "<!-- CUSTOM STYLING TO MIMIC SHINY APP UI -->",
#     "<style>",
#     "  @media print {",
#     "    body { -webkit-print-color-adjust: exact; print-color-adjust: exact; }",
#     "  }",
#     "  body { ",
#     "    font-family: system-ui, -apple-system, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; ",
#     "    color: #212529;",
#     "  }",
#     "  .overall-score-card {",
#     "    background-color: #f8f9fa; border-left: 5px solid #0d6efd;",
#     "    padding: 1.5rem; border-radius: 0.5rem; margin-bottom: 2rem;",
#     "    box-shadow: 0 .125rem .25rem rgba(0,0,0,.075);",
#     "    display: flex; justify-content: space-between; align-items: center;",
#     "  }",
#     "  .overall-score-card h2 { margin: 0; font-size: 1.5rem; color: #212529; font-weight: 600; }",
#     "  .overall-score-card .subtitle { color: #6c757d; font-size: 1rem; font-weight: normal; margin-top: 5px;}",
#     "  .score-badge {",
#     "    background-color: #0d6efd; color: white; padding: 0.5rem 1rem;",
#     "    border-radius: 0.5rem; font-weight: bold; font-size: 1.25rem;",
#     "  }",
#     "  .accordion-mimic {",
#     "    background-color: #e7f1ff; color: #0c63e4;",
#     "    padding: 1rem 1.25rem; margin-top: 2rem; margin-bottom: 1rem;",
#     "    border-radius: 0.375rem; font-weight: 600; font-size: 1.1rem;",
#     "    display: flex; justify-content: space-between; align-items: center;",
#     "    border: 1px solid #b6d4fe;",
#     "  }",
#     "  .table thead th { ",
#     "    background-color: #f8f9fa; color: #495057; ",
#     "    font-size: 0.85rem; text-transform: uppercase; ",
#     "    border-bottom: 2px solid #dee2e6; ",
#     "  }",
#     "  .table td { vertical-align: middle; }",
#     "</style>",
#     "",
#     "<!-- MAIN HEADER CONTENT -->",
#     "<!-- NO LEADING SPACES HERE TO PREVENT MARKDOWN CODE BLOCKS -->",
#     "<div class='overall-score-card'>",
#     "<div>",
#     "<h2>Rating Report Card</h2>",
#     paste0("<div class='subtitle'>Project: <strong>", project_name, "</strong></div>"),
#     "</div>",
#     paste0("<div class='score-badge'>", safe_total, " / ", max_pts, " Points</div>"),
#     "</div>",
#     "",
#     "<!-- DYNAMIC GROUPS AND TABLES -->",
#     "```{r, results='asis'}",
#     "library(dplyr)",
#     "library(knitr)",
#     "",
#     "grouped_data <- split(params$report_data, params$report_data$factor_group)",
#     "",
#     "for (group_name in names(grouped_data)) {",
#     "  group_df <- grouped_data[[group_name]]",
#     "  ",
#     "  # Subtotals (Treat NAs as 0 so it displays nicely)",
#     "  subtotal_score <- sum(as.numeric(group_df$rating_score), na.rm = TRUE)",
#     "  subtotal_max <- sum(as.numeric(group_df$max_point_value), na.rm = TRUE)",
#     "  ",
#     "  cat(sprintf('<div class=\"accordion-mimic\"><span>%s</span><span>Subtotal: %s / %s</span></div>\\n', ",
#     "              group_name, subtotal_score, subtotal_max))",
#     "  ",
#     "  table_df <- group_df %>%",
#     "    select(",
#     "      `Subgroup` = factor_subgroup,",
#     "      `Factor` = rating_factor_text, ",
#     "      `Goal` = goal, ",
#     "      `Performance` = performance, ",
#     "      `Score` = rating_score, ",
#     "      `Max Pts` = max_point_value",
#     "    )",
#     "  ",
#     "  # Check if Subgroup is completely empty/NA. If so, remove the column.",
#     "  if (all(is.na(table_df$Subgroup) | table_df$Subgroup == '' | table_df$Subgroup == 'NA')) {",
#     "    table_df <- table_df %>% select(-Subgroup)",
#     "  }",
#     "  ",
#     "  # Render the table",
#     "  table_output <- table_df %>%",
#     "    mutate(across(everything(), ~ifelse(is.na(.), '', .))) %>%",
#     "    kable(format = 'html', table.attr = 'class=\"table table-sm table-bordered table-striped\"', escape = FALSE)",
#     "  ",
#     "  print(table_output)",
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
# 
# ##################################
# # # -------- LaTeX ---------
# # # 300MB + Fragile (Heavyweight)
# # # Need to run install script when first tarting up the app
# ##################################
# # Check for tinytex package
# if (!requireNamespace("tinytex", quietly = TRUE)) install.packages("tinytex")
# 
# # Install TinyTeX if not already present
# if (!tinytex::is_tinytex()) {
#   tinytex::install_tinytex()
# }
# 
# # Pre-install all required LaTeX packages
# # These are the packages your specific report uses
# pkgs <- c(
#   "tcolorbox",
#   "xcolor",
#   "booktabs",
#   "longtable",
#   "environ",
#   "fp",
#   "pgf",
#   "trimspaces",
#   "unicode-math", # Needed for modern Unicode support
#   "colortbl"      # Needed for colored tables
# )
# 
# tinytex::tlmgr_install(pkgs)
# 
# message("LaTeX environment is ready.")
# 
# library(tinytex)
# library(kableExtra)
# build_report <- function(data, project_name, total, max_pts, file) {
#   tempReport <- file.path(tempdir(), "report_card.Rmd")
# 
#   # Ensure total is a number (fallback to 0 if NA)
#   safe_total <- ifelse(is.na(total), 0, total)
# 
#   rmd_lines <- c(
#     "---",
#     "title: ' '",
#     "output: ",
#     "  pdf_document:",
#     "    latex_engine: xelatex",
#     "    extra_dependencies: [\"tcolorbox\", \"xcolor\", \"booktabs\", \"longtable\"]",
#     "params:",
#     "  report_data: NA",
#     "---",
#     "",
#     "```{r setup, include=FALSE}",
#     "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
#     "```",
#     "",
#     # Overall Score Box (LaTeX)
#     "\\begin{tcolorbox}[colback=gray!5, colframe=blue!70, arc=4pt, boxrule=1pt, leftrule=5pt]",
#     paste0("{\\Large \\textbf{Rating Report Card}} \\hfill \\colorbox{blue!70}{\\textcolor{white}{\\textbf{ ", safe_total, " / ", max_pts, " Points }}} \\\\[0.2cm]"),
#     paste0("Project: \\textbf{", project_name, "}"),
#     "\\end{tcolorbox}",
#     "",
#     "\\vspace{0.5cm}",
#     "",
#     # Dynamic Groups & Tables (R Chunk)
#     "```{r, results='asis'}",
#     "library(dplyr)",
#     "library(knitr)",
#     "library(kableExtra)",
#     "",
#     "grouped_data <- split(params$report_data, params$report_data$factor_group)",
#     "",
#     "for (group_name in names(grouped_data)) {",
#     "  group_df <- grouped_data[[group_name]]",
#     "  ",
#     "  subtotal_score <- fsum(group_df$rating_score)",
#     "  subtotal_max <- fsum(group_df$max_point_value)",
#     "  ",
#     "  cat(paste0(",
#     "    '\\n\\n\\\\begin{tcolorbox}[colback=blue!5, colframe=blue!30, arc=2pt, boxrule=0.5pt, left=2pt, right=2pt, top=2pt, bottom=2pt]\\n',",
#     "    '\\\\textbf{\\\\textcolor{blue!80}{', group_name, '}} \\\\hfill \\\\textbf{\\\\textcolor{blue!80}{Subtotal: ', subtotal_score, ' / ', subtotal_max, '}}\\n',",
#     "    '\\\\end{tcolorbox}\\n\\n'",
#     "  ))",
#     "  ",
#     "  table_df <- group_df %>%",
#     "    select(",
#     "      `Subgroup` = factor_subgroup,",
#     "      `Factor` = rating_factor_text, ",
#     "      `Goal` = goal, ",
#     "      `Performance` = performance, ",
#     "      `Score` = rating_score, ",
#     "      `Max Pts` = max_point_value",
#     "    )",
#     "  ",
#     "  if (all(is.na(table_df$Subgroup) | table_df$Subgroup == '' | table_df$Subgroup == 'NA')) {",
#     "    table_df <- table_df %>% select(-Subgroup)",
#     "    has_subgroup <- FALSE",
#     "  } else {",
#     "    has_subgroup <- TRUE",
#     "  }",
#     "  ",
#     "  table_output <- table_df %>%",
#     "    mutate(across(everything(), ~ifelse(is.na(.), '', as.character(.)))) %>%",
#     "    # Using LaTeX format allows for column width control",
#     "    kbl(format = 'latex', booktabs = TRUE, longtable = TRUE, linesep = '') %>%",
#     "    kable_styling(latex_options = c('hold_position', 'repeat_header'), font_size = 8) %>%",
#     "    column_spec(if(has_subgroup) 2 else 1, width = '6.5cm') %>%", # Factor
#     "    column_spec(if(has_subgroup) 3 else 2, width = '1.8cm') %>%", # Goal
#     "    column_spec(if(has_subgroup) 4 else 3, width = '2.2cm') %>%", # Performance
#     "    column_spec(if(has_subgroup) 5 else 4, width = '1cm') %>%",   # Score
#     "    column_spec(if(has_subgroup) 6 else 5, width = '1cm')",       # Max Pts
#     "  ",
#     "  if(has_subgroup) {",
#     "    table_output <- table_output %>% ",
#     "      column_spec(1, width = '2.5cm') %>%",
#     "      collapse_rows(columns = 1, latex_hline = 'major', valign = 'top')",
#     "  }",
#     "  ",
#     "  print(table_output)",
#     "  cat('\\n\\\\vspace{0.3cm}\\n')",
#     "}",
#     "```"
#   )
# 
#   writeLines(rmd_lines, tempReport)
# 
#   rmarkdown::render(
#     tempReport,
#     output_file = file,
#     params = list(report_data = data),
#     envir = new.env(parent = globalenv())
#   )
# }
# 
# ####################################
# ## Quarto + Typst (gt)
# ###################################
# build_report <- function(data, project_name, total, max_pts, file) {
#   library(quarto)
#   library(gt)
#   library(dplyr)
#   
#   # Handle NULL project name
#   p_name_display <- ifelse(is.null(project_name), "Blank Template", project_name)
#   
#   # Create a temporary .qmd file
#   temp_qmd <- tempfile(fileext = ".qmd")
#   
#   # Define the Quarto content
#   # We use Typst-specific syntax for the blue header box
#   qmd_content <- c(
#     "---",
#     "format: ",
#     "  typst:",
#     "    papersize: us-letter",
#     "    margin:",
#     "      x: 0.5in",
#     "      y: 0.5in",
#     "execute:",
#     "  echo: false",
#     "  warning: false",
#     "---",
#     "",
#     "```{typst}",
#     paste0("#rect(fill: rgb(\"#4A90E2\"), width: 100%, radius: 5pt, inset: 12pt)[",
#            "#set text(fill: white, weight: \"bold\")",
#            "#text(size: 18pt)[Rating Report Card] #h(1fr) #text(size: 14pt)[", total, " / ", max_pts, " Points] \\",
#            "Project: ", p_name_display, 
#            "]"),
#     "```",
#     "",
#     "```{r}",
#     "library(gt)",
#     "library(dplyr)",
#     "",
#     "# Prepare Data",
#     "table_data <- params$report_data %>%",
#     "  mutate(across(everything(), ~ifelse(is.na(.) | . == 'NA', '', as.character(.))))",
#     "",
#     "# Build Table",
#     "table_data %>%",
#     "  gt(groupname_col = 'factor_group') %>%",
#     "  summary_rows(",
#     "    groups = TRUE,",
#     "    columns = c(rating_score, max_point_value),",
#     "    fns = list(label = 'Subtotal', fn = ~sum(as.numeric(.), na.rm = TRUE)),",
#     "    side = 'top'",
#     "  ) %>%",
#     "  tab_options(",
#     "    table.width = pct(100),",
#     "    table.font.size = px(10),", # Typst fonts render slightly larger
#     "    row_group.background.color = '#EBF5FB',",
#     "    row_group.font.weight = 'bold',",
#     "    summary_row.background.color = '#F4F6F7'",
#     "  ) %>%",
#     "  cols_label(",
#     "    factor_subgroup = 'Subgroup',",
#     "    piping_text = 'Factor',",
#     "    goal = 'Goal',",
#     "    performance = 'Performance',",
#     "    rating_score = 'Score',",
#     "    max_point_value = 'Max Pts'",
#     "  ) %>%",
#     "  cols_width(",
#     "    factor_subgroup ~ px(80),",
#     "    piping_text ~ px(280),",
#     "    goal ~ px(70),",
#     "    performance ~ px(70),",
#     "    rating_score ~ px(50),",
#     "    max_point_value ~ px(50)",
#     "  ) %>%",
#     "  text_transform(",
#     "    locations = cells_body(columns = factor_subgroup),",
#     "    fn = function(x) ifelse(x == '', 'N/A', x)",
#     "  )",
#     "```"
#   )
#   
#   writeLines(qmd_content, temp_qmd)
#   
#   # Render the report
#   quarto::quarto_render(
#     input = temp_qmd,
#     output_file = basename(file),
#     execute_params = list(report_data = data)
#   )
#   
#   # Move file to final location if necessary
#   file.copy(basename(file), file, overwrite = TRUE)
#   return(file)
# }
# 
# 
# 
# 
# 
# #################################################
# ### gt + pagedown
# ###############################################
# build_report <- function(data, project_name, total, max_pts, file) {
#   
#   # Handle NULL project name for the Blank Template
#   p_name_display <- ifelse(is.null(project_name), "Blank Template", project_name)
#   
#   # 1. Prepare Data
#   table_data <- data |>
#     fselect(
#       factor_group,
#       Subgroup = factor_subgroup,
#       Factor = piping_text,
#       Goal = goal,
#       Performance = performance,
#       Score = rating_score,
#       `Max Pts` = max_point_value
#     ) %>%
#     mutate(across(everything(), ~ifelse(is.na(.) | . == "NA", "", as.character(.))))
#   
#   # 2. Build the gt Table
#   gt_table <- table_data %>%
#     gt(groupname_col = "factor_group") %>%
#     tab_header(
#       title = md(paste0("<div style='background-color:#4A90E2; color:white; padding:10px; border-radius:5px;'>",
#                         "<span style='font-size:24px; font-weight:bold;'>Rating Report Card</span>",
#                         "<span style='float:right;'>", total, " / ", max_pts, " Points</span>",
#                         "</div>")),
#       subtitle = md(paste0("<div style='padding-top:10px;'>Project: **", p_name_display, "**</div>"))
#     ) %>%
#     summary_rows(
#       groups = TRUE,
#       columns = c(Score, `Max Pts`),
#       fns = list(label = "Subtotal", fn = ~sum(as.numeric(.), na.rm = TRUE)),
#       side = "top"
#     ) %>%
#     tab_options(
#       table.width = pct(100),
#       table.font.size = px(12),
#       row_group.background.color = "#EBF5FB", 
#       row_group.font.weight = "bold",
#       summary_row.background.color = "#F4F6F7",
#       heading.align = "left",
#       column_labels.font.weight = "bold"
#     ) %>%
#     cols_width(
#       Subgroup ~ px(100),
#       Factor ~ px(300),
#       Goal ~ px(80),
#       Performance ~ px(80),
#       Score ~ px(60),
#       `Max Pts` ~ px(60)
#     ) %>%
#     cols_align(align = "center", columns = c(Score, `Max Pts`)) %>%
#     
#     # 1. CHANGED: Handle the "Empty Subgroup" logic to show N/A
#     text_transform(
#       locations = cells_body(columns = Subgroup),
#       fn = function(x) ifelse(x == "", "N/A", x)
#     )
#   
#   # 3. Save as PDF
#   tmp_pdf <- tempfile(fileext = ".pdf")
#   tmp_html <- tempfile(fileext = ".html")
#   
#   # gt_table %>%
#   #   as_raw_html() %>%
#   #   writeLines(tmp_html)
#   gt::gtsave(gt_table, tmp_html)
#   
#   # Use pagedown to "print" the HTML to PDF
#   pagedown::chrome_print(
#     input = tmp_html,
#     output = file,
#     extra_args = c("--no-sandbox", "--disable-gpu"),
#     timeout = 60
#   )
#   
#   return(file)
# }