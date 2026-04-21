build_report <- function(data, project_name, total, max_pts, file) {
  
  # Handle NULL project name for the Blank Template
  p_name_display <- ifelse(is.null(project_name), "Blank Template", project_name)
  
  # 1. Prepare Data
  table_data <- data |>
    fselect(
      factor_group,
      Subgroup = factor_subgroup,
      Factor = piping_text,
      Goal = goal,
      Performance = performance,
      Score = rating_score,
      `Max Pts` = max_point_value
    ) %>%
    mutate(across(everything(), ~ifelse(is.na(.) | . == "NA", "", as.character(.))))
  
  # 2. Build the gt Table
  gt_table <- table_data %>%
    gt(groupname_col = "factor_group") %>%
    tab_header(
      title = md(paste0("<div style='background-color:#4A90E2; color:white; padding:10px; border-radius:5px;'>",
                        "<span style='font-size:24px; font-weight:bold;'>Rating Report Card</span>",
                        "<span style='float:right;'>", total, " / ", max_pts, " Points</span>",
                        "</div>")),
      subtitle = md(paste0("<div style='padding-top:10px;'>Project: **", p_name_display, "**</div>"))
    ) %>%
    summary_rows(
      groups = TRUE,
      columns = c(Score, `Max Pts`),
      fns = list(label = "Subtotal", fn = ~sum(as.numeric(.), na.rm = TRUE)),
      side = "top"
    ) %>%
    tab_options(
      table.width = pct(100),
      table.font.size = px(12),
      row_group.background.color = "#EBF5FB", 
      row_group.font.weight = "bold",
      summary_row.background.color = "#F4F6F7",
      heading.align = "left",
      column_labels.font.weight = "bold"
    ) %>%
    cols_width(
      Subgroup ~ px(100),
      Factor ~ px(300),
      Goal ~ px(80),
      Performance ~ px(80),
      Score ~ px(60),
      `Max Pts` ~ px(60)
    ) %>%
    cols_align(align = "center", columns = c(Score, `Max Pts`)) %>%
    
    # 1. CHANGED: Handle the "Empty Subgroup" logic to show N/A
    text_transform(
      locations = cells_body(columns = Subgroup),
      fn = function(x) ifelse(x == "", "N/A", x)
    )
  
  # 3. Save as PDF
  tmp_pdf <- tempfile(fileext = ".pdf")
  tmp_html <- tempfile(fileext = ".html")
  
  # gt_table %>%
  #   as_raw_html() %>%
  #   writeLines(tmp_html)
  gt::gtsave(gt_table, tmp_html)
  
  # Use pagedown to "print" the HTML to PDF
  pagedown::chrome_print(
    input = tmp_html,
    output = file,
    extra_args = c("--no-sandbox", "--disable-gpu"),
    timeout = 60
  )
  
  return(file)
}

generate_pdf_job <- function(data, project_name, total, max_pts, outfile) {
  library(dplyr)
  library(gt)
  library(pagedown)
  
  build_report(
    data = data,
    project_name = project_name,
    total = total,
    max_pts = max_pts,
    file = outfile
  )
  
  outfile
}