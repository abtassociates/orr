log_error <- function(msg) {
  if(IN_DEV_MODE) {
    print(msg)
  }
  logger::log_error(msg)
}

table_suffixes <- c(
  "_rows_selected",
  "_rows_current",
  "_rows_all",
  "_state"
)
table_names <- c(
  "dashboard-coc_selection-coc_versions_dt",
  "inventory-projects_table",
  "dashboard-requests-requests_dt",
  "funding_priorities-priorities_table",
  "ranking-ui_ranked_list",
  "ranking-ui_excluded_list",
  "rating-alternative-alternative_rating_table"
)


inputs_to_exclude <- outer(table_names, table_suffixes, paste, sep = "")