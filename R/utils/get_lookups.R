lookup_tables <- c(
  "project_types",
  "target_populations",
  "population_groups",
  "funding_actions",
  "factor_groups",
  "factor_subgroups",
  "bonus_types",
  "priorities",
  "coc_instance_roles",
  "request_statuses",
  "request_rejection_reasons",
  "coc_status"
)

lookups <- setNames(
  lapply(lookup_tables, function(t) {
    get_db_query(glue::glue("SELECT * FROM {t}"))
  }),
  lookup_tables
)