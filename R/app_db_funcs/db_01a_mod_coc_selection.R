get_coc_projects <- function(coc_version_id) {
  get_db_query(
    "SELECT * FROM projects WHERE coc_version_id = $1", 
    params = coc_version_id
  )
}

generate_data_for_new_coc_version <- function(p, coc_version_id) {
  generate_selected_thresholds_for_coc(p, coc_version_id)
  generate_selected_rating_factors_for_coc(p, coc_version_id)
  generate_selected_coc_nofo_opportunities(p, coc_version_id)
  generate_coc_funding_priorities(p, coc_version_id)
}

generate_selected_thresholds_for_coc <- function(p, coc_version_id) {
  thresholds <- DBI::dbGetQuery(
    p, 
    "SELECT threshold_id, $1 AS coc_version_id 
    FROM thresholds
    WHERE coc_version_id IS NULL",
    params = list(coc_version_id)
  )
  
  DBI::dbExecute(
    p,
    glue::glue("INSERT INTO selected_thresholds (threshold_id, selected, coc_version_id, created_by, updated_by)
    VALUES ($1, TRUE, $2, '{SERVICE_ACCOUNT}', '{SERVICE_ACCOUNT}')
    ON CONFLICT (coc_version_id, threshold_id) DO NOTHING;"),
    params = paramify(thresholds)
  )
}

generate_selected_rating_factors_for_coc <- function(p, coc_version_id) {
  rating_factors <- DBI::dbGetQuery(
    p, 
    "SELECT rating_factor_id, $1 AS coc_version_id, goal, max_point_value
    FROM rating_factors
    WHERE coc_version_id IS NULL",
    params = list(coc_version_id)
  )

  DBI::dbExecute(
    p,
    glue::glue("INSERT INTO selected_rating_factors (rating_factor_id, selected, coc_version_id, goal, max_point_value, created_by, updated_by)
    VALUES ($1, FALSE, $2, $3, $4, '{SERVICE_ACCOUNT}', '{SERVICE_ACCOUNT}')
    ON CONFLICT (coc_version_id, rating_factor_id) DO NOTHING;"),
    params = paramify(rating_factors)
  )
}

generate_selected_coc_nofo_opportunities <- function(p, coc_version_id) {
  coc_nofo_opportunities <- DBI::dbGetQuery(
    p, 
    "SELECT coc_nofo_opportunity_id, $1 AS coc_version_id 
    FROM coc_nofo_opportunities",
    params = list(coc_version_id)
  )
  
  DBI::dbExecute(
    p,
    glue::glue("INSERT INTO selected_coc_nofo_opportunities (coc_nofo_opportunity_id, selected, coc_version_id, created_by, updated_by)
    VALUES ($1, TRUE, $2, '{SERVICE_ACCOUNT}', '{SERVICE_ACCOUNT}')
    ON CONFLICT (coc_version_id, coc_nofo_opportunity_id) DO NOTHING;"),
    params = paramify(coc_nofo_opportunities)
  )
}

generate_coc_funding_priorities <- function(p, coc_version_id) {
  cfp <- expand.grid(
    project_type = get_labelled_lookups("project_type")[c("RRH","PSH","TH","TH+RRH")],
    target_population = get_labelled_lookups("target_population", lookup_col = "value_long"),
    population_group = get_labelled_lookups("population_group", lookup_col = "value_long")
  ) |>
    fsubset(!target_population %in% get_lookup_refid(c("Not Applicable", "Human Immunodeficiency Virus"), "target_population", lookup_col = "value_long"))
  
  # priorities
  data <- cfp |>
    fmutate(coc_version_id = coc_version_id)
  
  DBI::dbExecute(
    p,
    glue::glue("INSERT INTO coc_funding_priorities (project_type, target_population, population_group, coc_version_id, created_by, updated_by)
    VALUES ($1, $2, $3, $4, '{SERVICE_ACCOUNT}', '{SERVICE_ACCOUNT}')
    ON CONFLICT (coc_version_id, project_type, target_population, population_group) DO NOTHING;"),
    params = paramify(data)
  )
}
append_version_request <- function(selected_version, user_coc) {
  request_row <- data.table(
    #coc_request_id = 1 + (get_db_tbl('coc_version_requests') |> fnrow()),
    coc_version_id = selected_version$coc_version_id,
    request_status = get_lookup_refid('Sent','request_status'),
    reason_for_rejection = NA
  ) |>
    add_user_stamp(user_coc, is_new = TRUE)
  
  # Add row to requests table
  db_append("coc_version_requests", request_row)
}

update_coc_version <- function(params) {
  db_execute( 
    "UPDATE coc_versions 
      SET coc_status = $1, 
          date_updated = CURRENT_TIMESTAMP, 
          updated_by = $2,
          version_id = version_id + 1
      --WHERE coc_version_id = $3 AND version_id = $4
      WHERE coc_version_id = $3",
    params = params
  )
}

delete_coc_version <- function(coc_version_id) {
  db_execute("DELETE FROM coc_versions WHERE coc_version_id = $1", params = coc_version_id)
}

get_coc_status <- function(coc_version_id) {
  get_db_query(
    "SELECT coc_status
    FROM coc_versions
    WHERE coc_version_id = $1",
    params = coc_version_id
  )
}