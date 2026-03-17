get_coc_projects <- function(coc_version_id) {
  get_db_query(
    "SELECT * FROM projects WHERE coc_version_id = $1", 
    params = coc_version_id
  )
}

generate_data_for_new_coc_version <- function(coc_version_id) {
  generate_selected_thresholds_for_coc(coc_version_id)
  generate_selected_rating_factors_for_coc(coc_version_id)
  generate_selected_coc_nofo_opportunities(coc_version_id)
}

generate_selected_thresholds_for_coc <- function(coc_version_id) {
  thresholds <- get_db_query("SELECT threshold_id FROM thresholds") |>
    fmutate(coc_version_id = coc_version_id)
  
  DBI::dbExecute(
    DB_POOL,
    "INSERT INTO selected_thresholds (threshold_id, selected, coc_version_id)
    VALUES ($1, TRUE, $2)
    ON CONFLICT (coc_version_id, threshold_id) DO NOTHING;",
    params = paramify(thresholds)
  )
}

generate_selected_rating_factors_for_coc <- function(coc_version_id) {
  rating_factors <- get_db_query("SELECT rating_factor_id FROM rating_factors") |>
    fmutate(coc_version_id = coc_version_id)
  
  DBI::dbExecute(
    DB_POOL,
    "INSERT INTO selected_rating_factors (rating_factor_id, selected, coc_version_id)
    VALUES ($1, TRUE, $2)
    ON CONFLICT (coc_version_id, rating_factor_id) DO NOTHING;",
    params = paramify(rating_factors)
  )
}

generate_selected_coc_nofo_opportunities <- function(coc_version_id) {
  coc_nofo_opportunities <- get_db_query("SELECT coc_nofo_opportunity_id FROM coc_nofo_opportunities") |>
    fmutate(coc_version_id = coc_version_id)
  
  DBI::dbExecute(
    DB_POOL,
    "INSERT INTO selected_coc_nofo_opportunities (coc_nofo_opportunity_id, selected, coc_version_id)
    VALUES ($1, TRUE, $2)
    ON CONFLICT (coc_version_id, coc_nofo_opportunity_id) DO NOTHING;",
    params = paramify(coc_nofo_opportunities)
  )
}