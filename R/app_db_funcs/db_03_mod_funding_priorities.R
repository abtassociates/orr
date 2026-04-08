get_coc_funding_priorities <- function(coc_version_id) {
  get_db_query(
    "SELECT * 
        FROM coc_funding_priorities 
        WHERE coc_version_id = $1",
    params = list(coc_version_id)
  )
}

get_coc_nofo_opportunities <- function(coc_version_id) {
  get_db_query(
    "SELECT c.coc_nofo_opportunity_id, c.bonus_type, s.selected, s.version_id
            FROM coc_nofo_opportunities c
            LEFT JOIN selected_coc_nofo_opportunities s ON c.coc_nofo_opportunity_id = s.coc_nofo_opportunity_id AND coc_version_id = $1", 
    params = list(coc_version_id)
  )
}

update_coc_nofo_opportunities_db <- function(p, updated_coc_nofo_opportunities) {
  save_to_db(
    p,
    "INSERT INTO selected_coc_nofo_opportunities (coc_version_id, coc_nofo_opportunity_id, selected, created_by)
    VALUES ($1, $2, $3, $4)
    ON CONFLICT (coc_version_id, coc_nofo_opportunity_id) DO UPDATE SET
      selected = EXCLUDED.selected,
      updated_by = EXCLUDED.created_by
    " |> add_optimistic_locking(),
    updated_coc_nofo_opportunities,
    "selected_coc_nofo_opportunities"
  )
}

update_coc_funding_priorities_db <- function(p, metric_name, updated_coc_funding_priorities) {
  save_to_db(
    p,
    glue::glue(
    "INSERT INTO coc_funding_priorities (coc_version_id, project_type, target_population, population_group, {metric_name}, created_by)
    VALUES ($1, $2, $3, $4, $5, $6)
    ON CONFLICT (coc_version_id, project_type, target_population, population_group) DO UPDATE SET 
      {metric_name} = EXCLUDED.{metric_name},
      updated_by = EXCLUDED.created_by, -- Use the 'created_by' value from the attempted insert
    ") |> add_optimistic_locking(),
    updated_coc_funding_priorities,
    "coc_funding_priorities"
  )
}

update_dv_ard <- function(p, params) {
  # TODO: NEED TO ADD OPTIMISITC LOCKING
  tryCatch({
    DBI::dbExecute(
      p,
      "UPDATE coc_versions 
      SET 
        dv_ard = $1, 
        updated_by = $2, 
        date_updated = CURRENT_TIMESTAMP,
        version_id = version_id + 1
      WHERE coc_version_id = $3",
      paramify(params)
    )
    message("Saved DV ARD!")
  }, error = function(e) {
    # If an error occurs, do NOT reset the flag, so it will try again.
    # Notify the user of the failure.
    showNotification(glue::glue("Error saving dv_ard to coc_versions table: {e$message}"), type = "error", duration = 10)
    log_error(paste0("Updating dv_ard:", e$message))
    stop(e) # rethrow error so the transaction can catch it and roll back
  })
}