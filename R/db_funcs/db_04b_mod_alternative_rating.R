## Get project evaluation info for all projects to be entered
get_alternative_rating <- function(coc_version_id) {
  get_db_query(
    "SELECT 
      p.project_id, 
      p.organization_name, 
      p.project_name, 
      p.grant_number, 
      p.funding_action, 
      p.project_type, 
      p.target_population, 
      pe.met_hud_thresholds,
      pe.met_coc_thresholds,
      pe.weighted_score,
      pe.date_updated
    FROM projects p
    LEFT JOIN project_evaluations pe ON p.project_id = pe.project_id
    LEFT JOIN lookups l ON p.funding_action = l.reference_id
    WHERE p.coc_version_id = $1 AND p.funding_action IS NOT NULL AND l.value <> 'Ignore'",
    params = list(coc_version_id)
  )
}


update_project_evaluations_db <- function(p, updated_project_evaluation) {
  save_to_db(
    p,
    "INSERT INTO project_evaluations (project_id, method, met_hud_thresholds, met_coc_thresholds, created_by)
    VALUES ($1, 'outside', $2, $3, $4)
    ON CONFLICT (project_id) DO UPDATE SET
      method = EXCLUDED.method,
      met_hud_thresholds = EXCLUDED.met_hud_thresholds,
      met_coc_thresholds = EXCLUDED.met_coc_thresholds,
      updated_by   = EXCLUDED.created_by
    " |> add_optimistic_locking(),
    updated_project_evaluation,
    "project_evaluations"
  )
}
