get_all_thresholds_to_enter <- function(coc_version_id, project_id) {
  get_db_query(
    "SELECT st.selected_threshold_id, st.selected, t.type, t.threshold_text, t.threshold_id, te.met_threshold, te.threshold_entry_id, te.version_id
    FROM thresholds t
    LEFT JOIN selected_thresholds st ON st.threshold_id = t.threshold_id 
      AND st.coc_version_id = $1
    LEFT JOIN threshold_entries te ON te.threshold_id = t.threshold_id 
      AND (te.project_id = $2 OR te.project_id IS NULL) 
    WHERE st.selected",
    params = list(coc_version_id, project_id)
  )
}


update_threshold_entries_db <- function(p, updated_thresholds) {
  save_to_db(
    p, 
    "INSERT INTO threshold_entries (project_id, threshold_id, met_threshold, created_by)
    VALUES ($1, $2, $3, $4)
    ON CONFLICT (project_id, threshold_id) DO UPDATE SET 
      met_threshold = EXCLUDED.met_threshold, 
      updated_by = EXCLUDED.created_by
    " |> add_optimistic_locking(),
    updated_thresholds,
    "threshold_entries"
  )
}

update_threshold_project_evaluation_db <- function(p, updated_project_evaluation) {
  save_to_db(
    p,
    "INSERT INTO project_evaluations (project_id, method, met_hud_thresholds, met_coc_thresholds, created_by)
    VALUES ($1, 'in_app', $2, $3, $4)
    ON CONFLICT (project_id) DO UPDATE SET 
      method = EXCLUDED.method,
      met_hud_thresholds = EXCLUDED.met_hud_thresholds, 
      met_coc_thresholds = EXCLUDED.met_coc_thresholds,
      updated_by = EXCLUDED.created_by
    " |> add_optimistic_locking(),
    updated_project_evaluation,
    "project_evaluation"
  )
}


update_threshold_complete <- function(p, updated_threshold_complete) {
  save_to_db(
    p,
    "UPDATE project_evaluations 
    SET 
      threshold_complete = $1, 
      updated_by = $2,
      date_updated = CURRENT_TIMESTAMP,
      version_id = version_id + 1
    WHERE project_id = $3 AND version_id = $4",
    updated_threshold_complete,
    "project_evaluations"
  ) 
}