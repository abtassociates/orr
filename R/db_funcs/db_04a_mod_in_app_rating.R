# Used to pull project-evaluation info for a given project
get_project_evaluation <- function(coc_version_id, project_id) {
  get_db_query(
    "SELECT p.coc_version_id, pe.project_id, method, met_hud_thresholds, met_coc_thresholds, pe.date_updated 
        FROM project_evaluations pe
        LEFT JOIN projects p ON pe.project_id = p.project_id
        WHERE p.coc_version_id = $1 and pe.project_id = $2",
    params = list(coc_version_id, project_id)
  )
}
