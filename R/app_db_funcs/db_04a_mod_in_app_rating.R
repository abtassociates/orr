# Used to pull project-evaluation info for a given project
get_project_evaluation <- function(coc_version_id, project_id = NULL) {
  get_db_query(
    "SELECT p.coc_version_id, pe.project_id, method, met_hud_thresholds, met_coc_thresholds, weighted_score, pe.version_id, pe.threshold_complete, pe.rating_complete
    FROM project_evaluations pe
    LEFT JOIN projects p ON pe.project_id = p.project_id
    WHERE p.coc_version_id = $1 AND (pe.project_id = $2 OR $2 IS NULL)",
    params = list(coc_version_id, project_id)
  )
}

get_projects_by_funding_action <- function(coc_version_id, funding_action_ids) {
  get_db_query(glue::glue_sql(
    "SELECT project_id, organization_name, project_name, project_type, target_population, funding_action
    FROM projects 
    WHERE 
      coc_version_id = {coc_version_id} AND 
      funding_action IN ({funding_action_ids*}) AND 
      mckinneyvento = TRUE
    ORDER BY project_name",
    .con=get_db_pool()
  )) |>
    join(
      stack(RATABLE_PROJECT_TYPES) |> 
        frename("project_type" = values, "funding_action" = ind) |>
        fmutate(
          funding_action = get_lookup_refid(funding_action, "funding_action"),
          project_type = get_lookup_refid(project_type, "project_type")
        ),
      on = c("funding_action","project_type"),
      how = "inner"
    )
}

get_project_rating_completion <- function(project_ids) {
  get_db_query(glue::glue_sql(
    "SELECT project_id 
    FROM project_evaluations 
    WHERE project_id IN ({project_ids*}) AND rating_complete AND threshold_complete",
    .con=get_db_pool()
  ))
}