# Get the rating factor info to construct UI
get_rating_factors_and_scores <- function(coc_version_id, selected_project) {
  target_population <- ifelse(
    is.na(selected_project$target_population) || 
      get_lookup_label(selected_project$target_population, 'target_population') == 'NA',
    get_lookup_refid('General', 'target_population'),
    selected_project$target_population
  )
  
  get_db_query(
    "SELECT r.rating_factor_id, 
      r.rating_factor_text, 
      CASE WHEN r.rating_factor_text_short IS NOT NULL THEN r.rating_factor_text_short ELSE r.rating_factor_text END AS rating_factor_text_short, 
      r.piping_text, r.project_type, r.target_population, sr.selected_rating_factor_id, 
      fg.factor_group, fsg.factor_subgroup, 
      sr.goal, sr.max_point_value,
      rs.rating_score, rs.performance, rs.project_id,
      rs.date_updated
    FROM rating_factors r
    INNER JOIN selected_rating_factors sr ON sr.rating_factor_id = r.rating_factor_id AND sr.coc_version_id = $1
    JOIN factor_groups fg ON r.factor_group = fg.factor_group_id
    LEFT JOIN factor_subgroups fsg ON r.factor_subgroup = fsg.factor_subgroup_id
    LEFT JOIN rating_scores rs ON rs.selected_rating_factor_id = sr.selected_rating_factor_id AND (rs.project_id = $2 OR rs.project_id IS NULL)
    WHERE 
      r.funding_action = $3 AND
      (r.project_type = $4 OR r.project_type IS NULL) AND
      (r.target_population = $5 OR r.target_population IS NULL)
    ", 
    params = list(
      coc_version_id,
      selected_project$project_id,
      selected_project$funding_action,
      selected_project$project_type,
      target_population
    )
  )
}


update_rating_scores_db <- function(p, updated_rating_scores) {
  save_to_db(
    p,
    "INSERT INTO rating_scores (project_id, selected_rating_factor_id, rating_score, performance, created_by)
    VALUES ($1, $2, $3, $4, $5)
    ON CONFLICT (project_id, selected_rating_factor_id) DO UPDATE SET
      rating_score = EXCLUDED.rating_score,
      performance  = EXCLUDED.performance,
      updated_by   = EXCLUDED.created_by
    "  |> add_optimistic_locking(),
    updated_rating_scores,
    "rating_scores"
  )
}

update_rating_score_project_evaluation_db <- function(p, updated_project_evaluation) {
  save_to_db(
    p,
    "INSERT INTO project_evaluations (project_id, method, weighted_score, created_by)
    VALUES ($1, 'in_app', $2, $3)
    ON CONFLICT (project_id) DO UPDATE SET 
      method = EXCLUDED.method,
      weighted_score = EXCLUDED.weighted_score,
      updated_by = EXCLUDED.created_by
    " |> add_optimistic_locking(),
    updated_project_evaluation,
    "project_evaluations"
  )
}
