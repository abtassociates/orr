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
      rs.version_id
    FROM rating_factors r
    INNER JOIN selected_rating_factors sr ON sr.rating_factor_id = r.rating_factor_id AND sr.coc_version_id = $1 AND selected = 1
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
      if(selected_project$funding_action %in% get_lookup_refid(c("Renew","Expand"), "funding_action"))
        get_lookup_refid("Renew", "funding_action")
      else
        get_lookup_refid("New", "funding_action"),
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

# ------------ Downloads -------------
get_all_rating_factors_and_scores <- function(coc_version_id, funding_action_id) {
  get_db_query(
    "SELECT p.project_id, p.project_name, fg.factor_group, fsg.factor_subgroup, r.piping_text, sr.goal, rs.performance, rs.rating_score, sr.max_point_value
    FROM rating_factors r
    INNER JOIN selected_rating_factors sr ON sr.rating_factor_id = r.rating_factor_id AND sr.coc_version_id = $1 AND selected = 1
    JOIN factor_groups fg ON r.factor_group = fg.factor_group_id
    LEFT JOIN factor_subgroups fsg ON r.factor_subgroup = fsg.factor_subgroup_id
    LEFT JOIN rating_scores rs ON rs.selected_rating_factor_id = sr.selected_rating_factor_id
    LEFT JOIN projects p ON sr.coc_version_id = p.coc_version_id AND p.funding_action = r.funding_action AND r.project_type = p.project_type
    WHERE r.funding_action = $2  AND p.project_id IS NOT NULL
    ORDER BY p.project_id, fg.factor_group_id, fsg.factor_subgroup_id", 
    params = list(coc_version_id, funding_action_id)
  )
}

get_rating_factors_by_pop_target_type <- function(coc_version_id, funding_action_id, project_type, target_population) {
  get_db_query(
    "SELECT fg.factor_group, fsg.factor_subgroup, r.piping_text, sr.goal, NULL AS performance, NULL AS rating_score, sr.max_point_value
    FROM rating_factors r
    INNER JOIN selected_rating_factors sr ON sr.rating_factor_id = r.rating_factor_id AND sr.coc_version_id = $1 AND sr.selected = 1
    JOIN factor_groups fg ON r.factor_group = fg.factor_group_id
    LEFT JOIN factor_subgroups fsg ON r.factor_subgroup = fsg.factor_subgroup_id
    WHERE r.funding_action = $2 AND r.project_type = $3 AND r.target_population = $4
    ORDER BY fg.factor_group_id, fsg.factor_subgroup_id", 
    params = list(coc_version_id, funding_action_id, project_type, target_population)
  )
}

