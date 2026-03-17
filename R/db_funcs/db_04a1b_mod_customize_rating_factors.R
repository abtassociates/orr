get_all_coc_factors <- function(funding_action_id, coc_version_id) {
  get_db_query(
    "SELECT rf.rating_factor_id, rf.funding_action, srf.selected, rf.project_type, rf.target_population, rf.rating_factor_text, COALESCE(srf.goal, rf.goal) AS goal,
           COALESCE(srf.max_point_value, rf.max_point_value) AS max_point_value, fg.factor_group, fsg.factor_subgroup, srf.date_updated
    FROM rating_factors rf
    JOIN factor_groups fg ON rf.factor_group = fg.factor_group_id
    LEFT JOIN factor_subgroups fsg ON rf.factor_subgroup = fsg.factor_subgroup_id
    LEFT JOIN selected_rating_factors srf ON rf.rating_factor_id = srf.rating_factor_id AND srf.coc_version_id = $1
    WHERE rf.funding_action = $2 AND 
      (rf.coc_version_id = $1 OR rf.coc_version_id IS NULL)",
    params = list(coc_version_id, funding_action_id)
  )
}

get_other_factor_group_id <- function(funding_action_id) {
  get_db_query(
    "SELECT factor_group_id 
    FROM factor_groups
    WHERE factor_group = 'Other and Local Criteria' AND funding_action = $1", 
    params = funding_action_id
  )$factor_group_id
}

get_subgroups_by_funding_action <- function(funding_action_id) {
  get_db_query(
    "SELECT sg.factor_subgroup, fg.factor_group
    FROM factor_subgroups sg
    RIGHT JOIN factor_groups fg ON fg.factor_group_id = sg.factor_group
    WHERE fg.funding_action = $1
    ", 
    params = funding_action_id
  )
}

insert_custom_factor_to_db <- function(p, custom_factor_data) {
  save_to_db(
    p, 
    "INSERT INTO rating_factors (funding_action, coc_version_id, project_type, target_population, rating_factor_text, factor_group, goal, max_point_value, created_by)
          VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
          ON CONFLICT (coc_version_id, COALESCE(project_type, -1), COALESCE(target_population, -1), rating_factor_text) DO NOTHING
        RETURNING rating_factor_id;",
    custom_factor_data,
    "rating_factors"
  )
}

update_selected_rating_factors_db <- function(p, updated_selected_rating_factors) {
  save_to_db(
    p, 
    "INSERT INTO selected_rating_factors (rating_factor_id, coc_version_id, selected, goal, max_point_value, created_by)
          VALUES ($1, $2, $3, $4, $5, $6)
          ON CONFLICT (coc_version_id, rating_factor_id) DO UPDATE SET
            selected = EXCLUDED.selected,
            goal = EXCLUDED.goal,
            max_point_value = EXCLUDED.max_point_value,
            updated_by = EXCLUDED.created_by
        " |> add_optimistic_locking(),
    updated_selected_rating_factors,
    "selected_rating_factors"
  )
}
