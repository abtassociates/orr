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

get_dv_ard <- function(coc_version_id) {
  get_db_query(
    "SELECT dv_ard, version_id 
    FROM coc_versions 
    WHERE coc_version_id = $1", 
    params = coc_version_id
  ) |>
    fmutate(dv_ard = as.numeric(dv_ard))
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

get_coc_hud_ard_data <- function(user_coc) {
  dv_ard_db <- get_dv_ard(user_coc$coc_version_id)
  
  HUD_ARD_REPORT[coc == user_coc$coc] |>
    frename("total_ard" = estimated) |>
    fmutate(
      adjusted_ard = round(tier_1/0.9, 0),
      tier_2 = adjusted_ard * 0.1 + fcoalesce(coc_bonus, 0) + fcoalesce(dv_bonus, 0),
      # yhdp_ard = estimated - min(adjusted_ard, estimated),
      dv_ard = fcoalesce(dv_ard_db$dv_ard[1], 0),
      version_id = dv_ard_db$version_id[1],
      exceeds = Inf
    )
}

update_dv_ard <- function(p, updated_dv_ard) {
  save_to_db(
    p, 
    "UPDATE coc_versions 
      SET 
        dv_ard = $1, 
        updated_by = $2, 
        date_updated = CURRENT_TIMESTAMP,
        version_id = version_id + 1
      WHERE coc_version_id = $3 AND version_id = $4",
    updated_dv_ard,
    "coc_versions"
  )
}