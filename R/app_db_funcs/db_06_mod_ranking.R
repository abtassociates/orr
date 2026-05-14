get_projects_to_rank <- function(coc_version_id) {
  get_db_query(
    "SELECT DISTINCT
      r.tier,
      r.rank,
      pe.weighted_score,
      no.bonus_type,
      p.funding_action,
      p.grant_number,
      p.project_type,
      p.target_population,
      p.organization_name,
      p.project_name,
      p.coc_funding_requested,
      r.coc_funding_recommendation,
      p.all_fam_beds,
      p.dv_fam_beds,
      p.ch_fam_beds,
      p.vet_fam_beds,
      p.par_youth_beds,
      p.all_ind_beds,
      p.dv_ind_beds,
      p.total_ch_ind_beds,
      p.vet_ind_beds,
      p.single_youth_beds,
      p.is_dedicated_ch_fam,
      p.is_dedicated_ch_ind,
      p.is_dedicated_dv,
      pe.met_hud_thresholds,
      pe.met_coc_thresholds,
      no.population_group AS no_pop_grp,
      p.dv_renewal,
      p.project_id,
      r.version_id,
      pe.rating_complete AND pe.threshold_complete AS rating_complete
      
      FROM projects p
      LEFT JOIN ranking r ON p.project_id = r.project_id AND r.coc_version_id = p.coc_version_id
      LEFT JOIN project_evaluations pe ON p.project_id = pe.project_id
      LEFT JOIN selected_coc_nofo_opportunities sno ON sno.coc_version_id = p.coc_version_id
      LEFT JOIN coc_nofo_opportunities no ON no.coc_nofo_opportunity_id = sno.coc_nofo_opportunity_id AND no.funding_action = p.funding_action AND no.project_type = p.project_type AND no.target_population = p.target_population
      WHERE p.coc_version_id = $1 AND p.funding_action <> $2", 
    params = list(coc_version_id, get_lookup_refid("Ignore", "funding_action"))
  ) %>%
    fmutate(
      met_hud_thresholds = fcoalesce(as.logical(met_hud_thresholds), FALSE),
      met_coc_thresholds = fcoalesce(as.logical(met_coc_thresholds), FALSE),
      funding_action = convert_to_factor(., "funding_action"),
      project_type = convert_to_factor(., "project_type"),
      target_population = convert_to_factor(., "target_population"),
      dv_renewal = factor_yesno(dv_renewal),
      is_dedicated_ch_fam = factor_yesno(is_dedicated_ch_fam),
      is_dedicated_ch_ind = factor_yesno(is_dedicated_ch_ind),
      is_dedicated_dv = factor_yesno(is_dedicated_dv)
    )
}

get_ceilings_priorities <- function(coc_version_id) {
  get_db_query(
    "SELECT 
      project_type, 
      target_population, 
      population_group, 
      priority, 
      beds AS ceil_beds, 
      funding AS ceil_fund 
    FROM coc_funding_priorities 
    WHERE coc_version_id = $1",
    params = list(coc_version_id)
  ) %>%
    fmutate(
      project_type = convert_to_factor(., "project_type"),
      target_population = convert_to_factor(., "target_population"),
      population_group = convert_to_factor(., "population_group")
    )
}

update_ranking_db <- function(p, updated_rankings) {
  save_to_db(
    p,
    "INSERT INTO ranking (project_id, coc_version_id, rank, tier, coc_funding_recommendation, created_by)
     VALUES ($1, $2, $3, $4, $5, $6)
     ON CONFLICT (project_id, coc_version_id) DO UPDATE SET
       rank = EXCLUDED.rank,
       tier = EXCLUDED.tier,
       coc_funding_recommendation = EXCLUDED.coc_funding_recommendation,
       updated_by = EXCLUDED.created_by
    " |> add_optimistic_locking(),
    updated_rankings,
    "ranking"
  )
}