HDX_HIC_DATE <- as.Date('2025-07-25')

lookups <- get_db_tbl("lookups")
users <- get_db_tbl("users")
cocs <- get_db_tbl("cocs")
coc_version_users <- get_db_query(
  "SELECT v.*, u.username, u.coc_version_role
  FROM coc_versions v
  LEFT JOIN coc_version_users u
  ON v.coc_version_id = u.coc_version_id"
)
hud_ard_report <- get_db_tbl("hud_ard_report")
main_project_types <- c("PSH", "RRH", "TH", "TH+RRH")

coc_nofo_opportunities <- get_db_tbl("coc_nofo_opportunities") %>%
  fmutate(
    funding_action = get_lookup_label(funding_action, "funding_action"),
    project_type = get_lookup_label(project_type, "project_type"),
    target_population = get_lookup_label(target_population, "target_population"),
    population_group = get_lookup_label(population_group, "population_group", "value_long"),
    bonus_type = get_lookup_label(bonus_type, "bonus_type"),
    
    full_text = str_trim(paste0(
      funding_action, " ",
      project_type, " ",
      fifelse(!is.na(target_population) | !is.na(population_group), "for ", ""), 
      fcoalesce(fifelse(target_population == "Chronically Homeless", "100% Dedicated PLUS or Chronically Homeless", paste0(target_population, " ")), ""),
      fcoalesce(population_group, "")
    ))
  )

SERVICE_ACCOUNT <- 'orr_service@abtglobal.com'


TABS <- c(
  "about",
  "dashboard",
  "inventory",
  "rating_criteria",
  "renewal_rating",
  "new_rating",
  "alternative_rating",
  "funding_priorities",
  "final_review",
  "ranking",
  "account"
)
TABS_TO_SHOW <- c(
  "about",
  "dashboard",
  "inventory",
  "funding_priorities",
  "account"
)