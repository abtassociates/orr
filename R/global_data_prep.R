HDX_HIC_DATE <- as.Date('2025-07-25')

LOOKUPS <- get_db_tbl("lookups")

cocs <- get_db_tbl("cocs")
COC_VERSION_USERS <- get_db_query(
  "SELECT v.*, u.username, u.coc_version_role
  FROM coc_versions v
  LEFT JOIN coc_version_users u
  ON v.coc_version_id = u.coc_version_id"
)
HUD_ARD_REPORT <- get_db_tbl("hud_ard_report")
MAIN_PROJECT_TYPES <- c("PSH", "RRH", "TH", "TH+RRH")

COC_NOFO_OPPORTUNITIES <- get_db_tbl("coc_nofo_opportunities") %>%
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

TABS_AFTER_LOGIN <- c(
  "dashboard",
  "inventory",
  "funding_priorities",
  "rating",
  "account"
)
TABS_AFTER_COC_SELECTION <- c(
  "inventory",
  "funding_priorities",
  "rating"
)

TABS_AFTER_PROJECTS_EXIST <- c(
  "rating"
)
