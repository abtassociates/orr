library(stringr)
HDX_HIC_DATE <- as.Date('2025-07-25')

LOOKUPS <- get_db_tbl("lookups")

cocs <- get_db_tbl("cocs")

HUD_ARD_REPORT <- get_db_tbl("hud_ard_report") |>
  fmutate(
    estimated = as.numeric(estimated),
    tier_1 = as.numeric(tier_1),
    coc_bonus = as.numeric(coc_bonus),
    dv_bonus = as.numeric(dv_bonus)
  )

MAIN_PROJECT_TYPES <- c("PSH", "RRH", "TH", "TH+RRH")

COC_NOFO_OPPORTUNITIES <- get_db_tbl("coc_nofo_opportunities") |>
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
  "ranking",
  "account"
)
TABS_AFTER_COC_SELECTION <- c(
  "inventory",
  "funding_priorities",
  "rating",
  "ranking"
)

TABS_AFTER_PROJECTS_EXIST <- c(
  "rating",
  "ranking"
)

RATABLE_PROJECT_TYPES <- list(
  "New" = c("RRH","PSH", "TH+RRH", "TH"),
  "Renew" = c("RRH", "PSH", "TH","TH+RRH"),
  "Expand" = c("RRH","PSH", "TH")
)

# These project types are neither rated nor ranked
PROJECT_TYPES_TO_IGNORE <- c("ES","OPH","DEM")

RANKED_BUT_NOT_RATED_PROJECTS <- c("SSO-CE", "HMIS Project", "HMIS", "SSO-Host Homes", "SH")

HUD_THRESHOLD_REQUIREMENTS <- get_db_tbl("thresholds") |> 
  fsubset(type == "HUD")


POP_GRP_TOGGLES <- expand.grid(
  pop = get_labelled_lookups("target_population", lookup_col = "value_long"),
  grp = get_labelled_lookups("population_group", lookup_col = "value_long")
) |>
  qDT() |>
  fmutate(
    pop_txt = gsub("Domestic Violence", "DV", names(pop)),
    grp_txt = names(grp)
  ) |>
  fsubset(!pop_txt %in% c("Not Applicable", "Human Immunodeficiency Virus")) |>
  setorder(-grp, pop) |>
  fmutate(
    full_text = fcase(
      pop_txt == "Youth" & grp_txt == "Families", "Parenting Youth",
      pop_txt == "Youth" & grp_txt == "Individuals", "Single Youth",
      default = paste(pop_txt, grp_txt)
    )
  )