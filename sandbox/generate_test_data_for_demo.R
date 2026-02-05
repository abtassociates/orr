library(magrittr)
if(IN_DEV_MODE) DBI::dbExecute(DB_CON, "PRAGMA foreign_keys = OFF;")
dbExecute(DB_CON, "DELETE FROM coc_version_users WHERE coc_version_id > 4")
dbExecute(DB_CON, "DELETE FROM coc_versions WHERE coc_version_id > 4")
dbExecute(DB_CON, "DELETE FROM coc_version_requests")
dbExecute(DB_CON, "DELETE FROM projects WHERE coc_version_id > 4")
if(IN_DEV_MODE) DBI::dbExecute(DB_CON, "PRAGMA foreign_keys = ON;")

USERS <- get_db_tbl("users")
main_user <- toString(USERS[1, 1]) # alex.silverman@abtglobal.com
second_user <- toString(USERS[3, 1]) # thomas.brittain@abtglobal.com

coc_versions <- data.table(
  coc_version_id = 10:12,
  coc_version_name = c(
    'AK-500 Main Version',
    'AK-500 Alternative Version',
    'AK-501 Main Version'
  ),
  coc = c('AK-500', 'AK-500', 'AK-501'),
  coc_status = c(9, 8, 9),  # In Progress, Not Started, In Progress
  created_by = c(
    main_user,
    second_user,
    main_user
  ),
  date_created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  date_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  updated_by = main_user
)

# CoC Version Users (many-to-many relationship)
coc_version_users <- data.table(
  coc_version_user_id = 5:8,
  coc_version_id = c(10, 11, 11, 12),
  username = c(
    main_user,
    second_user,
    main_user,
    main_user
  ),
  coc_version_role = c(5, 5, 7, 5),  # Owner, Owner, Editor, Owner
  created_by = c(
    main_user,
    second_user,
    second_user,
    main_user
  ),
  date_created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  date_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  updated_by = main_user
)

# CoC Version Requests (requests to versions where you are Owner)
coc_version_requests <- data.table(
  coc_request_id = 1:2,
  coc_version_id = c(10, 12),  # AK-500 Main and AK-501 Main (where you are Owner)
  request_status = c(1, 3),  # Sent, Approved
  reason_for_rejection = NA_integer_,
  created_by = c(
    second_user,
    second_user
  ),
  date_created = format(Sys.time() - c(86400, 43200), "%Y-%m-%d %H:%M:%S"),  # 1 day ago, 12 hours ago
  date_updated = format(Sys.time() - c(86400, 43200), "%Y-%m-%d %H:%M:%S"),
  updated_by = main_user
)

get_hic_data <- function(coc, coc_version_id) {
  bed_field_mapping <- c(
    all_fam_beds = "beds_hh_w_children", 
    ch_fam_beds = "ch_beds_hh_w_children",
    vet_fam_beds = "veteran_beds_hh_w_children", 
    par_youth_beds = "youth_beds_hh_w_children",
    vet_ind_beds = "veteran_beds_hh_wo_children",
    single_youth_beds = "youth_beds_hh_wo_children"
  )
  
  coc_data <- get_db_tbl("all_hic_data") |>
    fsubset(hudnum == coc) 
  
  project_data <- coc_data %>%
    fmutate(
      mckinneyvento = factor_yesno(rowSums(gvr(., "mckinneyvento"), na.rm = TRUE) > 0),
      mckinneyventoyhdp = factor_yesno(mckinneyventoyhdp),
      dv_renewal = factor_yesno(NA),
      grant_number = as.character(NA), 
      coc_amount_awarded_last_year = as.numeric(NA),
      coc_amount_expended_last_year = as.numeric(NA),
      coc_funding_requested = as.numeric(NA),
      funding_action = fifelse(mckinneyvento == "Yes", "Renew", "Ignore"),
      coc_version_id = coc_version_id,
      # additional cols user will fill out
      is_dedicated_ch_fam = factor_yesno(NA),
      is_dedicated_ch_ind = factor_yesno(NA),
      is_dedicated_dv = factor_yesno(NA),
      amount_other_public_funding = as.numeric(NA),
      amount_private_funding = as.numeric(NA),
      all_ind_beds = beds_hh_wo_children + beds_hh_w_only_children,
      total_ch_ind_beds = ch_beds_hh_wo_children + ch_beds_hh_w_only_children,
      dv_fam_beds = fifelse(target_population == "DV", beds_hh_w_children, as.integer(0)),
      dv_ind_beds = fifelse(target_population == "DV", all_ind_beds, as.integer(0))
    ) %>%
    fmutate(
      funding_action = convert_to_factor(., "funding_action", textToNum = TRUE),
      # project_type = convert_to_factor(., "project_type", textToNum = TRUE),
      # target_population = convert_to_factor(., "target_population", textToNum = TRUE),
      created_by = SERVICE_ACCOUNT
    ) |>
    frename(bed_field_mapping) |>
    get_vars(setdiff(dbListFields(DB_CON, "projects"), "project_id"))
  
  return(project_data)
}


dbAppendTable(DB_CON, "coc_versions", coc_versions)

for (i in 1:nrow(coc_versions)) {
  # Access row data using index i
  current_row <- coc_versions[i, ]
  filtered_data <- get_hic_data(current_row$coc, current_row$coc_version_id)
  filtered_data_db <- factor_vars_db_prep(filtered_data)

  DBI::dbAppendTable(DB_CON, "projects", filtered_data_db)
}

dbAppendTable(DB_CON, "coc_version_users", coc_version_users)
dbAppendTable(DB_CON, "coc_version_requests", coc_version_requests)