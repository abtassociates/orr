library(magrittr)

LOOKUPS <- get_db_tbl("lookups")
if("error" %in% names(LOOKUPS)) {
  set_up_db_connection()
}
delete_test_data <- function(p, tbl, anchorid) {
  print(glue::glue("deleting from {tbl}"))
  dbExecute(
    p, 
    glue::glue(
      "DELETE FROM {tbl} 
      WHERE {anchorid} < 0"
    )
  )
}

if(USE_SQLITE) DBI::dbExecute(get_db_pool(), "PRAGMA foreign_keys = OFF;")
tbls_to_clear <- c(
  "coc_version_requests" = "coc_version_id",
  "coc_version_users" = "coc_version_id",
  "thresholds" = "coc_version_id",
  "selected_thresholds" = "coc_version_id",
  "selected_rating_factors" = "coc_version_id",
  "selected_coc_nofo_opportunities" = "coc_version_id",
  "rating_factors" = "coc_version_id",
  "rating_scores" = "project_id",
  "threshold_entries" = "project_id",
  "project_evaluations" = "project_id",
  "projects" = "coc_version_id",
  "coc_versions" = "coc_version_id"
)

pool::poolWithTransaction(get_db_pool(), function(p) {
lapply(names(tbls_to_clear), function(t) {
  delete_test_data(p, t, tbls_to_clear[[t]])
})

if(USE_SQLITE) DBI::dbExecute(p, "PRAGMA foreign_keys = ON;")

print("done deleting")

USERS <- DBI::dbReadTable(p, "users")

main_user <- toString(USERS[1, 1]) # alex.silverman@abtglobal.com
second_user <- toString(USERS[3, 1])

# -5 IL-517 Main Version, Second user owns, no one else on, no requests
# -4: FL-600 Main Version , Main user owns, no one else on, second user requested
# -3: AK-500 Main Version, Main user owns, second user is editor (request approved)
# -2: AK-500 Alternate, Second user owns, main user is editor (main user request approved)
# -1: AK-501 Main Version, Second user owns, no one else on, main user requested
coc_version_ids <- -6:-1
not_started <- get_lookup_refid("Not Started", "coc_status")
in_progress <- get_lookup_refid("In Progress", "coc_status")

coc_versions <- data.table(
  coc_version_id = coc_version_ids,
  coc_version_name = c(
    'IL-517 Main Version',
    'IL-517 Second Version',
    'FL-600 Main Version',
    'AK-500 Main Version',
    'AK-500 Alternative Version',
    'AK-501 Main Version'
  ),
  coc = c('IL-517', 'IL-517', 'FL-600', 'AK-500', 'AK-500', 'AK-501'),
  coc_status = c(not_started, not_started, not_started, in_progress, not_started, in_progress),  # Not Started, In Progress, Not Started, In Progress
  created_by = c(
    second_user,
    second_user,
    main_user,
    main_user,
    second_user,
    second_user
  ),
  updated_by = main_user
)

# CoC Version Users (many-to-many relationship)
owner <- get_lookup_refid("Owner", "coc_version_role")
editor <- get_lookup_refid("Editor", "coc_version_role")

coc_version_users <- data.table(
  coc_version_user_id = -12:-5,
  coc_version_id = c(-6, -5, -4, -3, -3, -2, -2, -1),
  username = c(second_user, second_user, main_user, main_user, second_user, second_user, main_user, second_user),
  coc_version_role = c(owner, owner, owner, owner, editor, owner, editor, owner), 
  created_by = c(
    second_user,
    second_user,
    main_user,
    main_user,
    second_user,
    second_user,
    main_user,
    second_user
  ),
  updated_by = main_user
)

# CoC Version Requests (requests to versions where you are Owner)
# -5 IL-517 Main Version, Second user owns, no one else on, no requests
# -4: FL-600 Main Version , Main user owns, no one else on, second user requested
# -3: AK-500 Main Version, Main user owns, second user is editor (request approved)
# -2: AK-500 Alternate, Second user owns, main user is editor (main user request approved)
# -1: AK-501 Main Version, Second user owns, no one else on, main user requested
sent <- get_lookup_refid("Sent","request_status")
approved <- get_lookup_refid("Approved","request_status")

coc_version_requests <- data.table(
  coc_request_id = -4:-1,
  coc_version_id = c(-4, -3, -2, -1),
  request_status = c(sent, approved, approved, sent),  # Sent by second user to main, Approved by main from second, Approved by second user from main, Sent by main user to second
  reason_for_rejection = NA_integer_,
  created_by = c(
    second_user,
    second_user,
    main_user,
    main_user
  ),
  date_created = Sys.time() - c(86400, 43200, 86300, 43200),  # 1 day ago, 12 hours ago
  date_updated = Sys.time() - c(86400, 43200, 86300, 43200),
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
  
  coc_data <- DBI::dbReadTable(p, "all_hic_data") |>
    fsubset(hudnum == coc) 
  
  project_data <- coc_data %>%
    fmutate(
      mckinneyvento = factor_yesno(rowSums(gvr(., "mckinneyvento"), na.rm = TRUE) > 0),
      # mckinneyventoyhdp = factor_yesno(mckinneyventoyhdp),
      dv_renewal = factor_yesno(NA),
      grant_number = as.character(NA), 
      coc_amount_awarded_last_year = as.numeric(NA),
      coc_amount_expended_last_year = as.numeric(NA),
      coc_funding_requested = as.numeric(NA),
      funding_action = fifelse(mckinneyvento == "Yes", get_lookup_refid("Renew", "funding_action"), get_lookup_refid("Ignore", "funding_action")), # renew = 10, ignore = 13
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
      # funding_action = convert_to_factor(., "funding_action", textToNum = TRUE),
      # project_type = convert_to_factor(., "project_type", textToNum = TRUE),
      # target_population = convert_to_factor(., "target_population", textToNum = TRUE),
      created_by = 'orr_service@abtglobal.com'
    ) |>
    frename(bed_field_mapping) |>
    get_vars(setdiff(dbListFields(get_db_pool(), "projects"), "project_id"))

  return(project_data)
}

DBI::dbAppendTable(p, "coc_versions", coc_versions)

print("doing projects")
total_projects <- 0
for (i in 1:nrow(coc_versions)) {
  # Access row data using index i
  current_row <- coc_versions[i, ]
  filtered_data <- get_hic_data(current_row$coc, current_row$coc_version_id)
  num_projects <- fnrow(filtered_data)

  ids <- if(total_projects == 0) seq(1, num_projects) else seq(total_projects + 1, total_projects + num_projects)
  
  filtered_data <- filtered_data |>
    fmutate(project_id = -1*ids)
  
  total_projects <- total_projects + num_projects

  filtered_data_db <- factor_vars_db_prep(filtered_data)

  DBI::dbAppendTable(p, "projects", filtered_data_db)
}

DBI::dbAppendTable(p, "coc_version_users", coc_version_users)
DBI::dbAppendTable(p, "coc_version_requests", coc_version_requests)

print("Adding selected thresholds, factors, and nofo opportunities for test coc_versions")
source("R/app_db_funcs/db_01a_mod_coc_selection.R", local=TRUE)

lapply(coc_version_ids, function(coc_version_id) {
  generate_data_for_new_coc_version(p, coc_version_id)
})
})
print("done generating demo data")
pool::poolClose(get_db_pool())