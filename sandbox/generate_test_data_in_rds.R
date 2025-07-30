
# 
# n <- 50
# 
# # --- Generate the data.table ---
# 
# projects_dt <- data.table(
#   # project_id SERIAL PRIMARY KEY -> Simple integer sequence
#   project_id = 1:n,
# 
#   # coc_instance_id SMALLINT REFERENCES -> Sample from a small set of IDs
#   coc_instance_id = sample(1:5, n, replace = TRUE),
# 
#   # grant_number INTEGER NULL -> Sample integers, with some NAs
#   grant_number = sample(c(100000:150000, NA_integer_), n, replace = TRUE),
# 
#   # project_name VARCHAR -> Create plausible, unique-ish project names
#   project_name = paste0(
#     sample(c("Hope", "Pathway", "Genesis", "Unity", "Beacon"), n, replace = TRUE),
#     " ",
#     sample(c("Shelter", "Housing", "Project", "Initiative"), n, replace = TRUE),
#     " ",
#     sample(10:99, n, replace = TRUE)
#   ),
# 
#   # organization_name VARCHAR -> Sample from a list of fake org names
#   organization_name = sample(
#     c("Community First Services", "Urban Pathways", "Family Solutions Inc.", "New Beginnings Alliance"),
#     n, replace = TRUE
#   ),
# 
#   # geocode VARCHAR(10) -> Fake geographic codes
#   geocode = sample(paste0(c("CA-601", "NY-510", "TX-201", "FL-125", "IL-303")), n, replace = TRUE),
# 
#   # project_type, funding_action, target_population SMALLINT REFERENCES -> Sample from small sets of IDs
#   project_type = sample(1:4, n, replace = TRUE),
#   funding_action = sample(1:4, n, replace = TRUE),
#   target_population = sample(1:6, n, replace = TRUE),
# 
#   # mckinneyventoydhp BOOLEAN -> Sample TRUE/FALSE
#   mckinneyventoydhp = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.15, 0.85)),
# 
#   # --- Bed Counts (INTEGER) ---
#   # We'll create the total bed counts first, then the subset counts based on them
#   all_fam_beds = rpois(n, lambda = 15), # Poisson distribution for realistic counts
#   all_ind_beds = rpois(n, lambda = 30),
# 
#   # is_dedicated_... BOOLEAN -> Sample TRUE/FALSE with lower probability for TRUE
#   is_dedicated_ch_fam = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.2, 0.8)),
#   is_dedicated_ch_ind = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.2, 0.8)),
#   is_dedicated_dv = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.3, 0.7)),
# 
#   # --- Financials (NUMERIC(11, 2)) ---
#   # Use runif for random numbers and round to 2 decimal places
#   coc_funding_requested = round(runif(n, min = 25000, max = 750000), 2),
#   amount_other_public_funding = round(runif(n, min = 0, max = 150000), 2),
#   amount_private_funding = round(runif(n, min = 0, max = 100000), 2),
# 
#   # coc_amount_expended_last_year NUMERIC(11, 2) NULL -> Generate then add NAs
#   coc_amount_expended_last_year = {
#     expended <- round(runif(n, min = 20000, max = 700000), 2)
#     expended[sample(1:n, size = n * 0.2)] <- NA # 20% of values will be NA
#     expended
#   },
# 
#   # --- Timestamps and Users ---
#   # date_created TIMESTAMP NOT NULL -> Recent dates within the last 2 years
#   date_created = Sys.time() - as.difftime(sample(1:(365*2), n, replace = TRUE), units = "days"),
# 
#   # created_by VARCHAR(100) NOT NULL -> Sample from a list of usernames
#   created_by = "alex.silverman@abtglobal.com",
# 
#   # date_updated and updated_by are nullable and dependent, so we create them as NA first
#   # and will populate them conditionally below
#   date_updated = lubridate::now(),
#   updated_by = NA_character_
# )
# 
# # --- Post-creation modifications for dependent columns ---
# 
# # Generate subset bed counts to be less than or equal to total beds for that row
# # Using mapply to apply a function row-wise based on the 'all_*_beds' columns
# # The helper function ensures we don't try to sample from a range like 0:0
# safe_sample <- function(max_val) if (max_val == 0) 0 else sample(0:max_val, 1)
# 
# projects_dt[, dv_fam_beds       := mapply(safe_sample, all_fam_beds)]
# projects_dt[, ch_fam_beds       := mapply(safe_sample, all_fam_beds)]
# projects_dt[, vet_fam_beds      := mapply(safe_sample, all_fam_beds)]
# projects_dt[, par_youth_beds    := mapply(safe_sample, all_fam_beds)]
# 
# projects_dt[, dv_ind_beds       := mapply(safe_sample, all_ind_beds)]
# projects_dt[, total_ch_ind_beds := mapply(safe_sample, all_ind_beds)]
# projects_dt[, vet_ind_beds      := mapply(safe_sample, all_ind_beds)]
# projects_dt[, single_youth_beds := mapply(safe_sample, all_ind_beds)]
# projects_dt[ , coc_instance_id := 6]
# 
# browser()
# RPostgres::dbAppendTable(
#   get_db_connection(),
#   "projects",
#   projects_dt
# )


# RPostgres::dbAppendTable(
#   get_db_connection(),
#   "cocs",
#   data.table(
#     coc_code = "CA-026",
#     coc_name = "TEST CA-026",
#     state = "CA",
#     date_created = lubridate::now(),
#     created_by = "alex.silverman@abtglobal.com"
#   )
# )
# 
# RPostgres::dbAppendTable(
#   get_db_connection(),
#   "coc_status",
#   data.table(
#     status = c(1, 2, 3),
#     status_description = c("Not Started", "In Progress", "Completed"),
#     date_created = lubridate::now(),
#     created_by = "alex.silverman@abtglobal.com"
#   )
# )
# 
# RPostgres::dbAppendTable(
#   get_db_connection(),
#   "project_types",
#   data.table(
#     project_type = c("TH","PSH","RRH","TH+RRH"),
#     project_type_full = c("Transitional Housing", "Permanent Supportive Housing", "Rapid Re-Housing", "Transitional Housing + Rapid Re-Housing"),
#     date_created = lubridate::now(),
#     created_by = "alex.silverman@abtglobal.com"
#   )
# )
# 
# RPostgres::dbAppendTable(
#   get_db_connection(),
#   "funding_actions",
#   data.table(
#     funding_action = c("Renew","New","Expand","Reallocate","Ignore"),
#     date_created = lubridate::now(),
#     created_by = "alex.silverman@abtglobal.com"
#   )
# )
# 
# RPostgres::dbAppendTable(
#   get_db_connection(),
#   "target_populations",
#   data.table(
#     target_population = c("DV", "HIC", "Youth", "General", "Chronically Homeless", "Veteran"),
#     target_population_abbrev = c("DV", "HIC", "Youth", "General", "CH", "Vet"),
#     date_created = lubridate::now(),
#     created_by = "alex.silverman@abtglobal.com"
#   )
# )
# RPostgres::dbAppendTable(
#   get_db_connection(),
#   "population_groups",
#   data.table(
#     population_group = c("Individual", "Family"),
#     population_group_abbrev = c("Ind","Fam"),
#     population_group_plural = c("Individuals","Families"),
#     date_created = lubridate::now(),
#     created_by = "alex.silverman@abtglobal.com"
#   )
# )
# RPostgres::dbAppendTable(
#   get_db_connection(),
#   "coc_instances",
#   data.table(
#     coc = "CA-026",
#     date_created = lubridate::now(),
#     created_by = "alex.silverman@abtglobal.com"
#   )
# )
