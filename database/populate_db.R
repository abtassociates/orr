populate_db <- function(
    add_demo_data = basename(getwd()) != "ORR", 
    USE_SQLITE = Sys.getenv("RSTUDIO") == "1"
) {
  ans <- readline(
    prompt = "If you proceed, you will erase all database data. Proceed? (Y/N): "
  )
  
  if (toupper(trimws(ans)) != "Y") {
    message("Cancelled.")
    return(invisible(NULL))
  }
  
  message("Proceeding...")
  
  library(here)
  library(DBI)
  library(data.table)
  library(collapse)
  
  files <- list.files(here("R/utils"), pattern = "\\.R$", full.names = TRUE)
  lapply(files, source)
  
  set_up_db_connection()
  p <- get_db_pool()
  SERVICE_ACCOUNT <- 'orr_service@abtglobal.com'
  
  # --- 1. PREPARE THE SQL SCRIPT ---
  message("Reading and preparing SQL schema...")
  sql_lines <- readLines(here("database/database_schema.sql"))
  sql_string <- paste(sql_lines, collapse = "\n")
  
  if (USE_SQLITE) {
    # SQLite Configuration
    DBI::dbExecute(p, "PRAGMA journal_mode = WAL;")
    DBI::dbExecute(p, "PRAGMA synchronous = NORMAL;")
    DBI::dbExecute(p, "PRAGMA foreign_keys = OFF;") # Required to drop tables with dependencies
    
    sql_string <- stringi::stri_replace_all_fixed(
      sql_string,
      c("__PK_TYPE__", "__CASCADE__"), 
      c("INTEGER PRIMARY KEY AUTOINCREMENT", ""),
      vectorize_all = FALSE
    )
  } else {
    # PostgreSQL Configuration
    sql_string <- stringi::stri_replace_all_fixed(
      sql_string,
      c("__PK_TYPE__", "__CASCADE__"), 
      c("SERIAL PRIMARY KEY", "CASCADE"),
      vectorize_all = FALSE
    )
  }
  
  # --- 2. EXECUTE THE DDL SCHEMA ---
  # In R/DBI, multiple statements in a single string need to be split if the driver doesn't support it natively
  # Or executed using a library like dbplyr or custom splits.
  # queries <- strsplit(sql_string, ";\\s*")[[1]]
  queries <- strsplit(sql_string, ";\\s*\\n")[[1]]
  
  
  pool::poolWithTransaction(p, function(pcon) {
    for (q in queries) {
      clean_q <- trimws(q)
      if (nchar(clean_q) > 0) DBI::dbExecute(pcon, clean_q)
    }
  })
  
  
  # Turn foreign keys back on for SQLite after creation
  if (USE_SQLITE) {
    DBI::dbExecute(p, "PRAGMA foreign_keys = ON;")
  }
  
  # --- 3. LOAD DYNAMIC / CSV DATA ---
  message("Loading HIC Data...")
  HIC_DATA_FILEPATH <- here("database/HIC_RawData2025 - 7.21.25_TEST.csv")
  hic_data <- fread(HIC_DATA_FILEPATH) |> 
    frename(
      row_num                   = "Row #",
      hudnum                    = "HudNum",
      coc_name                  = "CoC",
      organization_name         = "Organization Name",
      project_name              = "Project Name",
      project_type              = "Project Type",
      geocode                   = "Geocode",
      target_population         = "Target Population",
      mckinneyventoesges        = "mcKinneyVentoEsgEs",
      mckinneyventoesgrrh       = "mcKinneyVentoEsgRrh",
      mckinneyventoesgcov       = "mcKinneyVentoEsgCov",
      mckinneyventoesgrrhcov    = "mcKinneyVentoEsgRUSH",
      mckinneyventococsh        = "mcKinneyVentoCocSh",
      mckinneyventococth        = "mcKinneyVentoCocTh",
      mckinneyventococpsh       = "mcKinneyVentoCocPsh",
      mckinneyventococrrh       = "mcKinneyVentoCocRrh",
      mckinneyventococsro       = "mcKinneyVentoCocSro",
      mckinneyventococthrrh     = "mcKinneyVentoCocThRrh",
      mckinneyventospc          = "mcKinneyVentoSpC",
      mckinneyventos8           = "mcKinneyVentoS8",
      mckinneyventoshp          = "mcKinneyVentoShp",
      mckinneyventounshelt      = "mcKinneyVentoUnshelt",
      mckinneyventorural        = "mcKinneyVentoRural",
      beds_hh_w_children        = "Beds HH w/ Children",
      veteran_beds_hh_w_children= "Veteran Beds HH w/ Children",
      youth_beds_hh_w_children  = "Youth Beds HH w/ Children",
      ch_beds_hh_w_children     = "CH Beds HH w/ Children",
      beds_hh_wo_children       = "Beds HH w/o Children",
      veteran_beds_hh_wo_children = "Veteran Beds HH w/o Children",
      youth_beds_hh_wo_children = "Youth Beds HH w/o Children",
      ch_beds_hh_wo_children    = "CH Beds HH w/o Children",
      beds_hh_w_only_children   = "Beds HH w/ only Children",
      ch_beds_hh_w_only_children = "CH Beds HH w only Children"
    ) |>
    fmutate(
      mckinneyventoesg = FALSE,
      mckinneyventococ = FALSE,
      created_by = SERVICE_ACCOUNT, 
      updated_by = SERVICE_ACCOUNT
    )
  
  # Fetch lookup tables from database
  lookups <- pool::poolWithTransaction(p, function(pcon) {
    project_type_lookup <- DBI::dbGetQuery(pcon, "SELECT reference_id, value FROM lookups WHERE reference_type = 'project_type'")
    target_population_lookup <- DBI::dbGetQuery(pcon, "SELECT reference_id, value FROM lookups WHERE reference_type = 'target_population'")
    list(project_type_lookup = project_type_lookup, target_population_lookup = target_population_lookup)
  })
  
  # Create named vectors for mapping
  project_type_map <- setNames(lookups$project_type_lookup$reference_id, lookups$project_type_lookup$value)
  target_population_map <- setNames(lookups$target_population_lookup$reference_id, lookups$target_population_lookup$value)
  
  hic_data <- hic_data |>
    fmutate(
      target_population = fifelse(is.na(target_population) | target_population == "", "NA", as.character(target_population)),
      project_type = project_type_map[project_type],
      target_population = target_population_map[target_population]
    )
  
  DBI::dbAppendTable(p, "all_hic_data", hic_data)
  
  # --- 4. GENERATE STATES & COCS FROM CSV DATA ---
  message("Generating States and CoCs from HIC data...")
  pool::poolWithTransaction(p, function(pcon) {
  DBI::dbExecute(pcon, glue::glue("
    INSERT INTO states (state_code, state_name, created_by)
    SELECT DISTINCT SUBSTR(hudnum, 1, 2) as state_code, 'Unknown' as state_name, '{SERVICE_ACCOUNT}'
    FROM all_hic_data;
  "))
  
  DBI::dbExecute(pcon, glue::glue("
    INSERT INTO cocs (coc_code, coc_name, state, created_by)
    SELECT DISTINCT hudnum as coc_code, coc_name as coc_name, SUBSTR(hudnum, 1, 2) as state, '{SERVICE_ACCOUNT}'
    FROM all_hic_data;
  "))
  })
  
  # --- 5. GIW + HUD ARD
  message("Loading GIW data...")
  GIW_DATA_FILEPATH <- here("database/GIW.csv")
  giw_data <- fread(GIW_DATA_FILEPATH, encoding = "Latin-1") |>
    # Rename columns to match SQL table
    frename(
      "Grant Number" = "grant_number",
      "CoC" = "coc",
      "Applicant Name" = "applicant_name",
      "Project Name" = "project_name",
      "Expiration Year" = "expiration_year",
      "Project Component" = "project_component",
      "Restriction (DV or YHDP)" = "restriction_dv_or_ydhp",
      "DV ARD (Estimated)" = "dv_ard_estimated",
      "YHDP ARD (Estimated)" = "yhdp_ard_estimated",
      "CoCs ARD (Estimated)" =  "cocs_ard_estimated",
      "Total Units" = "total_units",
      "Total ARA" = "total_ara"
    ) |>
    fsubset(coc %in% funique(hic_data$hudnum)) |>
    fmutate(created_by = SERVICE_ACCOUNT, updated_by = SERVICE_ACCOUNT)

  DBI::dbAppendTable(p, "giw", giw_data)
  
  message("Loading HUD ARD data...")
  HUD_ARD_DATA_FILEPATH <- here("database/HUD_ard_report.csv")
  hud_ard_data <- fread(HUD_ARD_DATA_FILEPATH, encoding="Latin-1") |>
    frename( 
      "CoCName" = "coc",
      "CoC Number and Name" = "coc_number_and_name",
      "PPRN" = "pprn",
      "Estimated ARD" = "estimated",
      "Tier 1" = "tier_1",
      "CoC Bonus" = "coc_bonus",
      "DV Bonus" = "dv_bonus",
      "CoC Planning" = "coc_planning"
    ) |>
    fsubset(coc %in% funique(hic_data$hudnum)) |>
    fmutate(created_by = SERVICE_ACCOUNT, updated_by = SERVICE_ACCOUNT)
  DBI::dbAppendTable(p, "hud_ard_report", hud_ard_data)
  
  
  message("Done populating the db!")
  
  if(Sys.getenv("RSTUDIO") == "1" || add_demo_data)
    source(here("sandbox/generate_test_data_for_demo.R"), local=TRUE)
}
