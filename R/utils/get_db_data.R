# Create connection to RDS Postgres instance for testing purposes
if(IN_RSTUDIO && USE_DEV_POSTGRES_DB) {
  # Only open tunnel if port 5432 isn't already in use
  port_in_use <- system("ss -tulnp | grep ':5432'")
  
  if (port_in_use == 1) {
    
    tunnel <- sys::exec_background(
      "ssh",
      args = c(
        "-i", Sys.getenv("AWS_SSH_KEY"),
        "-L", paste(Sys.getenv("AWS_RDS_PORT"), Sys.getenv("AWS_RDS_HOST"), Sys.getenv("AWS_RDS_PORT"), sep=":"),
        "-N", "-o", "StrictHostKeyChecking=no",
        Sys.getenv("AWS_SSH_USER")
      )
    )
    
    Sys.sleep(2)
    
    shiny::onStop(function() {
      tools::pskill(tunnel)
    })
  }
}

# Database configuration
# Using a global pool shares access to the db and is long-lived
# - maintains N reusable connections
# - loans them out only when needed (fewer db resources)
# - automatically reconnects dropped connections (better stability, esp. if usage spikes)
DB_POOL <- if(IN_PROD_APP || USE_DEV_POSTGRES_DB) {
  pool::dbPool(
    drv = RPostgres::Postgres(),
    host = ifelse(IN_RSTUDIO, "localhost", Sys.getenv("AWS_RDS_HOST")),
    port = as.integer(Sys.getenv("AWS_RDS_PORT", "3306")),
    dbname = Sys.getenv(ifelse(IN_PROD_APP, "AWS_RDS_DBNAME", "AWS_RDS_DBNAME_DEV")),
    user = Sys.getenv("AWS_RDS_USERNAME"),
    password = Sys.getenv("AWS_RDS_PASSWORD")
  )
} else {
  pool::dbPool(
    drv = RSQLite::SQLite(),
    dbname = here("sandbox/dev_db.sqlite")
  )
}

shiny::onStop(function() {
  pool::poolClose(DB_POOL)
})


# Get DB data ------------------
# dbGetQuery returns result set
convert_timestamps_to_POSIXct <- function(dt) {
  if("date_created" %in% names(dt)) 
    dt[, date_created := as.POSIXct(date_created, tz = "UTC")]
  
  if("date_updated" %in% names(dt)) 
    dt[, date_updated := as.POSIXct(date_updated, tz = "UTC")]
  
  return(dt)
}

convert_timestamps_to_char <- function(dt) {
  if("date_created" %in% names(dt)) 
    dt[, date_created := as.character(date_created, tz = "UTC")]
  
  if("date_updated" %in% names(dt)) 
    dt[, date_updated := as.character(date_updated, tz = "UTC")]
  
  return(dt)
}

get_db_query <- function(sql, params = NULL) {
  tryCatch({
    dt <- DBI::dbGetQuery(
      DB_POOL,
      sql,
      params = params
    ) |> 
      qDT() |>
      convert_timestamps_to_char()
    
    return(dt)
  }, error = function(e) {
    list(ok = FALSE, error = e$message)
  })
}


get_db_tbl <- function(tbl_name) {
  tryCatch({
    tbl <- dbReadTable(DB_POOL, tbl_name) |> qDT()
    
    convert_timestamps_to_char(tbl)
    
    return(tbl)
  }, error = function(e) {
    list(ok = FALSE, error = e$message)
  })
}


# Write to db -------------------
# dbExecute returns rows affected
db_execute <- function(sql, params) {
  tryCatch({
    pool::poolWithTransaction(DB_POOL, function(p) {
      dbExecute(p, sql, params = params)
    })
  }, error = function(e) {
    list(ok = FALSE, error = e$message)
  })
}

db_append <- function(tbl, data) {
  tryCatch({
    pool::poolWithTransaction(DB_POOL, function(p) {
      dbAppendTable(p, tbl, data)
    })
  }, error = function(e) {
    list(ok = FALSE, error = e$message)
  })
}



# --- Specific DB Pulls --------------
## Requests/CoC Selection --------
get_coc_versions_for_user <- function(username) {
  get_db_query(
    "SELECT v.*, u.username, u.coc_version_role, v.created_by
      FROM coc_versions v
      LEFT JOIN coc_version_users u
      ON v.coc_version_id = u.coc_version_id
      WHERE u.username = $1
    ",
    params = username
  )
}


## In-App Rating --------------
get_project_evaluation <- function(coc_version_id, project_id) {
  get_db_query(
    "SELECT p.coc_version_id, pe.project_id, method, met_hud_thresholds, met_coc_thresholds, pe.date_updated 
    FROM project_evaluations pe
    LEFT JOIN projects p ON pe.project_id = p.project_id
    WHERE p.coc_version_id = $1 and pe.project_id = $2",
    params = list(coc_version_id, project_id)
  )
}

get_rating_factors_and_scores <- function(coc_version_id, selected_project) {
  target_population <- ifelse(
    is.na(selected_project$target_population) || 
      get_lookup_label(selected_project$target_population, 'target_population') == 'NA',
    get_lookup_refid('General', 'target_population'),
    selected_project$target_population
  )
  
  get_db_query(
    "SELECT r.rating_factor_id, 
      r.rating_factor_text, 
      CASE WHEN r.rating_factor_text_short IS NOT NULL THEN r.rating_factor_text_short ELSE r.rating_factor_text END AS rating_factor_text_short, 
      r.piping_text, r.project_type, r.target_population, sr.selected_rating_factor_id, 
      fg.factor_group, fsg.factor_subgroup, 
      r.goal, r.max_point_value,
      rs.rating_score, rs.performance, rs.project_id,
      rs.date_updated
    FROM rating_factors r
    INNER JOIN selected_rating_factors sr ON sr.rating_factor_id = r.rating_factor_id
    JOIN factor_groups fg ON r.factor_group = fg.factor_group_id
    LEFT JOIN factor_subgroups fsg ON r.factor_subgroup = fsg.factor_subgroup_id
    LEFT JOIN rating_scores rs ON rs.selected_rating_factor_id = sr.selected_rating_factor_id
    WHERE sr.coc_version_id = $1 AND 
      r.funding_action = $2 AND
      r.project_type = $3 AND
      r.target_population = $4 AND
      (rs.project_id = $5 OR rs.project_id IS NULL)
    ", 
    params = list(
      coc_version_id,
      selected_project$funding_action,
      selected_project$project_type,
      target_population,
      selected_project$project_id
    )
  )
}

## Alternative Rating ---------------
get_alternative_rating <- function(coc_version_id) {
  get_db_query(
    "SELECT 
            p.project_id, 
            p.organization_name, 
            p.project_name, 
            p.grant_number, 
            p.funding_action, 
            p.project_type, 
            p.target_population, 
            pe.met_hud_thresholds,
            pe.met_coc_thresholds,
            pe.weighted_score,
            pe.date_updated
          FROM projects p
          LEFT JOIN project_evaluations pe ON p.project_id = pe.project_id
          LEFT JOIN lookups l ON p.funding_action = l.reference_id
          WHERE p.coc_version_id = $1 AND p.funding_action IS NOT NULL AND l.value <> 'Ignore'",
    params = list(coc_version_id)
  )
}
