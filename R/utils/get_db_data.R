set_up_db_connection <- function(IN_PROD_APP, USE_DEV_POSTGRES_DB) {
  if(Sys.getenv("RSTUDIO") == "1" && USE_DEV_POSTGRES_DB)
    set_up_tunnel()
  
  if(IN_PROD_APP) {
    return(get_postgres_db(IN_PROD_APP = TRUE))
  } else if(USE_DEV_POSTGRES_DB) {
    return(get_postgres_db(IN_PROD_APP = FALSE))
  } else {
    return(get_sqlite_db())
  }
}

# Create connection to RDS Postgres instance for testing purposes
set_up_tunnel <- function() {
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
get_postgres_db <- function(IN_PROD_APP) {
  pool::dbPool(
    drv = RPostgres::Postgres(),
    host = ifelse(Sys.getenv("RSTUDIO") == "1", "localhost", Sys.getenv("AWS_RDS_HOST")),
    port = as.integer(Sys.getenv("AWS_RDS_PORT", "3306")),
    dbname = Sys.getenv(ifelse(IN_PROD_APP, "AWS_RDS_DBNAME", "AWS_RDS_DBNAME_DEV")),
    user = Sys.getenv("AWS_RDS_USERNAME"),
    password = Sys.getenv("AWS_RDS_PASSWORD")
  )
} 

get_sqlite_db <- function() {
  pool::dbPool(
    drv = RSQLite::SQLite(),
    dbname = here("sandbox/dev_db.sqlite")
  )
}
# Get a dev version that persists beyond the app 
shiny::onStop(function() {
  if(IN_PROD_APP)
    pool::poolClose(DB_POOL)
})


convert_timestamps_to_char <- function(dt) {
  if("date_created" %in% names(dt)) 
    dt[, date_created := as.character(date_created, tz = "UTC")]
  
  if("date_updated" %in% names(dt)) 
    dt[, date_updated := as.character(date_updated, tz = "UTC")]
  
  return(dt)
}

# Get DB data ------------------
get_db_query <- function(sql, params = NULL, p = DB_POOL) {
  tryCatch({
    dt <- DBI::dbGetQuery(
      p,
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


get_db_tbl <- function(tbl_name, p = DB_POOL) {
  tryCatch({
    tbl <- dbReadTable(p, tbl_name) |> qDT()
    
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
    dbExecute(DB_POOL, sql, params = params)
  }, error = function(e) {
    list(ok = FALSE, error = e$message)
  })
}

db_append <- function(tbl, data) {
  tryCatch({
    dbAppendTable(DB_POOL, tbl, data)
  }, error = function(e) {
    list(ok = FALSE, error = e$message)
  })
}
