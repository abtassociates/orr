# Database configuration
# Using a global pool shares access to the db and is long-lived
# - maintains N reusable connections
# - loans them out only when needed (fewer db resources)
# - automatically reconnects dropped connections (better stability, esp. if usage spikes)
DB_POOL <- if(!IN_DEV_MODE) {
  pool::dbPool(
    drv = RPostgres::Postgres(),
    host = Sys.getenv("AWS_RDS_HOST"),
    port = as.integer(Sys.getenv("AWS_RDS_PORT", "3306")),
    dbname = Sys.getenv("AWS_RDS_DBNAME"),
    username = Sys.getenv("AWS_RDS_USERNAME"),
    password = Sys.getenv("AWS_RDS_PASSWORD")
  )
} else {
  pool::dbPool(
    drv = RSQLite::SQLite(),
    dbname = here("sandbox/dev_db.sqlite")
  )
}

onStop(function() {
  pool::poolClose(DB_POOL)
})


# Get DB data ------------------
# dbGetQuery returns result set
convert_timestamps_to_POSIXct <- function(dt) {
  if("date_created" %in% names(dt)) 
    dt[, date_created := as.POSIXct(date_created)]
  
  if("date_updated" %in% names(dt)) 
    dt[, date_updated := as.POSIXct(date_updated)]
  
  return(dt)
}
get_db_query <- function(sql, params = NULL) {
  tryCatch({
    dt <- DBI::dbGetQuery(
      DB_POOL,
      sql,
      params = params
    ) |> qDT()
    
    convert_timestamps_to_POSIXct(dt)
    
    return(dt)
  }, error = function(e) {
    list(ok = FALSE, error = e$message)
  })
}


get_db_tbl <- function(tbl_name) {
  tryCatch({
    tbl <- dbReadTable(DB_POOL, tbl_name) |> qDT()
    
    convert_timestamps_to_POSIXct(tbl)
    
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
<<<<<<< hotfix/saving-priorities
    }) 
=======
    })
>>>>>>> dev
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

