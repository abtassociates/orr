# Database configuration
# Using a global pool shares access to the db and is long-lived
# - maintains N reusable connections
# - loans them out only when needed (fewer db resources)
# - automatically reconnects dropped connections (better stability, esp. if usage spikes)
DB_POOL <- if(IN_DEV_MODE) {
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
    here("sandbox/dev_db.sqlite")
  )
}

onStop(function() {
  pool::poolClose(pool)
})
get_db_query <- function(sql, params = NULL) {
  dt <- DBI::dbGetQuery(
    DB_CON,
    sql,
    params = params
  ) |> qDT() 
  
  if("date_created" %in% names(dt)) 
    dt[, date_created := as.POSIXct(date_created)]
  
  if("date_updated" %in% names(dt)) 
    dt[, date_updated := as.POSIXct(date_updated)]
  
  dt
}


get_db_tbl <- function(tbl_name) {
  tbl <- dbReadTable(DB_CON, tbl_name) |>
    qDT()
  
  if("date_created" %in% names(tbl)) 
    tbl[, date_created := as.POSIXct(date_created)]
  
  if("date_updated" %in% names(tbl)) 
    tbl[, date_updated := as.POSIXct(date_updated)]
  
  return(tbl)
}

DB_CON <- get_db_connection()