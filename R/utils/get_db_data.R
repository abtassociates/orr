# Database configuration
DB_CONFIG <- list(
  host = Sys.getenv("AWS_RDS_HOST"),
  port = as.integer(Sys.getenv("AWS_RDS_PORT", "3306")),
  dbname = Sys.getenv("AWS_RDS_DBNAME"),
  username = Sys.getenv("AWS_RDS_USERNAME"),
  password = Sys.getenv("AWS_RDS_PASSWORD")
)

# Database connection function
get_db_connection <- function() {
  if(IN_DEV_MODE) {
    dbConnect(RSQLite::SQLite(), here("sandbox/dev_db.sqlite"))
  } else {
    dbConnect(
      RPostgres::Postgres(),
      host = DB_CONFIG$host,
      port = DB_CONFIG$port,
      dbname = DB_CONFIG$dbname,
      user = DB_CONFIG$username,
      password = DB_CONFIG$password,
      sslmode = "require"
    )
  }
}

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