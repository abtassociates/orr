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
  if(in_dev_mode) {
    dbConnect(RSQLite::SQLite(), "dev_db.sqlite")
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
  DBI::dbGetQuery(
    DB_CON,
    sql,
    params = params
  ) |> qDT()
  DBI::dbReadTable()
}


get_db_tbl <- function(tbl_name) {
  get_db_query(
    glue::glue_sql(
      "SELECT * FROM {`tbl_name`}",
      .con = DB_CON
    )
  )
}
