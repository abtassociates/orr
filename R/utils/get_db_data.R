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

get_db_query <- function(sql, params = NULL) {
  RPostgres::dbGetQuery(
    get_db_connection(),
    sql,
    params = params
  ) %>% qDT()
}