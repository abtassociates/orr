library(DBI)
library(RPostgres)
library(RSQLite)

# Connect to the PostgreSQL database
DB_CONFIG <- list(
  host = Sys.getenv("AWS_RDS_HOST"),
  port = as.integer(Sys.getenv("AWS_RDS_PORT", "3306")),
  dbname = Sys.getenv("AWS_RDS_DBNAME"),
  username = Sys.getenv("AWS_RDS_USERNAME"),
  password = Sys.getenv("AWS_RDS_PASSWORD")
)

pg_conn <- dbConnect(
  RPostgres::Postgres(),
  host = DB_CONFIG$host,
  port = DB_CONFIG$port,
  dbname = DB_CONFIG$dbname,
  user = DB_CONFIG$username,
  password = DB_CONFIG$password,
  sslmode = "require"
)

# Connect to the SQLite database
sqlite_conn <- dbConnect(RSQLite::SQLite(), "dev_db.sqlite")

# List all tables in the PostgreSQL database
tables <- dbListTables(pg_conn)

# Copy each table from PostgreSQL to SQLite
for (table in tables) {
  # Read data from PostgreSQL
  data <- dbReadTable(pg_conn, table)
  
  # Write data to SQLite
  dbWriteTable(sqlite_conn, table, data, overwrite = TRUE)
}

# Disconnect from both databases
dbDisconnect(pg_conn)
dbDisconnect(sqlite_conn)