.db_env <- new.env(parent = emptyenv())

set_db_pool <- function(pool) {
  .db_env$pool <- pool
}

get_db_pool <- function() {
  if (is.null(.db_env$pool)) stop("DB pool not initialized. Call set_db_pool() first.")
  .db_env$pool
}

set_up_db_connection <- function(USE_SQLITE = Sys.getenv("RSTUDIO") == "1") {
  if(Sys.getenv("RSTUDIO") == "1" && USE_SQLITE) {
    return(get_sqlite_db())
  } else {
    if(Sys.getenv("RSTUDIO") == "1") set_up_tunnel()
    return(get_postgres_db())
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
get_postgres_db <- function() {
  pool::dbPool(
    drv = RPostgres::Postgres(),
    host = ifelse(Sys.getenv("RSTUDIO") == "1", "localhost", Sys.getenv("AWS_RDS_HOST")),
    port = as.integer(Sys.getenv("AWS_RDS_PORT", "3306")),
    dbname = Sys.getenv(ifelse(IN_PROD_APP(), "AWS_RDS_DBNAME", "AWS_RDS_DBNAME_DEV")),
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
  if(IN_PROD_APP())
    pool::poolClose(get_db_pool())
})
