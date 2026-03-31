.db_env <- new.env(parent = emptyenv())

get_db_pool <- function() {
  if (is.null(.db_env$pool)) stop("DB pool not initialized. Call set_db_pool() first.")
  .db_env$pool
}

set_up_db_connection <- function() {
  use_sqlite <- ifelse(exists("USE_SQLITE", where = .GlobalEnv), USE_SQLITE, Sys.getenv("RSTUDIO") == "1")
  .db_env$connection_type <- ifelse(use_sqlite, "SQLite", "RPostgres")
  
  .db_env$pool <- if(Sys.getenv("RSTUDIO") == "1" && use_sqlite) {
    get_sqlite_db()
  } else {
    if(Sys.getenv("RSTUDIO") == "1") set_up_tunnel()
    get_postgres_db()
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
  } else {
    message("Port 5432 is already in use. Assuming tunnel is active.")
  }
}

with_tunnel_retry <- function(db_expr) {
  # Capture the exact code passed into the function and the environment it came from
  expr <- substitute(db_expr)
  env  <- parent.frame()
  
  tryCatch({
    # 1. Attempt the database operation
    eval(expr, env)
    
  }, error = function(e) {
    
    # 2. If we are using SQLite, skip tunnel logic entirely and pass the error up
    if (.db_env$connection_type == "SQLite" || Sys.getenv("RSTUDIO") != "1") {
      stop(e)
    }
    
    # 3. If we are using Postgres, check if it's a network/tunnel error
    is_conn_error <- grepl("closed the connection|Connection refused|could not connect|no connection|terminating connection", e$message, ignore.case = TRUE)
    
    # 4. If the tunnel died AND we are in local development (RStudio)
    if (is_conn_error && Sys.getenv("RSTUDIO") == "1") {
      message("Database connection lost. Restarting SSH tunnel...")
      
      # Kill dead tunnel and start a new one
      kill_open_tunnel()
      set_up_tunnel()
      Sys.sleep(2) # Give the tunnel a moment to connect to AWS
      
      # Try the exact same database operation one more time!
      message("Retrying query...")
      return(eval(expr, env))
    }
    
    # 5. If it's just a normal SQL syntax error, pass the error up
    stop(e)
  })
}

kill_open_tunnel <- function() {
  message("Looking for and killing any open SSH tunnels on port 5432...")
  
  # This looks for any running process where the command contains "ssh" and "5432" and kills it.
  system("pkill -f 'ssh.*5432'", ignore.stdout = TRUE, ignore.stderr = TRUE)
  
  # Give the OS a second to release the port
  Sys.sleep(1)
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
    password = Sys.getenv("AWS_RDS_PASSWORD"),
    sslmode = "require"
  )
} 

get_sqlite_db <- function() {
  pool::dbPool(
    drv = RSQLite::SQLite(),
    dbname = here("sandbox/dev_db.sqlite")
  )
}

# Get a dev version that persists beyond the app 
# shiny::onStop(function() {
#   pool::poolClose(get_db_pool())
# })

close_pool <- function() {
  pool::poolClose(get_db_pool())
}

db_connect <- function(use_sqlite = Sys.getenv("RSTUDIO") == "1") {
  USE_SQLITE <<- use_sqlite
  set_up_db_connection()
}

run_app <- function(use_sqlite = Sys.getenv("RSTUDIO") == "1") {
  USE_SQLITE <<- use_sqlite
  runApp()
}
