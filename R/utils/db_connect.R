.db_env <- new.env(parent = emptyenv())

get_db_pool <- function() {
  if (is.null(.db_env$pool)) stop("DB pool not initialized. Call db_connect() first.")
  .db_env$pool
}

set_up_db_connection <- function(dbname = NULL) {
  use_sqlite <- ifelse(exists("USE_SQLITE", where = .GlobalEnv), USE_SQLITE, Sys.getenv("RSTUDIO") == "1")
  .db_env$connection_type <- ifelse(use_sqlite, "SQLite", "RPostgres")
  
  .db_env$pool <- if(Sys.getenv("RSTUDIO") == "1" && use_sqlite) {
    get_sqlite_db()
  } else {
    if(Sys.getenv("RSTUDIO") == "1") set_up_tunnel()
    get_postgres_db(dbname)
  }
  
  # Enforce referential integrity for SQLite (PostgreSQL is automatic)
  if(use_sqlite)
    DBI::dbExecute(.db_env$pool, "PRAGMA foreign_keys = ON;")
  
  return(.db_env$pool)
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
# Helper to get connection params (to keep code DRY)
get_pg_params <- function(dbname) {
  list(
    drv = RPostgres::Postgres(),
    host = ifelse(Sys.getenv("RSTUDIO") == "1", "localhost", Sys.getenv("AWS_RDS_HOST")),
    port = as.integer(Sys.getenv("AWS_RDS_PORT", "5432")), # Default PG port is 5432
    user = Sys.getenv("AWS_RDS_USERNAME"),
    password = Sys.getenv("AWS_RDS_PASSWORD"),
    dbname = dbname,
    sslmode = "require"
  )
}

get_current_db <- function() {
  return(DBI::dbGetQuery(get_db_pool(), "SELECT current_database() AS dbname")$dbname[1])
}
get_open_db_connections <- function(dbname) {
  get_db_query(glue::glue('SELECT pid, usename, application_name, state, query
    FROM pg_stat_activity
  WHERE datname = "{dbname}"'))
}
get_postgres_db <- function(dbname = NULL) {
  dbname <- get_db_name(dbname)
  
  # 2. Try to connect to the target DB
  params <- get_pg_params(dbname)
  
  pool <- tryCatch({
    do.call(pool::dbPool, params)
  }, error = function(e) {
    # If the error isn't about the DB not existing, re-throw it
    if (!grepl("database .* does not exist", e$message)) {
      stop(e)
    }
    return(NULL)
  })
  
  # 3. If connection failed because DB doesn't exist (and we are in DEV)
  if (is.null(pool)) {
    if (!USE_SQLITE && IN_DEV_MODE) {
      message(paste("Database", dbname, "not found. Attempting to create..."))
      
      # Connect to the default 'postgres' maintenance DB
      admin_params <- get_pg_params("postgres")
      con <- do.call(DBI::dbConnect, admin_params)
      on.exit(DBI::dbDisconnect(con))
      
      # Double check if it exists (prevents race conditions)
      db_list <- DBI::dbGetQuery(con, "SELECT datname FROM pg_database WHERE datistemplate = false")
      
      if (!(dbname %in% db_list$datname)) {
        ans <- readline(glue::glue("Database '{dbname}' not found. Create it? Y/N: "))
        if (toupper(trimws(ans)) != "Y") stop("Database creation cancelled.")
        
        # CREATE DATABASE cannot run in a transaction
        # RPostgres runs this fine via dbExecute
        DBI::dbExecute(con, glue::glue("CREATE DATABASE {dbname}"))
        message(glue::glue("Database '{dbname}' created successfully."))
      }
      
      # Now that it exists, try to connect again (recursive call)
      return(get_postgres_db(dbname))
      
    } else {
      stop(glue::glue("Database '{dbname}' does not exist and auto-creation is disabled."))
    }
  } else {
    message(paste0("Connected to RPostgresql database: ", dbname))
  }
  
  return(pool)
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

get_db_name <- function(dbname = NULL) {
  # 1. Determine target dbname
  if(is.null(dbname)) {
    if(exists("DBNAME", where = .GlobalEnv)) dbname <- DBNAME
    else if(!IN_PROD_APP() && Sys.getenv("RSTUDIO") != "1" && basename(getwd()) %in% list_rpostgres_dbs()) dbname <- basename(getwd())
    else dbname <- Sys.getenv(ifelse(IN_PROD_APP(), "AWS_RDS_DBNAME", "AWS_RDS_DBNAME_DEV"))
  }
  return(dbname)
}

list_rpostgres_dbs <- function(dbname = "postgres") {
  db_connect(FALSE, dbname)
  message("Here are all the databases in RDS:")
  x <- DBI::dbGetQuery(get_db_pool(), "
    SELECT datname
    FROM pg_database
    WHERE datistemplate = false;
  ")
  close_pool()
  
  message(x$datname)
  return(x$datname)
}
db_connect <- function(use_sqlite = Sys.getenv("RSTUDIO") == "1", dbname = NULL) {
  USE_SQLITE <<- use_sqlite
  db_pool <- set_up_db_connection(dbname)
}

run_app <- function(use_sqlite = Sys.getenv("RSTUDIO") == "1", dbname = NULL, user_email = NULL) {
  USE_SQLITE <<- use_sqlite
  DEV_USER_LOGIN <<- user_email
  DBNAME <<- dbname
  library(shiny)
  runApp()
}
