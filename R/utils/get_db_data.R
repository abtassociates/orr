convert_timestamps_to_char <- function(dt) {
  if("date_created" %in% names(dt)) 
    dt[, date_created := as.character(date_created, tz = "UTC")]
  
  if("date_updated" %in% names(dt)) 
    dt[, date_updated := as.character(date_updated, tz = "UTC")]
  
  return(dt)
}

# Get DB data ------------------
get_db_query <- function(sql, params = NULL) {
  tryCatch({
    dt <- with_tunnel_retry({
      DBI::dbGetQuery(
        get_db_pool(),
        sql,
        params = params
      )
    })
    
    dt <- dt |> 
      qDT() |>
      convert_timestamps_to_char()
    
    return(dt)
  }, error = function(e) {
    list(ok = FALSE, error = e$message)
  })
}


get_db_tbl <- function(tbl_name) {
  tryCatch({
    tbl <- with_tunnel_retry({
      DBI::dbReadTable(
        get_db_pool(), 
        tbl_name
      )
    })
    
    tbl <- tbl |> 
      qDT() |>
      convert_timestamps_to_char()
    
    return(tbl)
  }, error = function(e) {
    list(ok = FALSE, error = e$message)
  })
}


# Write to db -------------------
# dbExecute returns rows affected
db_execute <- function(sql, params) {
  tryCatch({
    with_tunnel_retry({
      DBI::dbExecute(get_db_pool(), sql, params = params)
    })
  }, error = function(e) {
    list(ok = FALSE, error = e$message)
  })
}

db_append <- function(tbl, data) {
  tryCatch({
    with_tunnel_retry({
      DBI::dbAppendTable(get_db_pool(), tbl, data)
    })
  }, error = function(e) {
    list(ok = FALSE, error = e$message)
  })
}


get_db_column_limit <- function(table_name, column_name) {
  pool <- get_db_pool()
  
  # 1. Determine which DB type we are using
  # RPostgres returns "PostgreSQL", RSQLite returns "SQLite"
  if (.db_env$connection_type == "SQLite") {
    # SQLite uses PRAGMA table_info
    # This returns a table with a 'type' column (e.g., "VARCHAR(10)")
    res <- DBI::dbGetQuery(pool, paste0("PRAGMA table_info(", table_name, ")"))
    col_type <- res$type[res$name == column_name]
    
    # Use Regex to extract the number inside the parentheses: VARCHAR(10) -> 10
    limit <- gsub(".*\\((\\d+)\\).*", "\\1", col_type)
    
    # If no parentheses found (e.g. type is just 'TEXT'), limit will be the same as col_type
    if (limit == col_type) limit <- NA 
  } else {
    # Postgres uses information_schema
    sql <- "
      SELECT character_maximum_length 
      FROM information_schema.columns 
      WHERE table_name = $1 AND column_name = $2"
    res <- DBI::dbGetQuery(pool, sql, params = list(table_name, column_name))
    limit <- res$character_maximum_length[1]
  } 
  
  # Return the limit found, or a safe default (255) if it's unlimited/TEXT
  return(if (is.na(limit) || is.null(limit)) 255 else as.integer(limit))
}
