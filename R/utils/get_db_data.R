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
    dt <- DBI::dbGetQuery(
      get_db_pool(),
      sql,
      params = params
    ) |> 
      qDT() |>
      convert_timestamps_to_char()
    
    return(dt)
  }, error = function(e) {
    list(ok = FALSE, error = e$message)
  })
}


get_db_tbl <- function(tbl_name) {
  tryCatch({
    tbl <- DBI::dbReadTable(
      get_db_pool(), 
      tbl_name
    ) |> 
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
    DBI::dbExecute(get_db_pool(), sql, params = params)
  }, error = function(e) {
    list(ok = FALSE, error = e$message)
  })
}

db_append <- function(tbl, data) {
  tryCatch({
    DBI::dbAppendTable(get_db_pool(), tbl, data)
  }, error = function(e) {
    list(ok = FALSE, error = e$message)
  })
}
