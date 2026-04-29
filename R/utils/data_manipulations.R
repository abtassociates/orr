
pluralize <- function(s) {
  ends <- "(sh?|x|z|ch)$"
  pluralify <- ifelse(grepl(ends, s, perl = TRUE), "es", "s")
  out <- gsub("ys$", "ies", paste0(s, pluralify))
  return(out)
}


factor_yesno <- function(v) {
  factor(
    v,
    levels = c(TRUE, FALSE, 1, 0), 
    labels = c("Yes", "No", "Yes", "No")
  )
}

get_labelled_lookups <- function(l, lookup_col = "value") {
  lookup_info <- LOOKUPS[reference_type == l]
  setNames(lookup_info$reference_id, lookup_info[[lookup_col]])
}
get_lookup_label <- function(v, ref_type, lookup_col = "value") {
  filtered_lookups <- LOOKUPS[reference_type == ref_type]
  if(is.character(v)) {
    filtered_lookups[reference_id == v, get(lookup_col)]
  } else {
    filtered_lookups[.(v), get(lookup_col), on = "reference_id"]
  } 
}

get_lookup_refid <- function(v, ref_type, lookup_col = "value") {
  filtered_lookups <- LOOKUPS[reference_type == ref_type]
  return(
    filtered_lookups[match(v, get(lookup_col))]$reference_id
  )
}

convert_to_factor <- function(data, v, textToNum = FALSE, label_col = "value") {
  lookup_info <- LOOKUPS[reference_type == v, .(reference_id, value, value_abbrev, value_long)]

  col_data <- if(!textToNum) {
    data[[v]] 
  } else {
    lookup_info$reference_id[match(data[[v]], lookup_info[[label_col]])]
  }
  
  factor(
    col_data,
    levels = lookup_info$reference_id,
    labels = lookup_info[[label_col]]
  )
}

#' @title Prepare Factor Variables for Database Insertion
#' @description Converts factor variables in a data frame to their boolean or numeric equivalents.
#'              "Yes"/"No" factors are converted to boolean (TRUE/FALSE). Other factors are
#'              converted to numeric reference IDs using the `get_lookup_refid` function.
#' @param data A data frame or data.table containing the variables to be processed.
#' @return A data.table with factor variables transformed.
#' @details
#' It also depends on a global `lookups` data.table and the `get_lookup_refid` function.
#' For non-"Yes"/"No" factors, the `ref_type` passed to `get_lookup_refid` is assumed
#' to be the column name itself (e.g., for `project_type`, `ref_type` is "project_type").
factor_vars_db_prep <- function(data) {
  # Initialize a list to hold expressions for collapse::fmutate
  mutate_expressions <- list()

  # Get names of factor columns
  factor_cols <- names(data)[sapply(data, is.factor)]
  if(length(factor_cols) == 0) return(data)
  
  for (col_name in factor_cols) {
    col_levels <- levels(data[[col_name]])
    
    # Convert "Yes"/"No" factor to boolean
    if (identical(levels(data[[col_name]]), c("Yes","No"))) {
      mutate_expressions[[col_name]] <- bquote(.(as.name(col_name)) == "Yes")
    } else {
      # Convert to numeric using get_lookup_refid
      mutate_expressions[[col_name]] <- bquote(get_lookup_refid(.(as.name(col_name)), .(col_name)))
    }
  }

  # Apply mutations
  return(do.call(fmutate, c(list(data), mutate_expressions)))
}

# Indicates how to display certain varibales in a more readable way
variable_labels <- c(
  "project_id" = "Project ID",
  "organization_name" = "Organization Name",
  "project_name" = "Project Name",
  "project_type" = "Project Type",
  "target_population" = "Target Population",
  "mckinneyvento" = "McKinney-Vento",
  "mckinneyventoyhdp" = "McKinney-Vento: YHDP",
  "dv_renewal" = "DV Renewal",
  "grant_number" = "Grant Number",
  "coc_amount_awarded_last_year" = "CoC Amount Awarded Last Operating Year",
  "coc_amount_expended_last_year" = "CoC Amount Expended Last Operating Year",
  "coc_funding_requested" = "CoC Funding Requested",
  "funding_action" = "Funding Action",
  "geocode" = "Geo Code",
  "all_fam_beds" = "All Fam Beds",
  "dv_fam_beds" = "DV Fam Beds",
  "ch_fam_beds" = "CH Fam Beds",
  "vet_fam_beds" = "Vet Fam Beds",
  "par_youth_beds" = "Par Youth Beds",
  "beds_hh_wo_children" = "Beds HH w/o Children",
  "beds_hh_w_only_children" = "Beds HH w/ only Children",
  "all_ind_beds" = "All Ind Beds",
  "dv_ind_beds" = "DV Ind Beds",
  "ch_beds_hh_wo_children" = "CH Beds HH w/o Children",
  "ch_beds_hh_w_only_children" = "CH Beds HH w/ only Children",
  "total_ch_ind_beds" = "Total CH Ind Beds",
  "vet_ind_beds" = "Vet Ind Beds",
  "single_youth_beds" = "Single Youth Beds",
  "is_dedicated_ch_fam" = "Is 100% Dedicated + or CH Fam (Yes/No)",
  "is_dedicated_ch_ind" = "Is 100% Dedicated + or CH Ind (Yes/No)",
  "is_dedicated_dv" = "Is 100% DV (Yes/No)",
  "amount_other_public_funding" = "Other public funding amount",#(federal, state, county, city)",
  "amount_private_funding" = "Private funding amount",
  "ch_bed_inventory" = "CH Bed Inventory (PSH Only)",
  "vet_bed_inventory" = "Veteran Bed Inventory",
  "youth_bed_inventory" = "Youth Bed Inventory",
  "created_by" = "Created By",
  "date_created" = "Date Created",
  "date_updated" = "Date Updated",
  "updated_by" = "Updated By",
  "met_hud_thresholds" = "Met HUD Thresholds",
  "met_coc_thresholds" = "Met CoC Thresholds",
  "weighted_score" = "Weighted Rating Score (out of 100)"
)

inventory_variable_labels <- variable_labels[!(names(variable_labels) %in% c('met_hud_thresholds', 'met_coc_thresholds', 'weighted_score'))]

initial_cols_to_show <- setdiff(names(inventory_variable_labels), c('created_by','date_created','date_updated','updated_by'))
                                                                  

giw_variable_labels <- c(
  "grant_number" = "Grant Number",
  "coc" = "CoC",
  "applicant_name" = "Applicant Name",
  "project_name" = "Project Name",
  "expiration_year" = "Expiration Year",
  "project_component" = "Project Component",
  "restriction_dv_or_ydhp" = "Restriction: DV or YHDP",
  "dv_ard_estimated" = "DV ARD Estimated",
  "yhdp_ard_estimated" = "YHDP ARD Estimated",
  "cocs_ard_estimated" = "CoCs ARD Estimated",
  "total_units" = "Total Units",
  "total_ara" = "Total ARA",
  "date_created" = "Date Created",
  "created_by" = "Created By",
  "date_updated" = "Date Updated",
  "updated_by" = "Updated By"
)

versions_variable_labels <- c(
  "coc" = "CoC Code",
  "coc_name" = "CoC Name",
  "coc_version_name" = "CoC Version Name",
  "coc_status" = "Status",
  "coc_version_id" = "CoC Version ID",
  "coc_version_role" = "Your Role",
  "updated_by" = "Updated By",
  "created_by" = "Created By",
  "date_updated" = "Date Updated",
  "created_by" = "Created By",
  "date_created" = "Date Created"
)

requests_variable_labels <- c(
  "coc_version_id" = "coc_version_id",
  "coc_request_id" = "coc_request_id",
  "coc" = "CoC Code",
  "coc_name" = "CoC Name",
  "coc_version_name" = "CoC Version Name",
  "request_status" = "Request Status",
  "created_by" = "Requested By",
  "date_created" = "Date Requested",
  "date_updated" = "Date Updated"
)

add_user_stamp <- function(x, user_coc, is_new = FALSE) {
  x <- x |> fmutate(updated_by = user_coc$username)
  if(is_new) x <- x |> fmutate(created_by = user_coc$username)
  return(x)
}

add_datetime_stamp <- function(x, is_new = FALSE) {
  x <- x |> fmutate(date_updated = get_db_timestamp())
  if(is_new) x <- x |> fmutate(date_created = get_db_timestamp())
  return(x)
}

insert_and_return <- function(p, table, new_dt, return_cols) {
  col_list <- paste(DBI::dbQuoteIdentifier(p, names(new_dt)), collapse = ", ")
  return_col_list <- paste(DBI::dbQuoteIdentifier(p, return_cols), collapse = ", ")
  placeholders <- paste0("$", seq_along(names(new_dt)), collapse = ", ")
  
  results <- DBI::dbGetQuery(
    p, 
    sprintf(
      "INSERT INTO %s (%s) VALUES (%s) RETURNING %s",
      table,
      col_list,
      placeholders,
      return_col_list
    ), 
    params = paramify(new_dt)
  ) |>
    qDT()
  
  return(results)
}

format_timestamp <- function(t) {
  strftime(t, format = "%Y-%m-%d %H:%M:%S")
}


get_db_timestamp <- function() {
  format_timestamp(Sys.time())
}

save_to_db <- function(p, sql, params, tbl_name) {
  paramified <- paramify(params)
  if(purrr::every(paramified, is.null)) 
    return(FALSE)
  
  save_func <- function(conn) {
    if(!grepl("RETURNING ", sql)) {
      DBI::dbExecute(conn, sql, params = paramified)
    } else {
      DBI::dbGetQuery(conn, sql, params = paramified)
    }
  }
  
  tryCatch({
    # if not already inside a transaction (i.e. where p is a connection, wrap in transaction for speed)
    rows_changed <- if(inherits(p, "Pool")) {
      pool::poolWithTransaction(p, function(conn) {
        save_func(conn)
      })
    } else {
      # otherwise, we're in a transaction, so just save
      save_func(p)
    }
    
    if(grepl("RETURNING ", sql)) {
      if(is.null(rows_changed))
        msg <- glue::glue("Someone recently edited this {tbl_name}! Refreshing your view. Resubmit when you're ready.")
      else
        msg <- glue::glue("{tbl_name} saved successfully!")
      print(msg)
      print(sql)
      showNotification(msg, type = "message")
      return(rows_changed)
    } 
    
    num_rows <- ifelse("list" %in% class(params), length(params[[1]]), fnrow(params))
    if(rows_changed == 0) {
      msg <- glue::glue("Someone recently edited this {tbl_name}! Refreshing your view. Resubmit when you're ready.")
      print(sql)
      needs_refresh <- TRUE
    } else if(rows_changed < num_rows) {
      msg <- glue::glue("Someone recently edited one or more {tbl_name} for this project! Refreshing your view. Resubmit when you're ready.")
      needs_refresh <- TRUE
    } else {
      msg <- glue::glue("{tbl_name} saved successfully!")
      print(sql)
      needs_refresh <- FALSE
    }
    logger::log_info(msg)
    showNotification(msg, type = "message")
    return(needs_refresh)
  }, error = function(e) {
    # If an error occurs, do NOT reset the flag, so it will try again.
    # Notify the user of the failure.
    showNotification(glue::glue("Error saving {tbl_name}: {e$message}"), type = "error", duration = 10)
    logger::log_error(paste0(sql, e$message))
    stop(e) # rethrow error so the transaction can catch it and roll back
  })
}

# make sure data are SQL/db ready, i.e. no dfs or named lists
paramify <- function(p) {
  p |>
    as.list() |>
    unname()
}

# Dynamically add the optimistic locking SQL code. This makes future updates easier
add_optimistic_locking <- function(sql) {
  # 1. Extract table name from "INSERT INTO tbl_name ("
  tbl_name <- regmatches(sql, regexpr("(?<=INSERT INTO )\\w+", sql, perl = TRUE))
  
  # 2. Count how many $N placeholders are already in the SQL
  existing_params <- regmatches(sql, gregexpr("\\$\\d+", sql, perl = TRUE))[[1]]
  n <- length(unique(existing_params))
  
  # 3. Append date_updated and WHERE clause
  sql_with_locking <- glue::glue(
    "  {trimws(sql, which = 'right')},\n
      date_updated = CURRENT_TIMESTAMP,
      version_id = {tbl_name}.version_id + 1
    WHERE {tbl_name}.version_id = ${n + 1}
      OR (${n + 1} IS NULL AND {tbl_name}.version_id IS NULL)"
  )
  
  sql_with_locking
}

## Revert cell to original value -----
get_old_val <- function(info, visible_rows, full_data, server=TRUE) {
  if("oldValue" %in% info) 
    return(info$oldValue)
  else
    return(full_data[info$row, info$col + 1, with=FALSE])
  #   
  # else if(server) {
  #   browser()
  #   # Map displayed row -> actual row index
  #   actual_row_index <- whichv(visible_rows, info$row)
  #   
  #   # Get the ID
  #   order <- seq_row(full_data)
  #   row_id <- order[info$row]
  #   
  #   # Now find the row in your full dataset using the ID
  #   true_row <- whichv(order, order[info$row])
  #   
  #   return(full_data[info$row, info$col + 1, with=FALSE])
  # } else {
  #   browser()
  #   return(full_data[info$row, info$col + 1, with=FALSE])
  # }
  
}
revert_cell <- function(tableID, info, visible_rows, full_data) {
  # replaceData(projects_table_proxy, projects_data(), resetPaging = FALSE)
  # info$oldValue works when handled via js because we pass that value
  # otherwise, when server=FALSE, we can grab from the not-yet-updated reactive
  # if server=TRUE, then we need to determine the actual row in case user filtered 
  
  oldVal <- get_old_val(info, visible_rows, full_data, server=TRUE)
  shinyjs::runjs(sprintf(
    "
              var table = $('#%s table').DataTable();
              table.cell(%s, %s).data('%s').draw(false);
            ",
    tableID,
    info$row - 1,
    info$col,
    jsonlite::toJSON(oldVal, auto_unbox = TRUE)
  ))
}

not_equal_na <- function(x, y) {
  if (is.character(x)) x[x == ""] <- NA_character_
  if (is.character(y)) y[y == ""] <- NA_character_
  
  is.na(x) != is.na(y) | (!is.na(x) & !is.na(y) & x != y)
}

get_rating_data_to_save <- function(input_vals, base, id_col, input_prefixes) {
  new_data <- base |> fselect(id_col)
  for (prefix in input_prefixes) {
    input_name <- paste0(prefix, "_", base[[id_col]])
    new_data[[prefix]] <- unlist(input_vals[input_name])
  }
  
  if(!any(input_prefixes %in% names(new_data))) return(NULL)
  
  diffs <- join(
    new_data, 
    base,
    on = id_col
  ) %>%
    fsubset(
      Reduce(
        `|`,
        lapply(input_prefixes, function(col) {
          not_equal_na(.[[col]], .[[paste0(col, "_base")]])
        })
      )
    ) |>
    fselect(
      c(id_col, input_prefixes, "version_id")
    )
  
  if(fnrow(diffs) == 0) return(NULL)
  
  diffs |>
    fselect(
      c(id_col, input_prefixes, "version_id")
    )
}

get_threshold_data_to_save <- function(base, id_col, valuecol, selections) {
  new_val <- as.integer(base[[id_col]] %in% selections)
  diff <- base |> 
    fmutate(new_val = new_val) %>%
    fsubset(new_val != fcoalesce(DT::coerceValue(.[[valuecol]], new_val), 0L))
  
  diff[[valuecol]] <- diff$new_val
  diff$new_val <- NULL
  
  if(fnrow(diff) == 0) return(NULL)
  return(diff)
}