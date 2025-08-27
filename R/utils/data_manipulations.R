
pluralize <- function(s) {
  ends <- "(sh?|x|z|ch)$"
  pluralify <- ifelse(grepl(ends, s, perl = TRUE), "es", "s")
  out <- gsub("ys$", "ies", paste0(s, pluralify))
  return(out)
}


factor_yesno <- function(v) {
  factor(
    v,
    levels = c(1,0),
    labels = c("Yes", "No")
  )
}

get_labelled_lookups <- function(l, lookup_col = "value") {
  lookup_info <- lookups[reference_type == l]
  setNames(lookup_info$reference_id, lookup_info[[lookup_col]])
}
get_lookup_label <- function(v, ref_type, lookup_col = "value") {
  filtered_lookups <- lookups[reference_type == ref_type]
  if(is.character(v)) {
    filtered_lookups[reference_id == v, get(lookup_col)]
  } else {
    filtered_lookups[.(v), get(lookup_col), on = "reference_id"]
  } 
}

get_lookup_refid <- function(v, ref_type, lookup_col = "value") {
  filtered_lookups <- lookups[reference_type == ref_type]
  if(is.character(v)) {
    r <- filtered_lookups[get(lookup_col) == v, "reference_id"]
  } else {
    r <- filtered_lookups[.(v), get(lookup_col), on = "reference_id"]
  } 
  as.integer(r)
}

convert_to_factor <- function(data, v, textToNum = FALSE, label_col = "value") {
  lookup_info <- lookups[reference_type == v, .(reference_id, value, value_abbrev, value_long)]
  
  factor(
    if(!textToNum) data[[v]] else lookup_info$reference_id[match(data[[v]], lookup_info[[label_col]])],
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