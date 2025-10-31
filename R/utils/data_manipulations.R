
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

project_variable_labels <- c(
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
  "amount_other_public_funding" = "Amount of other public funding (federal, state, county, city)",
  "amount_private_funding" = "Amount of private funding",
  "ch_bed_inventory" = "CH Bed Inventory (PSH Only)",
  "vet_bed_inventory" = "Veteran Bed Inventory",
  "youth_bed_inventory" = "Youth Bed Inventory",
  "created_by" = "Created By",
  "date_created" = "Date Created"
)

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
  "coc_version_name" = "CoC Version Name",
  "coc_status" = "Status",
  "coc_version_id" = "CoC Version ID",
  "coc_version_role" = "Your Role",
  "updated_by" = "Updated By",
  "date_updated" = "Date Updated"
)

add_user_stamp <- function(x, user_coc, is_new = FALSE) {
  x <- x |> fmutate(updated_by = user_coc$email)
  if(is_new) x <- x |> fmutate(created_by = user_coc$email)
  return(x)
}

insert_and_return <- function(table, new_dt, return_cols) {
  col_list <- paste(DBI::dbQuoteIdentifier(DB_CON, names(new_dt)), collapse = ", ")
  return_col_list <- paste(DBI::dbQuoteIdentifier(DB_CON, return_cols), collapse = ", ")
  placeholders <- paste0("$", seq_along(names(new_dt)), collapse = ", ")

  sql <- sprintf(
    "INSERT INTO %s (%s) VALUES (%s) RETURNING %s",
    table,
    col_list,
    placeholders,
    return_col_list
  )
  
  results <- lapply(1:nrow(new_dt), function(i) {
    row_values <- as.character(unname(new_dt))
    DBI::dbGetQuery(DB_CON, sql, params = as.list(row_values))
  })

  return(results)
}
