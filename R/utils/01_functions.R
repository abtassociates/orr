## Retrieve latest users and CoC versions data
get_all_users_and_versions <- function(params) {
  get_db_query(
    "SELECT v.*, u.username, u.coc_version_role
            FROM coc_versions v
            LEFT JOIN coc_version_users u
            ON v.coc_version_id = u.coc_version_id"
  ) |>
    fsubset(username == params$username, -created_by) |>
    fmutate(
      coc_version_role = get_lookup_label(coc_version_role, 'coc_version_role', LOOKUPS = params$LOOKUPS),
      coc_status = get_lookup_label(coc_status, 'coc_status', LOOKUPS = params$LOOKUPS)
    ) |>
    join(
      params$coc_tbl %>% fselect(coc_code, coc_name),
      how = 'left', 
      on = c('coc' = 'coc_code')
    ) |>
    colorder(coc, coc_name, pos = "after")
}

## Add new CoC Version for current user
create_new_version_for_user <- function(params) {
  
  new_version <- params$new_version_data |>
    fmutate(coc_status = get_lookup_refid("Not Started", "coc_status")) |>
    add_user_stamp(params$username, is_new = TRUE)
  
  # Update CoC Version in db, and grab autonumbered coc_version_id
  new_coc_version_info <- insert_and_return(
    "coc_versions", new_version %>% fselect(-coc_name), c("coc_version_id", "date_updated")
  )
  
  new_version_user <- data.table(
    coc_version_id = unlist(new_coc_version_info)[["coc_version_id"]],
    username = params$username,
    coc_version_role = as.character(get_lookup_refid("Owner","coc_version_role"))
  ) |>
    add_user_stamp(params$username, is_new = TRUE)
  
  # Next, update CoC Version USers in db
  db_append('coc_version_users', new_version_user)
  
  # update reactiveVal
  if(params$update_rv){
    coc_vu(
      rbind(
        copy(coc_vu()), 
        new_version |>
          fmutate(
            coc_version_id = new_version_user$coc_version_id,
            coc_version_role = new_version_user$coc_version_role,
            coc_status = get_lookup_label(coc_status, "coc_status"),
            coc_version_role = get_lookup_label(coc_version_role, "coc_version_role"),
            date_updated = as.POSIXct(new_coc_version_info[[1]]$date_updated),
            date_created = as.POSIXct(new_coc_version_info[[1]]$date_updated)
          ),
        fill=TRUE
      ) %>% fselect(-created_by)
    )
  }
  
  return(new_version_user$coc_version_id)
}

## Request CoC Version access from another user, update DB table
create_request <- function(params) {
  request_status_num <- get_lookup_refid('Sent','request_status')
  
  request_row <- data.table(
    #coc_request_id = 1 + (get_db_tbl('coc_version_requests') |> fnrow()),
    coc_version_id = params$version_id,
    request_status = request_status_num,
    reason_for_rejection = NA,
    date_created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    date_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
    add_user_stamp(params$username, is_new = TRUE)
  
  # Add row to requests table
  db_append("coc_version_requests", request_row)
}

## Retrieve projects DB table and reformat
get_hic_data <- function(params) {
  bed_field_mapping <- c(
    all_fam_beds = "beds_hh_w_children", 
    ch_fam_beds = "ch_beds_hh_w_children",
    vet_fam_beds = "veteran_beds_hh_w_children", 
    par_youth_beds = "youth_beds_hh_w_children",
    vet_ind_beds = "veteran_beds_hh_wo_children",
    single_youth_beds = "youth_beds_hh_wo_children"
  )
  
  coc_data <- get_db_tbl("all_hic_data") |>
    fsubset(hudnum == params$coc) 
  
  ## REFACTOR INTO SEPARATE FUNCTION SCRIPT
  project_data <- coc_data %>% # %>% needed for gvr to work
    fmutate(
      mckinneyvento = factor_yesno(rowSums(gvr(., "mckinneyvento"), na.rm = TRUE) > 0),
      mckinneyventoyhdp = factor_yesno(mckinneyventoyhdp),
      dv_renewal = factor_yesno(NA),
      grant_number = as.character(NA), 
      coc_amount_awarded_last_year = as.numeric(NA),
      coc_amount_expended_last_year = as.numeric(NA),
      coc_funding_requested = as.numeric(NA),
      funding_action = fifelse(mckinneyvento == "Yes", "Renew", "Ignore"),
      coc_version_id = params$coc_version_id,
      # additional cols user will fill out
      is_dedicated_ch_fam = factor_yesno(NA),
      is_dedicated_ch_ind = factor_yesno(NA),
      is_dedicated_dv = factor_yesno(NA),
      amount_other_public_funding = as.numeric(NA),
      amount_private_funding = as.numeric(NA),
      all_ind_beds = beds_hh_wo_children + beds_hh_w_only_children,
      total_ch_ind_beds = ch_beds_hh_wo_children + ch_beds_hh_w_only_children,
      dv_fam_beds = fifelse(target_population == "DV", beds_hh_w_children, as.integer(0)),
      dv_ind_beds = fifelse(target_population == "DV", all_ind_beds, as.integer(0))
    ) %>% # %>% needed for convert_to_factor to work
    fmutate(
      funding_action = convert_to_factor(., "funding_action", textToNum = T),
      project_type = convert_to_factor(., "project_type", textToNum = F),
      target_population = convert_to_factor(., "target_population", textToNum = F),
      created_by = SERVICE_ACCOUNT
    ) |>
    frename(bed_field_mapping) |>
    get_vars(setdiff(dbListFields(DB_POOL, "projects"), "project_id"))
  
  return(project_data)
}

## Update single request
update_single_request <- function(row, request_status_num, params) {
  
  
  if(request_status_num == 2){
    
    # Set Status in Requests table
    db_execute(
      "UPDATE coc_version_requests 
          SET request_status = $1, date_updated = CURRENT_TIMESTAMP, updated_by = $2
          WHERE coc_request_id = $3", 
      params = list(request_status_num, params$username, row[["coc_request_id"]])
    )
    
    # Create version user
    user_role_num <- get_lookup_refid("Editor", "coc_version_role")
    db_append(
      "coc_version_users",
      data.table(
        coc_version_id = row[["coc_version_id"]],
        username = row[["created_by"]],
        coc_version_role = user_role_num,  # Owner, Owner, Editor, Owner
        created_by = params$username,
        date_created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        date_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        updated_by = params$username
      )
    )
  } else if(request_status_num == 3){
    # Set Status in Requests table
    db_execute(
      "UPDATE coc_version_requests 
          SET request_status = $1, date_updated = CURRENT_TIMESTAMP, updated_by = $2,
              reason_for_rejection = $3
          WHERE coc_request_id = $4", 
      params = list(request_status_num, params$username, params$rej_reason, row[["coc_request_id"]])
    )
  }
}

# Updating all requests DB table
update_requests <- function(params, cur_requests) {
  request_status_num <- get_lookup_refid(params$status, "request_status")
  selected_requests <- cur_requests()[params$rows_selected]
  
  ## REFACTOR INTO SEPARATE FUNCTION SCRIPT
  apply(selected_requests, 1, update_single_request, request_status_num = request_status_num, params = params)
  
  if(params$update_rv){
    # Update datatable proxy
    cur_requests(
      cur_requests() |> 
        fmutate(
          request_status = fifelse(
            coc_request_id %in% selected_requests$coc_request_id, 
            params$status, 
            request_status
          )
        )
    )
  }
}