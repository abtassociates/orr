## retrieve individual user setting value from DB
get_user_setting <- function(user_coc, setting_nm){
  user_coc$settings[[paste0("v", user_coc$coc_version_id)]][[setting_nm]]
}

update_user_coc_setting <- function(user_coc, setting_name, setting_value) {
  user_coc$settings[[paste0("v", user_coc$coc_version_id)]][[setting_name]] <- setting_value
  
  updated_user_settings <- data.frame(
    coc_version_id = user_coc$coc_version_id,
    coc_user = user_coc$username,
    setting_name = setting_name,
    setting_value = paste(setting_value, collapse = ",")
  ) |>
    fmutate(
      coc_user = fifelse(setting_name == "rating_method", SERVICE_ACCOUNT, coc_user),
      created_by = fifelse(setting_name == "rating_method", SERVICE_ACCOUNT, coc_user),
      updated_by = fifelse(setting_name == "rating_method", SERVICE_ACCOUNT, coc_user)
    )
  
  save_user_settings_db(get_db_pool(), updated_user_settings)
}


## on app exit, update individual settings
save_user_settings_db <- function(p, updated_user_settings){
  paramified <- paramify(updated_user_settings)
  if(purrr::every(paramified, is.null)) 
    return(FALSE)
  
  sql <- "INSERT INTO user_settings (coc_version_id, coc_user, setting_name, setting_value, created_by, updated_by)
    VALUES ($1, $2, $3, $4, $5, $6)
    ON CONFLICT (coc_version_id, coc_user, setting_name) DO UPDATE SET
      setting_value = EXCLUDED.setting_value,
      updated_by = EXCLUDED.created_by,
      date_updated = CURRENT_TIMESTAMP
    RETURNING user_setting_id, setting_name, setting_value, version_id"
  
  DBI::dbExecute(p, sql, params = paramified)
}