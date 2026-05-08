## retrieve individual user setting value from DB
get_user_setting <- function(p, setting_nm, coc_version_id, username){
  if(p$valid){
    get_db_query(
      "SELECT setting_value 
      FROM user_settings 
      WHERE coc_version_id = $1 AND coc_user = $2 AND setting_name = $3",
      params = list(coc_version_id, username, setting_nm)
    ) |> unlist(use.names = FALSE)
  } else {
    return(character(0))
  }
}

update_user_coc_setting <- function(user_coc, setting_name, setting_value) {
  user_coc$settings[[paste0("v", user_coc$coc_version_id)]][[setting_name]] <- setting_value
  
  updated_user_settings <- data.frame(
    coc_version_id = user_coc$coc_version_id,
    coc_user = user_coc$username,
    setting_name = setting_name,
    setting_value = setting_value
  ) |>
    fmutate(
      coc_user = fifelse(setting_name == "rating_method", NA, coc_user)
    )
  
  update_user_settings(get_db_pool(), updated_user_settings)
}


## on app exit, update individual settings
update_user_settings <- function(p, updated_user_settings){
  save_to_db(
    p,
    "INSERT INTO user_settings (coc_version_id, coc_user, setting_name, setting_value, created_by, updated_by)
    VALUES ($1, $2, $3, $4, $2, $2)
    ON CONFLICT (coc_version_id, coc_user, setting_name) DO UPDATE SET
      setting_value = EXCLUDED.setting_value,
      updated_by = EXCLUDED.created_by,
      date_updated = CURRENT_TIMESTAMP
    RETURNING user_setting_id, setting_name, setting_value, version_id",
    updated_user_settings,
    "user_settings"
  )
}

## on app exit, update all settings
update_all_user_settings <- function(user_coc, tab_name){
  # Make sure user is signed in and coc-version is selected
  if(!isolate(user_coc$auth))
    return(NULL)
  
  coc_version_id <- isolate(user_coc$coc_version_id)
  if(is.null(coc_version_id))
    return(NULL)
 
  username <- isolate(user_coc$username)
  settings <- isolate(user_coc$settings[[paste0("v", coc_version_id)]])
  
  updated_user_settings <- data.frame(
    coc_version_id = coc_version_id,
    coc_user = username,
    setting_name = names(settings),
    setting_value = unlist(settings, use.names = FALSE)
  ) |>
    fmutate(
      coc_user = fifelse(setting_name == "rating_method", NA, coc_user)
    )
  
  update_user_settings(get_db_pool(), updated_user_settings)
}
