## retrieve individual user setting value from DB
get_user_setting <- function(setting_nm, coc_version_id, username){
  get_db_query(
    "SELECT setting_value FROM user_settings WHERE coc_version_id = $1 AND coc_user = $2 AND setting_name = $3",
    params = list(coc_version_id,
                  username,
                  setting_nm)
  ) |> unlist(use.names = FALSE)
}

## on app exit, update individual settings
update_single_user_setting <- function(p, user_coc, setting_nm, setting_val){
  
  if(is.null(isolate(setting_val)))
    return(NULL)
  
  cur_setting_existing <- fsubset(existing_settings, setting_name == setting_nm)
  
  if(fnrow(cur_setting_existing) > 0){
    # modify
    db_execute(
      "UPDATE user_settings SET setting_value = $1, 
        date_updated = CURRENT_TIMESTAMP, updated_by = $2
        WHERE coc_version_id = $3 AND coc_user = $2 AND setting_name = $4", 
      params = list(isolate(setting_val), 
                    isolate(user_coc$username), 
                    isolate(user_coc$coc_version_id),
                    setting_nm)
    )
  } else {
    # add
    print('row does not exist in settings - creating one')
    
    append_df <- data.frame(
      'coc_version_id' = isolate(user_coc$coc_version_id),
      'coc_user' = isolate(user_coc$username),
      'setting_name' =  setting_nm,
      'setting_value' = isolate(setting_val),
      'created_by' = isolate(user_coc$username),
      'updated_by' = isolate(user_coc$username)
    )
    rownames(append_df) <- NULL
    
    db_append("user_settings", append_df)
  }
}

## on app exit, update all settings
update_all_user_settings <- function(user_coc, tab_name){
  
  if(!isolate(user_coc$auth))
    return(NULL)
  
  if(is.null(isolate(user_coc$coc_version_id)))
    return(NULL)
  
  existing_settings <-  dbGetQuery(get_db_pool(),
                                   'SELECT * FROM user_settings WHERE coc_version_id = $1 AND coc_user = $2', 
                                   params = list(isolate(user_coc$coc_version_id),
                                                 isolate(user_coc$username)))
  
  
  settings_to_save <- c('rating_method','rating_tab','rating_subtab', 'rating_renew_project_selected', 'rating_new_project_selected')
  
  ## save ratings navigation user settings
  lapply(settings_to_save, 
         function(x){
           store_single_setting(user_coc, existing_settings, x, user_coc$settings[[x]])
         })
  
  store_single_setting(user_coc, existing_settings, 'active_tab', tab_name)
  
  
  # check if row exists 
  disp_existing <- fsubset(existing_settings, grep('disp_', setting_name)) 
  
  if(fnrow(disp_existing) > 0){
    # modify
    
    current_selection <- isolate(user_coc$settings$cols_to_hide)
    previous_selection <-  get_project_fields_to_display(isolate(user_coc$coc_version_id),
                                                 isolate(user_coc$username))
    
    
    to_add <- setdiff(current_selection, gsub('disp_', '', previous_selection))
    to_remove <- setdiff(gsub('disp_', '', previous_selection), current_selection)
    
    if(length(to_add) > 0){
      to_add <- paste0('disp_', to_add)
      db_append("user_settings",
                data.frame(
                  'coc_version_id' = isolate(user_coc$coc_version_id),
                  'coc_user' = isolate(user_coc$username),
                  'setting_name' = to_add,
                  'setting_value' = 'hide',
                  'created_by' = isolate(user_coc$username),
                  'updated_by' = isolate(user_coc$username)
                )
      )
    }
    
    if(length(to_remove) > 0){
      to_remove <- paste0('disp_', to_remove)
      
      sapply(to_remove,
             function(x){
               db_execute(
                 'DELETE FROM user_settings WHERE coc_version_id = $1 AND coc_user = $2 AND setting_name = $3', 
                 params = list(isolate(user_coc$coc_version_id), isolate(user_coc$username), x)
               )
             }
      )
    }
    
  } else {
    to_add <- isolate(user_coc$settings$cols_to_hide)
    if(length(to_add) > 0){
      to_add <- paste0('disp_', to_add)
      db_append(
        "user_settings",
        data.frame(
          'coc_version_id' = isolate(user_coc$coc_version_id),
          'coc_user' = isolate(user_coc$username),
          'setting_name' = to_add,#paste0('disp_', to_add),
          'setting_value' = 'hide',
          'created_by' = isolate(user_coc$username),
          'updated_by' = isolate(user_coc$username)
        )
      )
    }
  }
  
}