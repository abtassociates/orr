## retrieve individual user setting value from DB
get_user_setting <- function(p, setting_nm, username){
  
  if(p$valid){
    get_db_query(
      "SELECT setting_value FROM user_settings WHERE coc_user = $1 AND setting_name = $2",
      params = list(username,
                    setting_nm)
    ) |> unlist(use.names = FALSE)
  } else {
    return(character(0))
  }
  
}

get_version_setting <- function(p, setting_nm, coc_version_id, username){
  
  if(p$valid){
    get_db_query(
      "SELECT setting_value FROM version_settings WHERE coc_version_id = $1 AND coc_user = $2 AND setting_name = $3",
      params = list(coc_version_id,
                    username,
                    setting_nm)
    ) |> unlist(use.names = FALSE)
  } else {
    return(character(0))
  }
  
}

## on app exit, update individual settings
update_single_version_setting <- function(p, user_coc, setting_nm, setting_val){
  
  if(is.null(isolate(setting_val)))
    return(NULL)
  
  ## new changes
  updated_settings <- data.frame(
    'coc_version_id' = isolate(user_coc$coc_version_id),
    'coc_user' = isolate(user_coc$username),
    'setting_name' =  setting_nm,
    'setting_value' = isolate(setting_val),
    'created_by' = isolate(user_coc$username)
  )

  save_to_db(
    p,
    paste0(
      "INSERT INTO version_settings (coc_version_id, coc_user, setting_name, setting_value, created_by, updated_by)
            VALUES ($1, $2, $3, $4, $5, $5)
            ON CONFLICT (coc_version_id, coc_user, setting_name) DO UPDATE SET
              setting_value = EXCLUDED.setting_value,
              updated_by = EXCLUDED.created_by,
              date_updated = CURRENT_TIMESTAMP",
      "\nRETURNING user_setting_id, setting_name, setting_value, version_id"
    ),
    updated_settings,
    "version_settings"
  )
}

update_single_user_setting <- function(p, user_coc, setting_nm, setting_val){
  
  if(is.null(isolate(setting_val)))
    return(NULL)
  
  ## new changes
  updated_settings <- data.frame(
    'coc_user' = isolate(user_coc$username),
    'setting_name' =  setting_nm,
    'setting_value' = isolate(setting_val),
    'created_by' = isolate(user_coc$username)
  )
  
  save_to_db(
    p,
    paste0(
      "INSERT INTO user_settings (coc_user, setting_name, setting_value, created_by, updated_by)
            VALUES ($1, $2, $3, $4, $4)
            ON CONFLICT (coc_user, setting_name) DO UPDATE SET
              setting_value = EXCLUDED.setting_value,
              updated_by = EXCLUDED.created_by,
              date_updated = CURRENT_TIMESTAMP",
      "\nRETURNING user_setting_id, setting_name, setting_value, version_id"
    ),
    updated_settings,
    "user_settings"
  )
}

## on app exit, update all settings
update_all_user_settings <- function(user_coc, tab_name){
  
  if(!isolate(user_coc$auth))
    return(NULL)
  
  if(is.null(isolate(user_coc$coc_version_id)))
    return(NULL)
 
  p <- get_db_pool()
  
  if(!p$valid)
    return(NULL)
  
  existing_settings <-  dbGetQuery(p,
                                   'SELECT * FROM user_settings WHERE coc_user = $2', 
                                   params = list(isolate(user_coc$username)))
  
  
  rating_settings <- c('rating_method','rating_tab','rating_subtab', 'rating_renew_project_selected', 'rating_new_project_selected')
  
  ## save ratings navigation user settings
  lapply(rating_settings, 
         function(x){
           update_single_version_setting(p, user_coc, x, user_coc$version_settings[[x]])
  })
  
  ## save current tab 
  update_single_user_setting(p, user_coc, 'active_tab', tab_name)
  
  if(is.null(isolate(user_coc$settings$cols_to_hide)))
    return(NULL)
  
  ## save project table fields to hide
 
  # check if a column was previously hidden
  disp_existing <- fsubset(existing_settings, grep('disp_', setting_name)) 
  
  if(fnrow(disp_existing) > 0){

    current_selection <- isolate(user_coc$settings$cols_to_hide)
    previous_selection <-  get_project_fields_to_hide(get_db_pool(), isolate(user_coc$username))


    to_add <- setdiff(current_selection, gsub('disp_', '', previous_selection))
    to_remove <- setdiff(gsub('disp_', '', previous_selection), current_selection)

    if(length(to_add) > 0){
      to_add <- paste0('disp_', to_add)
      
      update_single_user_setting(p, user_coc, setting_nm = to_add, setting_val = 'hide')
    }

    if(length(to_remove) > 0){
      to_remove <- paste0('disp_', to_remove)

      sapply(to_remove,
             function(x){
               dbExecute(p,
                 'DELETE FROM user_settings WHERE coc_user = $1 AND setting_name = $3',
                 params = list(isolate(user_coc$username), x)
               )
             }
      )
    }
  } else {
    
      update_single_user_setting(p, user_coc, setting_nm = paste0('disp_', isolate(user_coc$settings$cols_to_hide)), setting_val = 'hide')
  }
}