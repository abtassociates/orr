get_project_fields_to_hide <- function(p, coc_version_id, username){
  
  if(p$valid){
    get_db_query(
      "SELECT setting_name FROM user_settings WHERE coc_version_id = $1 AND coc_user = $2 AND setting_value = 'hide' AND setting_name LIKE 'disp_%'",
      params = list(coc_version_id, username)
    ) |> unlist(use.names = FALSE)
  } else {
    return(character(0))
  }
}

update_inventory_db <- function(new_value, col_name, proj_id, version_id) {
  sql <- if(col_name == "funding_action" && new_value == "New") {
    glue::glue(
      "UPDATE projects 
      SET 
        {col_name} = $1, 
        grant_number = NULL, 
        coc_amount_awarded_last_year = NULL, 
        coc_amount_expended_last_year = NULL,
        version_id = version_id + 1
      WHERE project_id = $2 AND version_id = $3")
  } else {
    glue::glue(
      "UPDATE projects 
      SET 
        {col_name} = $1,
        version_id = version_id + 1
      WHERE project_id = $2 AND version_id = $3")
  }
  save_to_db(
    get_db_pool(),
    sql,
    list(new_value, proj_id, version_id),
    "projects"
  )
  # db_execute(sql, params = list(new_value, proj_id, version_id))
}
