get_project_fields_to_hide <- function(coc_version_id, username){
  get_db_query(
    "SELECT setting_name FROM user_settings WHERE coc_version_id = $1 AND coc_user = $2 AND setting_value = 'hide' AND setting_name LIKE 'disp_%'",
    params = list(coc_version_id, username)
  ) |> unlist(use.names = FALSE)
}