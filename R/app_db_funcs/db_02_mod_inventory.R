get_project_col_names <- function() {
  
  sql <- if(get_db_pool()$objClass[[1]] == "SQLiteConnection") 
    "SELECT name as column_name FROM pragma_table_info('projects');"
  else 
    "SELECT column_name
      FROM information_schema.columns
      AND table_name = 'projects'
      ORDER BY ordinal_position;"
  
  x <- get_db_query(sql)
  
  admin_cols <-  c("version_id", "coc_version_id", "date_created", "date_updated", "updated_by")
  calculated_cols <- c("ch_bed_inventory", "vet_bed_inventory", "youth_bed_inventory")
  setdiff(append(x$column_name, calculated_cols), admin_cols)
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
