get_coc_projects <- function(coc_version_id) {
  get_db_query(
    "SELECT * FROM projects WHERE coc_version_id = $1", 
    params = coc_version_id
  )
}