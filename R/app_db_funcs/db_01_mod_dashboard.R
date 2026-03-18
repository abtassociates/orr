get_coc_versions_for_user <- function(username) {
  get_db_query(
    "SELECT v.*, u.username, u.coc_version_role, v.created_by
    FROM coc_versions v
    LEFT JOIN coc_version_users u
    ON v.coc_version_id = u.coc_version_id
    WHERE u.username = $1
    ",
    params = username
  )
}