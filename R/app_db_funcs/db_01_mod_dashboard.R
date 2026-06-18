get_all_coc_versions_and_users <- function(username) {
  get_db_query(
    "SELECT v.*, u.username, u.coc_version_role, c.coc_name
    FROM coc_versions v
    LEFT JOIN coc_version_users u ON v.coc_version_id = u.coc_version_id
    LEFT JOIN cocs c ON v.coc = c.coc_code"
  ) |>
    fselect(-dv_ard) |>
    colorder(coc, coc_name, pos = "after")
}