get_all_requests_by_user <- function(username) {
  owner_role <- get_lookup_refid("Owner", ref_type = "coc_version_role")
  get_db_query(
    "SELECT DISTINCT
      coc_request_id, 
      coc_version_name, 
      coc_name, 
      coc, 
      cvr.coc_version_id, 
      u.coc_version_id AS u_coc_version_id,
      cvr.created_by, 
      cvr.date_created, 
      cvr.request_status,
      cvr.date_updated
    FROM coc_version_requests cvr 
    LEFT JOIN coc_versions cv ON cvr.coc_version_id = cv.coc_version_id
    LEFT JOIN cocs c ON cv.coc = c.coc_code
    FULL JOIN coc_version_users u ON cv.coc_version_id = u.coc_version_id
    WHERE cvr.created_by = $1 OR (u.username = $2 AND u.coc_version_role = $3)
    ORDER BY request_status DESC;",
    params = list(username, username, owner_role)
  ) |>
    fmutate(
      coc_version_id = fcoalesce(coc_version_id, u_coc_version_id)
    ) |>
    fselect(-u_coc_version_id)
}

update_request_status <- function(p, update_params) {
  DBI::dbExecute(
    p,
    "UPDATE coc_version_requests 
    SET request_status = $1, updated_by = $2, reason_for_rejection = $3, date_updated = CURRENT_TIMESTAMP
    WHERE coc_request_id = $4 AND date_updated = $5",
    params = paramify(update_params)
  )
}
