dbExecute(DB_CON, "DELETE FROM coc_version_users WHERE coc_version_id > 4")
dbExecute(DB_CON, "DELETE FROM coc_versions WHERE coc_version_id > 4")
dbExecute(DB_CON, "DELETE FROM coc_version_requests")

main_user <- toString(users[1, 1])
second_user <- toString(users[3, 1])

coc_versions <- data.table(
  coc_version_id = 10:13,
  coc_version_name = c(
    'AK-500 Main Version',
    'AK-500 Alternative Version',
    'AK-501 Main Version'
  ),
  coc = c('AK-500', 'AK-500', 'AK-501'),
  coc_status = c(9, 8, 9),  # In Progress, Not Started, In Progress
  created_by = c(
    main_user,
    second_user,
    main_user
  ),
  date_created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  date_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  updated_by = main_user
)

# CoC Version Users (many-to-many relationship)
coc_version_users <- data.table(
  coc_version_user_id = 5:8,
  coc_version_id = c(10, 11, 11, 12),
  username = c(
    main_user,
    second_user,
    main_user,
    main_user
  ),
  coc_version_role = c(5, 5, 7, 5),  # Owner, Owner, Editor, Owner
  created_by = c(
    main_user,
    second_user,
    second_user,
    main_user
  ),
  date_created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  date_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  updated_by = main_user
)

# CoC Version Requests (requests to versions where you are Owner)
coc_version_requests <- data.table(
  coc_request_id = 1:2,
  coc_version_id = c(10, 12),  # AK-500 Main and AK-501 Main (where you are Owner)
  requesting_user = c(
    second_user,
    second_user
  ),
  request_status = c(1, 3),  # Sent, Approved
  reason_for_rejection = NA_integer_,
  created_by = c(
    second_user,
    second_user
  ),
  date_created = format(Sys.time() - c(86400, 43200), "%Y-%m-%d %H:%M:%S"),  # 1 day ago, 12 hours ago
  date_updated = format(Sys.time() - c(86400, 43200), "%Y-%m-%d %H:%M:%S"),
  updated_by = main_user
)


dbAppendTable(DB_CON, "coc_versions", coc_versions)
dbAppendTable(DB_CON, "coc_version_users", coc_version_users)
dbAppendTable(DB_CON, "coc_version_requests", coc_version_requests)