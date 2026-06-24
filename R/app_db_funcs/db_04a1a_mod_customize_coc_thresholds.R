get_all_coc_thresholds <- function(coc_version_id) {
  get_db_query(
    "SELECT t.threshold_id, t.threshold_text, st.selected, t.version_id AS threshold_version_id, st.version_id AS selected_threshold_version_id
        FROM thresholds t
        LEFT JOIN selected_thresholds st ON t.threshold_id = st.threshold_id AND st.coc_version_id = $1
        WHERE t.type = 'CoC' AND 
          (t.coc_version_id = $1 OR t.coc_version_id IS NULL)
        ORDER BY t.threshold_id",
    params = list(coc_version_id)
  )
}

update_thresholds_db <- function(p, updated_thresholds) {
  save_to_db(
    p,
    paste0(
      "INSERT INTO thresholds (type, coc_version_id, threshold_text, created_by)
            VALUES ('CoC', $1, $2, $3)
            ON CONFLICT (coc_version_id, threshold_text) DO UPDATE SET
              updated_by = EXCLUDED.created_by
          " |> add_optimistic_locking(),
      "\nRETURNING threshold_id, coc_version_id, version_id"
    ),
    updated_thresholds,
    "thresholds"
  )
}

update_selected_thresholds_db <- function(p, updated_selected_thresholds) {
  save_to_db(
    p,
    "INSERT INTO selected_thresholds (threshold_id, coc_version_id, selected, created_by)
          VALUES ($1, $2, $3, $4)
          ON CONFLICT (coc_version_id, threshold_id) DO UPDATE SET 
            selected = EXCLUDED.selected,
            updated_by = EXCLUDED.created_by, -- Use the 'created_by' value from the attempted insert
        " |> add_optimistic_locking(),
    updated_selected_thresholds,
    "selected_thresholds"
  )
}