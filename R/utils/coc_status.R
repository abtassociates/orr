calculate_coc_status <- function(coc_version_id, ranking_complete = FALSE) {
  pe <- get_project_evaluation(list(coc_version_id, NA))
  coc_status <- get_coc_status(coc_version_id)
  coc_status_label <- get_lookup_label(coc_status, "coc_status")
  
  new_status <- if(coc_status_label == "Rating Complete" && ranking_complete)
    "Complete"
  else if(coc_status_label == "Rating In Progress" && (
    all(pe$rating_complete) &&
    all(pe$threshold_complete)
  )) "Rating Complete"
  
  else if(coc_status_label == "Not Started" & (
    any(pe$rating_complete) ||
    any(pe$threshold_complete)
  )) "Rating In Progress"
  
  else if (
    !any(pe$rating_complete) &&
    !any(pe$threshold_complete)
  ) "Not Started"
 
  return(
    get_lookup_refid(new_status, "coc_status")
  )
}

update_coc_status <- function(user_coc, status) {
  update_coc_version_status(
    params = list(
      status,
      user_coc$username, 
      user_coc$coc_version_id
    )
  )
  
  if(user_coc$coc_status != status) {
    user_coc$coc_status_updated <- user_coc$coc_status_updated + 1
  }
}
