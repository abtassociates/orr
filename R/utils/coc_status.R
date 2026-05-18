calculate_coc_status <- function(coc_version_id, project_id) {
  pe <- get_project_evaluation(coc_version_id, project_id)
  coc_status <- get_coc_status(coc_version_id)
  browser()
  if(coc_status == get_lookup_refid("Rating In Progress", "coc_status") && (
    all(pe$rating_complete) &&
    all(pe$threshold_complete)
  )) status = "Rating Complete"
  
  else if(coc_status == get_lookup_refid("Not Started", "coc_status") && (
    any(pe$rating_complete) ||
    any(pe$threshold_complete)
  )) status = "Rating In Progress"
  
  else if (
    !any(pe$rating_complete) &&
    !any(pe$threshold_complete)
  ) status = "Not Started"
 
  return(status) 
}

update_coc_status <- function(user_coc, status) {
  update_coc_version_status(
    params = list(
      get_lookup_refid(status, "coc_status"), 
      user_coc$username, 
      user_coc$coc_version_id
    )
  )
  
  user_coc$coc_status_updated <- user_coc$coc_status_updated + 1
}
