login_as_user <- function(user_coc, user_email = NULL) {
  
  devs <- list(
    "furmanm" = "marschall.furman@abtglobal.com",
    "silvermana" = "alex.silverman@abtglobal.com"
  )
  
  if(is.null(user_email)) user_email <- devs[[Sys.info()[["user"]]]]
  u <- get_db_tbl("users") |>
    fsubset(username == user_email)
  
  # u <- devs[[Sys.info()[["user"]]]]
  hideElement("login_link")
  hideElement("signup_link")
  user_coc$auth <- TRUE
  user_coc$username <- u$username
  user_coc$given_name <- u$firstname
}