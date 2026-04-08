login_as_user <- function(user_coc, user_email = NULL) {

  u <- if(is.null(user_email))
    list(
      username = "alex.silverman@abtglobal.com",
      given_name = "Alex"
    )
  else
    get_db_tbl("users") |>
      fsubset(username == user_email)
  
  # u <- devs[[Sys.info()[["user"]]]]
  hideElement("login_link")
  hideElement("signup_link")
  user_coc$auth <- TRUE
  user_coc$username <- u$username
  user_coc$given_name <- u$given_name
}