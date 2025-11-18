login_as_dev <- function(user_coc) {
  devs <- list(
    "silvermana" = list(
      "email" = "alex.silverman@abtglobal.com",
      "given_name" = "Alex"
    ), 
    "furmanm" = list(
      "email" = "marschall.furman@abtglobal.com",
      "given_name" = "Marschall"
    )
  )
  
  u <- devs[[Sys.info()[["user"]]]]
  hideElement("login_link")
  hideElement("signup_link")
  user_coc$auth <- TRUE
  user_coc$username <- u$email
  user_coc$given_name <- u$given_name
}