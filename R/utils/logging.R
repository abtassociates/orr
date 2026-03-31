log_error <- function(msg) {
  if(IN_DEV_MODE) {
    browser()
    print(msg)
  }
  logger::log_error(msg)
}