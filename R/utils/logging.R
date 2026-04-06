log_error <- function(msg) {
  if(IN_DEV_MODE) {
    print(msg)
  }
  logger::log_error(msg)
}