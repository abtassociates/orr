# Utility functions for the application
format_currency <- function(x) {
  paste0("$", format(x, big.mark = ",", scientific = FALSE))
}
