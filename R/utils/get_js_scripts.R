get_js_script <- function(script_name) {
  paste(
    readLines(
      here(glue::glue("www/js/{script_name}")), 
      warn = FALSE
    ), 
    collapse = "\n"
  )
}