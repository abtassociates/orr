main_test_script("test-main")


main_test_script <- function(test_script_name) {
  test_that(paste0("{shinytest2} recording: ", test_script_name), {
    print(paste0("Running ",test_script_name))
    testthat::local_edition(3)
    
    app <- AppDriver$new(
      variant = platform_variant(),
      name = test_script_name, 
      # screenshot_args = FALSE,
      # expect_values_screenshot_args = FALSE,
      seed = 12345,
      width = 1920,
      height = 1080,
      load_timeout = 2e+05
      # options = list(
      #   shiny.testmode = TRUE
      # )
    )
    
  })
}