library(shinytest2)
library(testthat)

test_that("Basic Shiny app functionality", {
  # Create a new shinytest2 app
  app <- AppDriver$new("../")
  
  # Test initial state
  expect_equal(app$get_title(), "Basic Shiny Application")
  
  # Test that the histogram plot exists
  app$expect_html("#distPlot")
  
  # Test slider input
  app$set_inputs(bins = 20)
  
  # Check that bins input was set correctly
  expect_equal(app$get_value(input = "bins"), 20)
  
  # Test dataset selection
  app$set_inputs(dataset = "iris")
  
  # Check that dataset input was set correctly
  expect_equal(app$get_value(input = "dataset"), "iris")
  
  # Test refresh button
  app$click("refresh")
  
  # Test tab switching - click on Summary tab
  app$click(selector = "a[data-value='Summary']")
  
  # Test that summary output exists
  app$expect_html("#summary")
  
  # Switch to Data Table tab
  app$click(selector = "a[data-value='Data Table']")
  
  # Test that data table exists
  app$expect_html("#dataTable")
  
  # Test different dataset
  app$set_inputs(dataset = "faithful")
  
  # Verify the dataset changed
  expect_equal(app$get_value(input = "dataset"), "faithful")
  
  # Test slider boundary values
  app$set_inputs(bins = 1)
  expect_equal(app$get_value(input = "bins"), 1)
  
  app$set_inputs(bins = 50)
  expect_equal(app$get_value(input = "bins"), 50)
  
  # Test histogram tab navigation
  app$click(selector = "a[data-value='Histogram']")
  app$expect_html("#distPlot")
  
  # Take a screenshot for visual verification
  app$expect_screenshot("app_final_state")
})