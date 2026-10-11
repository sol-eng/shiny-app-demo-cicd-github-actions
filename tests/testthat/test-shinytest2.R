library(shinytest2)

# Value-based integration tests (no screenshots / snapshots): these assert the
# app's exported reactive values and output text, which are deterministic across
# environments, so the suite is a reliable CI deploy gate rather than a flaky
# pixel comparison. The app exports `api_data` via shiny::exportTestValues().

test_that("app launches with the expected inputs and no data yet", {
  app <- AppDriver$new(name = "openfda-initial", height = 800, width = 1200)
  withr::defer(app$stop())

  inputs <- app$get_values()$input
  expect_true(all(c("api", "start_date", "end_date", "submit") %in% names(inputs)))

  # Nothing has been queried, so the exported data is empty.
  expect_null(app$get_value(export = "api_data"))
})

test_that("submitting a known drug returns adverse-event data", {
  app <- AppDriver$new(name = "openfda-query", height = 800, width = 1200)
  withr::defer(app$stop())

  app$set_inputs(api = "Keytruda")
  app$click("submit")
  app$wait_for_idle(timeout = 30000)

  data <- app$get_value(export = "api_data")

  # The app queries the live OpenFDA API (api.fda.gov). If it is unreachable or
  # returns nothing, skip rather than fail, so an external outage never blocks a
  # deploy. When data does come back, assert its shape and the success message.
  skip_if(is.null(data) || !is.data.frame(data) || nrow(data) == 0,
          "OpenFDA returned no data (API unreachable or empty)")

  expect_setequal(colnames(data), c("Rank", "Event", "Count", "Frequency"))
  expect_match(app$get_value(output = "message"), "Download the data", fixed = TRUE)
})
