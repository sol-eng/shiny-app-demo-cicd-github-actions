library(shinytest2)

test_that("{shinytest2} recording: test1", {
  app <- AppDriver$new(name = "test1", height = 606, width = 871, variant= platform_variant())
  withr::defer({app$stop()})
  app$expect_values()
  app$set_inputs(api = "Keytruda")
  app$click("submit")
  app$expect_values(output = "plt")
})


test_that("{shinytest2} recording: learn_shinytest2", {
  app <- AppDriver$new(name = "learn_shinytest2", height = 648, width = 1011, variant= platform_variant())
  withr::defer({app$stop()})
  app$set_inputs(end_date = "2018-05-18", api = "Keytruda")
  app$click("submit")
  app$expect_values()
  app$expect_values(output = "tble")
})


test_that("{shinytest2} recording: test2", {
  app <- AppDriver$new(name = "test2", height = 648, width = 1011, variant= platform_variant())
  withr::defer({app$stop()})
  app$set_inputs(end_date = "2018-05-18", api = "Keytruda")
  app$click("submit")
  app$wait_for_idle()
  app$expect_values()
  app$expect_values(output = "tble")
})