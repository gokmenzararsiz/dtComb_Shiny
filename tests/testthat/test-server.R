test_that("Check output", {
  app <- shinytest::ShinyDriver$new("../../")
  app$setInputs(`generalTabSetPanel` = "Data Upload")
  app$setInputs(`generalTabSetPanel` = "Analysis")

  app$setInputs(`goButton` = "click")
  app$setInputs(`tabGroupPlot` = "Results")
  output <- app$getAllValues(input = FALSE,output = TRUE, export = TRUE)
  expect_equal(is.null(output$output$AUCTable), FALSE)
  expect_equal(is.null(output$output$ROCplot), FALSE)
  })
