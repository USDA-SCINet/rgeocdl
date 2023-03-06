test_that("invalid datasets return GeoCDL message", {
  expect_condition(view_metadata('PRIS'),'Invalid dataset ID')
})
