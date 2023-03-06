test_that("invalid datasets return GeoCDL message", {
  expect_condition(submit_subset_query('10.1.1.80:8000/subset_polygon?datasets=PRISM',
                                       dsn = '.',
                                       req_name = ''),
                   'Incorrect dataset specification')
})
