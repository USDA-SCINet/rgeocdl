test_that("unsupported geometries return GeoCDL message", {
  expect_condition(upload_geometry('test'),
                   'No uploaded polygon data found for GUID ')
})
