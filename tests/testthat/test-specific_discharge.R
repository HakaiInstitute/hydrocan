test_that("specific_discharge returns a correct value", {
  expect_equal(
    specific_discharge(flow = 10, drainage_area = 40),
    0.25
  )
})

test_that("specific_discharge returns a numeric", {
  expect_type(
    specific_discharge(flow = 10, drainage_area = 40),
    "double"
  )
})
