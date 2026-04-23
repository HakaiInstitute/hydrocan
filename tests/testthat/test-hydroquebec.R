test_that("hydrocan_adapter_hydroquebec creates a valid adapter", {
  a <- hydrocan:::hydrocan_adapter_hydroquebec()
  expect_s3_class(a, "hydrocan_adapter")
  expect_equal(a$name, "hydroquebec")
  expect_true(is.function(a$list_stations_fn))
  expect_true(is.function(a$fetch_flows_fn))
})

test_that("HQ station list returns a non-empty character vector", {
  skip_if_offline()
  stations <- hydrocan:::hydrocan_adapter_hydroquebec()$list_stations_fn()
  expect_type(stations, "character")
  expect_gt(length(stations), 0L)
})

test_that("HQ fetch returns a valid schema for a known station", {
  skip_if_offline()
  end_date <- Sys.Date()
  start_date <- end_date - 5L
  result <- hc_read_flows(
    station_number = "3-230",
    start_date = start_date,
    end_date = end_date,
    source = "hydroquebec"
  )
  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c(
      "station_number",
      "datetime",
      "value",
      "parameter",
      "units",
      "source",
      "approval",
      "quality_flag"
    )
  )
  expect_true(nrow(result) > 0L)
  expect_equal(unique(result$parameter), "flow")
  expect_equal(unique(result$source), "hydroquebec")
  expect_true(all(is.na(result$approval)))
})

test_that("HQ daily aggregation works end-to-end", {
  skip_if_offline()
  end_date <- Sys.Date()
  start_date <- end_date - 3L
  result <- hc_read_daily_flows(
    station_number = "3-230",
    start_date = start_date,
    end_date = end_date,
    source = "hydroquebec"
  )
  expect_s3_class(result, "tbl_df")
  expect_true("date" %in% names(result))
  expect_s3_class(result$date, "Date")
})
