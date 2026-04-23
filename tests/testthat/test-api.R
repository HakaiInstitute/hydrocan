test_that("hc_read_flows returns the correct schema", {
  register_hydrocan_adapter(mock_adapter)
  result <- hc_read_flows(
    "MOCK001",
    start_date = "2024-01-01",
    end_date   = "2024-01-03",
    source     = "mock"
  )
  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("station_number", "datetime", "value", "parameter",
      "units", "source", "approval", "quality_flag")
  )
  expect_s3_class(result$datetime, "POSIXct")
  expect_equal(attr(result$datetime, "tzone"), "UTC")
})

test_that("hc_read_daily_flows returns the correct schema", {
  register_hydrocan_adapter(mock_adapter)
  result <- hc_read_daily_flows(
    "MOCK001",
    start_date = "2024-01-01",
    end_date   = "2024-01-03",
    source     = "mock"
  )
  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("station_number", "date", "value", "parameter",
      "units", "source", "approval", "quality_flag")
  )
  expect_s3_class(result$date, "Date")
  expect_false("datetime" %in% names(result))
})

test_that("hc_read_daily_flows produces one row per day from daily fetch", {
  register_hydrocan_adapter(mock_adapter)
  result <- hc_read_daily_flows(
    "MOCK001",
    start_date = "2024-01-01",
    end_date   = "2024-01-03",
    source     = "mock"
  )
  expect_equal(nrow(result), 3L)
})

test_that("hc_read_daily_flows warns when adapter has no daily support", {
  register_hydrocan_adapter(mock_adapter_flows_only)
  expect_warning(
    result <- hc_read_daily_flows(
      "MOCK001",
      start_date = "2024-01-01",
      end_date   = "2024-01-01",
      source     = "mock_rt_only"
    ),
    "does not support"
  )
  expect_equal(nrow(result), 0L)
})

test_that("hc_read_stations returns the correct schema", {
  register_hydrocan_adapter(mock_adapter)
  result <- hc_read_stations(source = "mock")
  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("station_number", "station_name", "source", "longitude", "latitude",
      "elevation_m", "period_start", "period_end", "notes")
  )
  expect_s3_class(result$period_start, "Date")
  expect_s3_class(result$period_end, "Date")
  expect_type(result$notes, "list")
})

test_that("hc_read_stations returns one row per station", {
  register_hydrocan_adapter(mock_adapter)
  result <- hc_read_stations(source = "mock")
  expect_equal(nrow(result), length(.mock_stations))
})

test_that("hc_read_stations warns and returns empty tibble when adapter has no metadata support", {
  register_hydrocan_adapter(mock_adapter_flows_only)
  expect_warning(
    result <- hc_read_stations(source = "mock_rt_only"),
    "does not support"
  )
  expect_equal(nrow(result), 0L)
})

test_that("hc_read_flows errors on unparseable dates", {
  register_hydrocan_adapter(mock_adapter)
  expect_error(
    hc_read_flows("MOCK001", start_date = "not-a-date", end_date = "2024-01-01", source = "mock"),
    "start_date"
  )
  expect_error(
    hc_read_flows("MOCK001", start_date = "2024-01-01", end_date = "not-a-date", source = "mock"),
    "end_date"
  )
})

test_that("hc_read_flows errors when end_date is before start_date", {
  register_hydrocan_adapter(mock_adapter)
  expect_error(
    hc_read_flows("MOCK001", start_date = "2024-01-10", end_date = "2024-01-01", source = "mock"),
    "before"
  )
})

test_that("hc_read_flows accepts Date objects for date arguments", {
  register_hydrocan_adapter(mock_adapter)
  expect_no_error(
    hc_read_flows(
      "MOCK001",
      start_date = as.Date("2024-01-01"),
      end_date   = as.Date("2024-01-02"),
      source     = "mock"
    )
  )
})
