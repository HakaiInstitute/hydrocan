test_that("hc_read_flows returns the correct schema", {
  local_register_adapter(mock_adapter)
  result <- hc_read_flows(
    "TOCHI001",
    start_date = "2024-01-01",
    end_date = "2024-01-03",
    source = "mock"
  )
  expect_s3_class(result, "hydrocan_flows")
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
  expect_s3_class(result$datetime, "POSIXct")
  expect_equal(attr(result$datetime, "tzone"), "UTC")
})

test_that("hc_read_daily_flows returns the correct schema", {
  local_register_adapter(mock_adapter)
  result <- hc_read_daily_flows(
    "TOCHI001",
    start_date = "2024-01-01",
    end_date = "2024-01-03",
    source = "mock"
  )
  expect_s3_class(result, "hydrocan_daily_flows")
  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c(
      "station_number",
      "date",
      "value",
      "parameter",
      "units",
      "source",
      "approval",
      "quality_flag"
    )
  )
  expect_s3_class(result$date, "Date")
  expect_false("datetime" %in% names(result))
})

test_that("hc_read_daily_flows produces one row per day from daily fetch", {
  local_register_adapter(mock_adapter)
  result <- hc_read_daily_flows(
    "TOCHI001",
    start_date = "2024-01-01",
    end_date = "2024-01-03",
    source = "mock"
  )
  expect_equal(nrow(result), 3L)
})

test_that("hc_read_daily_flows warns when adapter has no daily support", {
  local_register_adapter(mock_adapter_flows_only)
  expect_warning(
    result <- hc_read_daily_flows(
      "TOCHI001",
      start_date = "2024-01-01",
      end_date = "2024-01-01",
      source = "mock_rt_only"
    ),
    "does not support"
  )
  expect_equal(nrow(result), 0L)
})

test_that("hc_read_stations returns the correct schema", {
  local_register_adapter(mock_adapter)
  result <- hc_read_stations(source = "mock")
  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c(
      "station_number",
      "station_name",
      "source",
      "longitude",
      "latitude",
      "elevation_m",
      "period_start",
      "period_end",
      "notes"
    )
  )
  expect_s3_class(result$period_start, "Date")
  expect_s3_class(result$period_end, "Date")
  expect_type(result$notes, "list")
})

test_that("hc_read_stations returns one row per station", {
  local_register_adapter(mock_adapter)
  result <- hc_read_stations(source = "mock")
  expect_equal(nrow(result), length(.mock_stations))
})

test_that("hc_read_stations warns and returns empty tibble when adapter has no metadata support", {
  local_register_adapter(mock_adapter_flows_only)
  expect_warning(
    result <- hc_read_stations(source = "mock_rt_only"),
    "does not support"
  )
  expect_equal(nrow(result), 0L)
})

test_that("hc_read_flows errors on unparseable dates", {
  expect_error(
    hc_read_flows(
      "TOCHI001",
      start_date = "not-a-date",
      end_date = "2024-01-01",
      source = "mock"
    ),
    "start_date"
  )
  expect_error(
    hc_read_flows(
      "TOCHI001",
      start_date = "2024-01-01",
      end_date = "not-a-date",
      source = "mock"
    ),
    "end_date"
  )
})

test_that("hc_read_flows errors when end_date is before start_date", {
  expect_error(
    hc_read_flows(
      "TOCHI001",
      start_date = "2024-01-10",
      end_date = "2024-01-01",
      source = "mock"
    ),
    "before"
  )
})

test_that("hc_read_flows accepts Date objects for date arguments", {
  local_register_adapter(mock_adapter)
  expect_no_error(
    hc_read_flows(
      "TOCHI001",
      start_date = as.Date("2024-01-01"),
      end_date = as.Date("2024-01-02"),
      source = "mock"
    )
  )
})

test_that("hc_read_flows records requested_stations on the returned object", {
  local_register_adapter(mock_adapter)
  result <- hc_read_flows(
    c("TOCHI001", "HOTH001"),
    start_date = "2024-01-01",
    end_date = "2024-01-01",
    source = "mock"
  )
  expect_equal(attr(result, "requested_stations"), c("TOCHI001", "HOTH001"))
})

test_that("hc_read_daily_flows records requested_stations on the returned object", {
  local_register_adapter(mock_adapter)
  result <- hc_read_daily_flows(
    "TOCHI001",
    start_date = "2024-01-01",
    end_date = "2024-01-01",
    source = "mock"
  )
  expect_equal(attr(result, "requested_stations"), "TOCHI001")
})

test_that("print.hydrocan_flows snapshot", {
  local_register_adapter(mock_adapter)
  result <- hc_read_flows(
    "TOCHI001",
    start_date = "2024-01-01",
    end_date = "2024-01-01",
    source = "mock"
  )
  expect_snapshot(print(result))
})

test_that("print.hydrocan_daily_flows snapshot", {
  local_register_adapter(mock_adapter)
  result <- hc_read_daily_flows(
    "TOCHI001",
    start_date = "2024-01-01",
    end_date = "2024-01-01",
    source = "mock"
  )
  expect_snapshot(print(result))
})

test_that("print.hydrocan_flows reports stations that were requested but not returned", {
  local_register_adapter(mock_adapter)
  # ALDERAAN001 is not in the mock registry so the router drops it with a
  # warning; the print method should then flag it as missing.
  expect_warning(
    result <- hc_read_flows(
      c("TOCHI001", "ALDERAAN001"),
      start_date = "2024-01-01",
      end_date = "2024-01-01",
      source = "mock"
    ),
    "ALDERAAN001"
  )
  expect_equal(
    attr(result, "requested_stations"),
    c("TOCHI001", "ALDERAAN001")
  )
  expect_snapshot(print(result))
})

test_that("hc_read_flows auto-routes when source is not specified", {
  local_clear_registry()
  local_register_adapter(mock_adapter)
  result <- hc_read_flows(
    "TOCHI001",
    start_date = "2024-01-01",
    end_date = "2024-01-01"
  )
  expect_s3_class(result, "hydrocan_flows")
  expect_equal(unique(result$station_number), "TOCHI001")
})

test_that("hc_read_daily_flows auto-routes when source is not specified", {
  local_clear_registry()
  local_register_adapter(mock_adapter)
  result <- hc_read_daily_flows(
    "TOCHI001",
    start_date = "2024-01-01",
    end_date = "2024-01-03"
  )
  expect_s3_class(result, "hydrocan_daily_flows")
  expect_equal(nrow(result), 3L)
})

test_that("hc_read_stations without source queries all registered adapters", {
  local_clear_registry()
  local_register_adapter(mock_adapter)
  result <- hc_read_stations()
  expect_equal(nrow(result), length(.mock_stations))
  expect_equal(unique(result$source), "mock")
})
