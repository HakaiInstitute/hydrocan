test_that("hydrocan_adapter_hydroquebec creates a valid adapter with all capabilities", {
  a <- hydrocan:::hydrocan_adapter_hydroquebec()
  expect_s3_class(a, "hydrocan_adapter")
  expect_equal(a$name, "hydroquebec")
  expect_true(is.function(a$list_stations_fn))
  expect_true(is.function(a$fetch_flows_fn))
  expect_true(is.function(a$fetch_daily_flows_fn))
  expect_true(is.function(a$list_stations_meta_fn))
})

test_that(".hq_parse_datetime parses the slash-date format to UTC POSIXct", {
  result <- hydrocan:::.hq_parse_datetime("2024/04/14T06:30:00Z")
  expect_s3_class(result, "POSIXct")
  expect_equal(
    format(result, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    "2024-04-14 06:30:00"
  )
  expect_equal(attr(result, "tzone"), "UTC")
})

test_that(".hq_parse_datetime returns NA for unparseable input", {
  result <- suppressWarnings(hydrocan:::.hq_parse_datetime("not-a-date"))
  expect_true(is.na(result))
})

test_that("HQ station list returns a non-empty character vector", {
  httptest2::with_mock_api({
    stations <- hydrocan:::hydrocan_adapter_hydroquebec()$list_stations_fn()
    expect_type(stations, "character")
    expect_gt(length(stations), 0L)
  })
})

test_that("HQ fetch returns a valid schema for a known station", {
  httptest2::with_mock_api({
    result <- hc_read_flows(
      station_number = "3-230",
      start_date = "2026-04-18",
      end_date = "2026-04-20",
      source = "hydroquebec"
    )
    expect_s3_class(result, "hydrocan_flows")
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
    expect_gt(nrow(result), 0L)
    expect_equal(unique(result$parameter), "flow")
    expect_equal(unique(result$source), "hydroquebec")
    expect_true(all(is.na(result$approval)))
  })
})

test_that("HQ daily aggregation works end-to-end", {
  httptest2::with_mock_api({
    result <- hc_read_daily_flows(
      station_number = "3-230",
      start_date = "2026-04-18",
      end_date = "2026-04-20",
      source = "hydroquebec"
    )
    expect_s3_class(result, "hydrocan_daily_flows")
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
    expect_equal(nrow(result), 3L)
    expect_s3_class(result$date, "Date")
  })
})
