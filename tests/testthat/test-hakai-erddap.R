test_that("hydrocan_adapter_hakai_erddap creates a valid adapter", {
  a <- hydrocan_adapter_hakai_erddap()
  expect_s3_class(a, "hydrocan_adapter")
  expect_equal(a$name, "hakai_erddap")
  expect_true(is.function(a$list_stations_fn))
  expect_true(is.function(a$fetch_flows_fn))
  expect_true(is.function(a$fetch_levels_fn))
  expect_null(a$fetch_daily_flows_fn)
  expect_null(a$fetch_daily_levels_fn)
  expect_true(is.function(a$list_stations_meta_fn))
})

test_that("Hakai ERDDAP station list returns bundled station IDs", {
  stations <- hydrocan_adapter_hakai_erddap()$list_stations_fn()
  expect_type(stations, "character")
  expect_gt(length(stations), 0L)
  expect_true(all(grepl("^H08KC", stations)))
})

test_that("Hakai ERDDAP station metadata returns correct schema", {
  meta <- hydrocan_adapter_hakai_erddap()$list_stations_meta_fn()
  expect_s3_class(meta, "data.frame")
  expect_named(
    meta,
    c(
      "station_id",
      "station_name",
      "provider_name",
      "longitude",
      "latitude",
      "elevation_m",
      "period_start",
      "period_end",
      "notes"
    )
  )
  expect_gt(nrow(meta), 0L)
  expect_equal(unique(meta$provider_name), "hakai_erddap")
  expect_s3_class(meta$period_start, "Date")
  expect_s3_class(meta$period_end, "Date")
  expect_type(meta$notes, "list")
})

test_that("Hakai ERDDAP fetch_flows returns valid schema for a known station", {
  httptest2::with_mock_api({
    result <- hc_read_flows(
      station_id = "H08KC0703",
      start_date = "2023-08-01",
      end_date = "2023-08-03",
      source = "hakai_erddap"
    )
    expect_s3_class(result, "hydrocan_realtime")
    expect_named(
      result,
      c(
        "station_id",
        "timestamp",
        "value",
        "parameter",
        "unit",
        "provider_name",
        "quality_code",
        "qf_desc"
      )
    )
    expect_gt(nrow(result), 0L)
    expect_s3_class(result$timestamp, "POSIXct")
    expect_equal(attr(result$timestamp, "tzone"), "UTC")
    expect_equal(unique(result$station_id), "H08KC0703")
    expect_equal(unique(result$parameter), "water_discharge")
    expect_equal(unique(result$unit), "m3/s")
    expect_equal(unique(result$provider_name), "hakai_erddap")
    expect_true(all(is.na(result$qf_desc)))
  })
})

test_that("Hakai ERDDAP fetch_flows converts NaN to NA", {
  httptest2::with_mock_api({
    result <- hc_read_flows(
      station_id = "H08KC0703",
      start_date = "2023-08-01",
      end_date = "2023-08-03",
      source = "hakai_erddap"
    )
    # ERDDAP uses "NaN" for missing values; they must be parsed to NA_real_, not NaN.
    expect_false(any(is.nan(result$value)))
  })
})

test_that("Hakai ERDDAP fetch_levels returns valid schema for a known station", {
  httptest2::with_mock_api({
    result <- hc_read_levels(
      station_id = "H08KC0703",
      start_date = "2023-08-01",
      end_date = "2023-08-03",
      source = "hakai_erddap"
    )
    expect_s3_class(result, "hydrocan_realtime")
    expect_named(
      result,
      c(
        "station_id",
        "timestamp",
        "value",
        "parameter",
        "unit",
        "provider_name",
        "quality_code",
        "qf_desc"
      )
    )
    expect_gt(nrow(result), 0L)
    expect_s3_class(result$timestamp, "POSIXct")
    expect_equal(attr(result$timestamp, "tzone"), "UTC")
    expect_equal(unique(result$station_id), "H08KC0703")
    expect_equal(unique(result$parameter), "water_level")
    expect_equal(unique(result$unit), "m")
    expect_equal(unique(result$provider_name), "hakai_erddap")
    expect_true(any(!is.na(result$value)))
    expect_true(all(is.na(result$qf_desc)))
  })
})

test_that("Hakai ERDDAP fetch_flows returns empty tibble for unknown station", {
  httptest2::with_mock_api({
    result <- hydrocan_adapter_hakai_erddap()$fetch_flows_fn(
      "UNKNOWN999",
      as.Date("2023-08-01"),
      as.Date("2023-08-03")
    )
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 0L)
    expect_named(
      result,
      c(
        "station_id",
        "timestamp",
        "value",
        "parameter",
        "unit",
        "provider_name",
        "quality_code",
        "qf_desc"
      )
    )
  })
})
