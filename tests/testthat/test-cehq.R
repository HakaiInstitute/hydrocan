test_that("hydrocan_adapter_cehq creates a valid adapter", {
  a <- hydrocan_adapter_cehq()
  expect_s3_class(a, "hydrocan_adapter")
  expect_equal(a$name, "cehq")
  expect_true(is.function(a$list_stations_fn))
  expect_null(a$fetch_flows_fn)
  expect_null(a$fetch_levels_fn)
  expect_true(is.function(a$fetch_daily_flows_fn))
  expect_true(is.function(a$fetch_daily_levels_fn))
  expect_true(is.function(a$list_stations_meta_fn))
})

test_that("CEHQ station list returns only open flow stations", {
  httptest2::with_mock_api({
    stations <- hydrocan_adapter_cehq()$list_stations_fn()
    expect_type(stations, "character")
    expect_equal(stations, "030101")
  })
})

test_that("CEHQ station metadata returns correct schema", {
  httptest2::with_mock_api({
    meta <- hydrocan_adapter_cehq()$list_stations_meta_fn()
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
    expect_equal(nrow(meta), 1L)
    expect_equal(meta$station_id, "030101")
    expect_equal(meta$provider_name, "cehq")
    expect_s3_class(meta$period_start, "Date")
    expect_equal(as.integer(format(meta$period_start, "%Y")), 1960L)
    expect_true(is.na(meta$elevation_m))
    expect_type(meta$notes, "list")
  })
})

test_that("CEHQ fetch_daily_flows returns valid schema for a known station", {
  httptest2::with_mock_api({
    result <- hc_read_daily_flows(
      station_id = "030101",
      start_date = "2022-01-01",
      end_date = "2022-01-05",
      source = "cehq"
    )
    expect_s3_class(result, "hydrocan_daily")
    expect_named(
      result,
      c(
        "station_id",
        "date",
        "value",
        "parameter",
        "unit",
        "provider_name",
        "quality_code",
        "qf_desc"
      )
    )
    expect_equal(nrow(result), 5L)
    expect_s3_class(result$date, "Date")
    expect_equal(unique(result$parameter), "water_discharge")
    expect_equal(unique(result$unit), "m3/s")
    expect_equal(unique(result$provider_name), "cehq")
  })
})

test_that("CEHQ fetch_daily_flows passes through quality_code correctly", {
  httptest2::with_mock_api({
    result <- hc_read_daily_flows(
      station_id = "030101",
      start_date = "2022-01-01",
      end_date = "2022-01-05",
      source = "cehq"
    )
    # 2022-01-01: no remark -> quality_code NA
    expect_true(is.na(result$quality_code[[1L]]))
    # 2022-01-02: P remark (value present)
    expect_equal(result$value[[2L]], 6.14)
    expect_equal(result$quality_code[[2L]], "P")
    # 2022-01-03: E remark (value present)
    expect_equal(result$value[[3L]], 4.56)
    expect_equal(result$quality_code[[3L]], "E")
    # 2022-01-04: E in value column (no measurement) -> NA value
    expect_true(is.na(result$value[[4L]]))
    expect_equal(result$quality_code[[4L]], "E")
    # 2022-01-05: no remark -> quality_code NA
    expect_true(is.na(result$quality_code[[5L]]))
    # qf_desc is populated from the internal remark code lookup
    expect_true(is.na(result$qf_desc[[1L]])) # no remark -> NA
    expect_equal(result$qf_desc[[2L]], "La donn\u00e9e est provisoire")
    expect_equal(result$qf_desc[[3L]], "La donn\u00e9e est estim\u00e9e.")
    expect_equal(result$qf_desc[[4L]], "La donn\u00e9e est estim\u00e9e.")
    expect_true(is.na(result$qf_desc[[5L]]))
  })
})

test_that("CEHQ fetch_daily_levels returns valid schema for a known station", {
  httptest2::with_mock_api({
    result <- hc_read_daily_levels(
      station_id = "030101",
      start_date = "2022-01-01",
      end_date = "2022-01-05",
      source = "cehq"
    )
    expect_s3_class(result, "hydrocan_daily")
    expect_named(
      result,
      c(
        "station_id",
        "date",
        "value",
        "parameter",
        "unit",
        "provider_name",
        "quality_code",
        "qf_desc"
      )
    )
    expect_equal(nrow(result), 5L)
    expect_equal(unique(result$parameter), "water_level")
    expect_equal(unique(result$unit), "m")
    expect_equal(unique(result$provider_name), "cehq")
  })
})

test_that("CEHQ fetch_daily_levels passes through quality_code correctly", {
  httptest2::with_mock_api({
    result <- hc_read_daily_levels(
      station_id = "030101",
      start_date = "2022-01-01",
      end_date = "2022-01-05",
      source = "cehq"
    )
    expect_true(is.na(result$quality_code[[1L]]))
    expect_equal(result$quality_code[[2L]], "P")
    expect_equal(result$quality_code[[3L]], "E")
    expect_equal(result$value[[4L]], NA_real_)
    expect_equal(result$quality_code[[4L]], "E")
    expect_true(is.na(result$qf_desc[[1L]]))
    expect_equal(result$qf_desc[[2L]], "La donn\u00e9e est provisoire")
    expect_equal(result$qf_desc[[3L]], "La donn\u00e9e est estim\u00e9e.")
  })
})

test_that("CEHQ fetch_daily_flows returns empty tibble for unknown station", {
  httptest2::with_mock_api({
    # 999999_Q.R fixture returns a 404; the adapter catches httr2_http_404
    # and returns an empty tibble. Network errors and 5xx propagate.
    result <- hydrocan_adapter_cehq()$fetch_daily_flows_fn(
      "999999",
      as.Date("2022-01-01"),
      as.Date("2022-01-05")
    )
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 0L)
    expect_named(
      result,
      c(
        "station_id",
        "date",
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
