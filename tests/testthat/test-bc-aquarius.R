test_that("hydrocan_adapter_bc_aquarius creates a valid adapter", {
  a <- hydrocan_adapter_bc_aquarius()
  expect_s3_class(a, "hydrocan_adapter")
  expect_equal(a$name, "bc_aquarius")
  expect_true(is.function(a$list_stations_fn))
  expect_true(is.function(a$fetch_flows_fn))
  expect_true(is.function(a$fetch_daily_flows_fn))
  expect_true(is.function(a$fetch_levels_fn))
  expect_true(is.function(a$fetch_daily_levels_fn))
})

test_that("bc_aquarius station list is a non-empty character vector", {
  stations <- hydrocan_adapter_bc_aquarius()$list_stations_fn()
  expect_type(stations, "character")
  expect_gt(length(stations), 0L)
  expect_true("08HB0021" %in% stations)
})

test_that("bc_aquarius fetch_flows returns valid schema for a known station", {
  skip_if_offline()
  local_register_adapter(hydrocan_adapter_bc_aquarius())
  result <- hc_read_flows(
    station_id = "08HB0021",
    start_date = "2026-05-01",
    end_date   = "2026-05-07",
    source     = "bc_aquarius"
  )
  expect_s3_class(result, "hydrocan_realtime")
  expect_named(result, c(
    "station_id", "timestamp", "value", "parameter",
    "unit", "provider_name", "quality_code", "qf_desc"
  ))
  expect_gt(nrow(result), 0L)
  expect_equal(unique(result$parameter), "water_discharge")
  expect_equal(unique(result$unit), "m3/s")
  expect_equal(unique(result$provider_name), "bc_aquarius")
  expect_false(anyNA(result$quality_code))
})

test_that("bc_aquarius fetch_daily_flows returns valid schema for a known station", {
  skip_if_offline()
  local_register_adapter(hydrocan_adapter_bc_aquarius())
  result <- hc_read_daily_flows(
    station_id = "08HB0021",
    start_date = "2026-05-01",
    end_date   = "2026-05-07",
    source     = "bc_aquarius"
  )
  expect_s3_class(result, "hydrocan_daily")
  expect_named(result, c(
    "station_id", "date", "value", "parameter",
    "unit", "provider_name", "quality_code", "qf_desc"
  ))
  expect_gt(nrow(result), 0L)
  expect_s3_class(result$date, "Date")
  expect_equal(unique(result$provider_name), "bc_aquarius")
})

test_that("bc_aquarius fetch_levels returns valid schema for a known station", {
  skip_if_offline()
  local_register_adapter(hydrocan_adapter_bc_aquarius())
  result <- hc_read_levels(
    station_id = "08HB0012",
    start_date = "2026-05-01",
    end_date   = "2026-05-07",
    source     = "bc_aquarius"
  )
  expect_s3_class(result, "hydrocan_realtime")
  expect_named(result, c(
    "station_id", "timestamp", "value", "parameter",
    "unit", "provider_name", "quality_code", "qf_desc"
  ))
  expect_gt(nrow(result), 0L)
  expect_equal(unique(result$parameter), "water_level")
  expect_equal(unique(result$unit), "m")
  expect_equal(unique(result$provider_name), "bc_aquarius")
})

test_that("bc_aquarius fetch_daily_levels returns valid schema for a known station", {
  skip_if_offline()
  local_register_adapter(hydrocan_adapter_bc_aquarius())
  result <- hc_read_daily_levels(
    station_id = "08HB0012",
    start_date = "2026-05-01",
    end_date   = "2026-05-07",
    source     = "bc_aquarius"
  )
  expect_s3_class(result, "hydrocan_daily")
  expect_named(result, c(
    "station_id", "date", "value", "parameter",
    "unit", "provider_name", "quality_code", "qf_desc"
  ))
  expect_gt(nrow(result), 0L)
  expect_s3_class(result$date, "Date")
  expect_equal(unique(result$parameter), "water_level")
  expect_equal(unique(result$provider_name), "bc_aquarius")
})

test_that("bc_aquarius request URL EndTime includes all of end_date", {
  # Aquarius treats EndTime as an exclusive instant, so the request must reach
  # past 00:00 on end_date: the next midnight for daily, 23:59:59 for sub-daily.
  subdaily_url <- .build_aquarius_url(
    stn_number = "08HB0021",
    parameter  = "discharge",
    start_time = as.Date("2026-05-01"),
    end_time   = as.Date("2026-05-07")
  )
  subdaily_query <- httr2::url_parse(subdaily_url)$query
  expect_equal(subdaily_query$StartTime, "2026-05-01")
  expect_equal(subdaily_query$EndTime, "2026-05-07 23:59:59")

  daily_url <- .build_aquarius_url(
    stn_number = "08HB0021",
    parameter  = "discharge",
    start_time = as.Date("2026-05-01"),
    end_time   = as.Date("2026-05-07"),
    interval   = "Daily"
  )
  daily_query <- httr2::url_parse(daily_url)$query
  expect_equal(daily_query$StartTime, "2026-05-01")
  expect_equal(daily_query$EndTime, "2026-05-08")
})

test_that("bc_aquarius fetch_daily_flows includes end_date", {
  skip_if_offline()
  local_register_adapter(hydrocan_adapter_bc_aquarius())
  result <- hc_read_daily_flows(
    station_id = "08HB0021",
    start_date = "2026-05-01",
    end_date   = "2026-05-07",
    source     = "bc_aquarius"
  )
  expect_equal(
    result$date,
    seq(as.Date("2026-05-01"), as.Date("2026-05-07"), by = "day")
  )
})

test_that("bc_aquarius fetch_flows covers all of end_date and nothing after", {
  skip_if_offline()
  local_register_adapter(hydrocan_adapter_bc_aquarius())
  result <- hc_read_flows(
    station_id = "08HB0021",
    start_date = "2026-05-01",
    end_date   = "2026-05-07",
    source     = "bc_aquarius"
  )
  # 08HB0021 logs every 5 minutes: 288 readings per day for 7 days
  expect_equal(nrow(result), 288L * 7L)
  expect_equal(
    max(result$timestamp),
    as.POSIXct("2026-05-07 23:55:00", tz = "UTC")
  )
  expect_true(all(result$timestamp < as.POSIXct("2026-05-08", tz = "UTC")))
})
