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
  expect_true(!any(is.na(result$quality_code)))
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
