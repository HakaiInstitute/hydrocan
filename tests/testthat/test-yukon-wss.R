# Yukon WSS adapter tests --------------------------------------------------
#
# Some of these tests are live integration tests against the Government of Yukon API.
# They are skipped when internet access is unavailable or when run on CRAN.
#
# Other tests use mocking and can run on CRAN and CI.

test_that("Yukon adapter is constructed correctly", {
  adapter <- hydrocan_adapter_yukon_wss()

  expect_s3_class(adapter, "hydrocan_adapter")
  expect_identical(adapter$name, YUKON_PROVIDER_NAME)

  expect_true(is.function(adapter$list_stations_fn))
  expect_true(is.function(adapter$list_stations_meta_fn))
  expect_true(is.function(adapter$fetch_flows_fn))
  expect_true(is.function(adapter$fetch_levels_fn))
  expect_true(is.function(adapter$fetch_daily_flows_fn))
  expect_true(is.function(adapter$fetch_daily_levels_fn))
})


test_that("Yukon station list and metadata are valid", {
  skip_on_cran()
  skip_if_offline("service.yukon.ca")

  stations <- .yukon_list_stations()
  metadata <- .yukon_list_stations_meta()

  expect_type(stations, "character")
  expect_gte(length(stations), 20L) # as of 2026-09-02, 20 stations exist. Adapter should never report *fewer* than 20 even if we cease operation of stations as we intend to serve historic data
  expect_false(anyNA(stations))
  expect_length(unique(stations), length(stations))

  expect_s3_class(metadata, "tbl_df")
  expect_identical(
    names(metadata),
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

  expect_setequal(stations, metadata$station_id)
  expect_true(all(metadata$provider_name == YUKON_PROVIDER_NAME))
  expect_true(all(metadata$latitude >= -90))
  expect_true(all(metadata$latitude <= 90))
  expect_true(all(metadata$longitude >= -180))
  expect_true(all(metadata$longitude <= 180))
  expect_true(is.list(metadata$notes))

  expect_no_error(
    validate_hydrocan_schema(metadata, type = "stations")
  )
})

test_that("Yukon timeseries catalogue provides unique flow and level series", {
  skip_on_cran()
  skip_if_offline("service.yukon.ca")

  catalogue <- .yukon_get_timeseries_catalogue()

  expect_s3_class(catalogue, "data.frame")
  expect_gt(nrow(catalogue), 0L)
  expect_true(all(
    c(
      "location_code",
      "timeseries_id",
      "parameter_name",
      "units",
      "aggregation_type"
    ) %in%
      names(catalogue)
  ))

  expect_true(all(catalogue$publicly_visible))
  expect_true(all(catalogue$active))
  expect_false(anyNA(catalogue$location_code))

  flow_series <- catalogue |>
    dplyr::filter(
      parameter_name == YUKON_FLOW_PARAMETER,
      units == YUKON_FLOW_UNIT,
      aggregation_type == YUKON_SOURCE_AGGREGATION_TYPE
    )

  level_series <- catalogue |>
    dplyr::filter(
      parameter_name == YUKON_LEVEL_PARAMETER,
      units == YUKON_LEVEL_UNIT,
      aggregation_type == YUKON_SOURCE_AGGREGATION_TYPE
    )

  expect_false(anyDuplicated(flow_series$location_code) > 0L)
  expect_false(anyDuplicated(level_series$location_code) > 0L)
})


test_that("Yukon realtime flow and level outputs match HydroCan schema", {
  skip_on_cran()
  skip_if_offline("service.yukon.ca")

  flows <- .yukon_fetch_flows(
    station_id = "29AB005",
    start_date = as.Date("2026-07-12"),
    end_date = as.Date("2026-07-13")
  )

  levels <- .yukon_fetch_levels(
    station_id = "29AB005",
    start_date = as.Date("2026-07-12"),
    end_date = as.Date("2026-07-13")
  )

  expect_gt(nrow(flows), 0L)
  expect_gt(nrow(levels), 0L)

  expect_no_error(
    validate_hydrocan_schema(flows, type = "realtime")
  )
  expect_no_error(
    validate_hydrocan_schema(levels, type = "realtime")
  )

  expect_true(all(flows$station_id == "29AB005"))
  expect_true(all(flows$parameter == "water_discharge"))
  expect_true(all(flows$provider_name == YUKON_PROVIDER_NAME))
  expect_s3_class(flows$timestamp, "POSIXct")

  expect_true(all(levels$station_id == "29AB005"))
  expect_true(all(levels$parameter == "water_level"))
  expect_true(all(levels$unit == YUKON_LEVEL_UNIT))
  expect_true(all(levels$provider_name == YUKON_PROVIDER_NAME))
  expect_s3_class(levels$timestamp, "POSIXct")
})


test_that("Yukon daily flow and level outputs match HydroCan schema", {
  skip_on_cran()
  skip_if_offline("service.yukon.ca")

  flows <- .yukon_fetch_daily_flows(
    station_id = "29AB005",
    start_date = as.Date("2026-07-01"),
    end_date = as.Date("2026-07-07")
  )

  levels <- .yukon_fetch_daily_levels(
    station_id = "29AB005",
    start_date = as.Date("2026-07-01"),
    end_date = as.Date("2026-07-07")
  )

  expect_equal(nrow(flows), 7L)
  expect_equal(nrow(levels), 7L)

  expect_no_error(
    validate_hydrocan_schema(flows, type = "daily")
  )
  expect_no_error(
    validate_hydrocan_schema(levels, type = "daily")
  )

  expect_true(all(flows$parameter == "water_discharge"))
  expect_true(all(levels$parameter == "water_level"))
  expect_s3_class(flows$date, "Date")
  expect_s3_class(levels$date, "Date")
  expect_true(all(is.na(flows$quality_code)))
  expect_true(all(is.na(levels$quality_code)))
})


test_that("Yukon fetch functions return typed empty results", {
  skip_on_cran()
  skip_if_offline("service.yukon.ca")

  realtime_empty <- .yukon_fetch_flows(
    station_id = "29AB005",
    start_date = as.Date("1900-01-01"),
    end_date = as.Date("1900-01-07")
  )

  daily_empty <- .yukon_fetch_daily_flows(
    station_id = "29AB005",
    start_date = as.Date("1900-01-01"),
    end_date = as.Date("1900-01-07")
  )

  expect_equal(nrow(realtime_empty), 0L)
  expect_equal(nrow(daily_empty), 0L)
  expect_s3_class(realtime_empty$timestamp, "POSIXct")
  expect_s3_class(daily_empty$date, "Date")

  expect_no_error(
    validate_hydrocan_schema(realtime_empty, type = "realtime")
  )
  expect_no_error(
    validate_hydrocan_schema(daily_empty, type = "daily")
  )
})
