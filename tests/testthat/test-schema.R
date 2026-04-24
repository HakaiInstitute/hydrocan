good_stations <- tibble::tibble(
  station_number = "X",
  station_name = "Station X",
  source = "mock",
  longitude = -114.0,
  latitude = 51.0,
  elevation_m = 1000.0,
  period_start = as.Date("2020-01-01"),
  period_end = as.Date(NA),
  notes = list(NULL)
)

good_rt <- tibble::tibble(
  station_number = "X",
  datetime = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
  value = 1.0,
  parameter = "flow",
  units = "m3/s",
  source = "mock",
  approval = "provisional",
  quality_flag = NA_character_
)

good_daily <- tibble::tibble(
  station_number = "X",
  date = as.Date("2024-01-01"),
  value = 1.0,
  parameter = "flow",
  units = "m3/s",
  source = "mock",
  approval = "provisional",
  quality_flag = NA_character_
)

test_that("validate_hydrocan_schema passes a well-formed flows tibble", {
  result <- hydrocan:::validate_hydrocan_schema(good_rt, "flows")
  expect_s3_class(result, "data.frame")
})

test_that("validate_hydrocan_schema passes a well-formed daily tibble", {
  result <- hydrocan:::validate_hydrocan_schema(good_daily, "daily")
  expect_s3_class(result, "data.frame")
})

test_that("validate_hydrocan_schema errors on missing column", {
  expect_error(
    hydrocan:::validate_hydrocan_schema(good_rt[, -1], "flows"),
    "station_number"
  )
  expect_error(
    hydrocan:::validate_hydrocan_schema(good_daily[, -2], "daily"),
    "date"
  )
})

test_that("validate_hydrocan_schema errors on invalid approval value", {
  bad <- good_rt
  bad$approval <- "unknown"
  expect_error(
    hydrocan:::validate_hydrocan_schema(bad, "flows"),
    "approval"
  )
})

test_that("validate_hydrocan_schema accepts NA in approval", {
  na_approval <- good_rt
  na_approval$approval <- NA_character_
  expect_s3_class(
    hydrocan:::validate_hydrocan_schema(na_approval, "flows"),
    "data.frame"
  )
})

test_that("validate_hydrocan_schema accepts all valid approval values", {
  for (val in c("provisional", "approved", "estimated")) {
    df <- good_rt
    df$approval <- val
    expect_s3_class(
      hydrocan:::validate_hydrocan_schema(df, "flows"),
      "data.frame"
    )
  }
})

test_that(".normalize_units maps known variants to canonical strings", {
  expect_equal(hydrocan:::.normalize_units("m\u00b3/s"), "m3/s")
  expect_equal(hydrocan:::.normalize_units("cms"), "m3/s")
  expect_equal(hydrocan:::.normalize_units("m^3/s"), "m3/s")
  expect_equal(hydrocan:::.normalize_units("cfs"), "ft3/s")
  expect_equal(hydrocan:::.normalize_units("feet"), "ft")
  expect_equal(hydrocan:::.normalize_units("metres"), "m")
})

test_that(".normalize_units passes unknown units through with a warning", {
  expect_warning(
    result <- hydrocan:::.normalize_units("furlongs/fortnight"),
    "furlongs/fortnight"
  )
  expect_equal(result, "furlongs/fortnight")
})

test_that("validate_hydrocan_schema normalizes units in the returned tibble", {
  raw <- good_rt
  raw$units <- "m\u00b3/s"
  result <- hydrocan:::validate_hydrocan_schema(raw, "flows")
  expect_equal(result$units, "m3/s")
})

test_that("validate_hydrocan_schema passes a well-formed stations tibble", {
  result <- hydrocan:::validate_hydrocan_schema(good_stations, "stations")
  expect_s3_class(result, "data.frame")
})

test_that("validate_hydrocan_schema errors on missing column for stations type", {
  expect_error(
    hydrocan:::validate_hydrocan_schema(good_stations[, -4], "stations"),
    "longitude"
  )
})


test_that(".normalize_units handles a vector of mixed units", {
  result <- suppressWarnings(
    hydrocan:::.normalize_units(c("cms", "cfs", "furlongs/fortnight"))
  )
  expect_equal(result[1L], "m3/s")
  expect_equal(result[2L], "ft3/s")
  expect_equal(result[3L], "furlongs/fortnight")
})
