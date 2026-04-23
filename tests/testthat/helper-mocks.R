# Mock adapter used across multiple test files. Returns deterministic data
# for two hardcoded stations without making any HTTP requests.

.mock_stations <- c("MOCK001", "MOCK002")

.mock_list_stations <- function() .mock_stations

.mock_fetch_flows <- function(station_number, start_date, end_date) {
  dates         <- seq(start_date, end_date, by = "day")
  n             <- length(dates)
  morning_times <- as.POSIXct(paste0(dates, " 06:00:00"), tz = "UTC")
  evening_times <- as.POSIXct(paste0(dates, " 18:00:00"), tz = "UTC")

  tibble::tibble(
    station_number = rep(station_number, n * 2L),
    datetime       = c(morning_times, evening_times),
    value          = seq(1.0, by = 1.0, length.out = n * 2L),
    parameter      = "flow",
    units          = "m3/s",
    source         = "mock",
    approval       = "provisional",
    quality_flag   = NA_character_
  )
}

.mock_fetch_daily_flows <- function(station_number, start_date, end_date) {
  dates <- seq(start_date, end_date, by = "day")

  tibble::tibble(
    station_number = rep(station_number, length(dates)),
    date           = dates,
    value          = seq(10.0, by = 1.0, length.out = length(dates)),
    parameter      = "flow",
    units          = "m3/s",
    source         = "mock",
    approval       = "provisional",
    quality_flag   = NA_character_
  )
}

.mock_list_stations_meta <- function() {
  tibble::tibble(
    station_number = .mock_stations,
    station_name   = c("Mock Station One", "Mock Station Two"),
    source         = "mock",
    longitude      = c(-114.0, -113.5),
    latitude       = c(51.0, 51.5),
    elevation_m    = c(1000.0, 1100.0),
    period_start   = as.Date(c("2020-01-01", "2021-01-01")),
    period_end     = as.Date(c(NA, NA)),
    notes          = list(NULL, NULL)
  )
}

mock_adapter <- new_hydrocan_adapter(
  "mock",
  "Mock adapter for offline testing. Returns deterministic data for two hardcoded stations.",
  .mock_list_stations,
  fetch_flows_fn        = .mock_fetch_flows,
  fetch_daily_flows_fn  = .mock_fetch_daily_flows,
  list_stations_meta_fn = .mock_list_stations_meta
)

# Adapter with only flows support, for testing the unsupported-type warning.
mock_adapter_flows_only <- new_hydrocan_adapter(
  "mock_rt_only",
  "Mock adapter with flows support only.",
  .mock_list_stations,
  fetch_flows_fn = .mock_fetch_flows
)
