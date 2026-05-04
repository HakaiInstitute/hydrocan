# Mock adapter used across multiple test files. Returns deterministic data
# for two hardcoded stations without making any HTTP requests.

.mock_stations <- c("TOCHI001", "HOTH001")

.mock_list_stations <- function() .mock_stations

.mock_fetch_realtime <- function(
  station_id,
  start_date,
  end_date,
  parameter,
  unit,
  start_val,
  by_val
) {
  dates <- seq(start_date, end_date, by = "day")
  n <- length(dates)
  morning_times <- as.POSIXct(paste0(dates, " 06:00:00"), tz = "UTC")
  evening_times <- as.POSIXct(paste0(dates, " 18:00:00"), tz = "UTC")
  tibble::tibble(
    station_id = rep(station_id, n * 2L),
    timestamp = c(morning_times, evening_times),
    value = seq(start_val, by = by_val, length.out = n * 2L),
    parameter = parameter,
    unit = unit,
    provider_name = "mock",
    quality_code = NA_character_,
    qf_desc = NA_character_
  )
}

.mock_fetch_daily <- function(
  station_id,
  start_date,
  end_date,
  parameter,
  unit,
  start_val,
  by_val
) {
  dates <- seq(start_date, end_date, by = "day")
  tibble::tibble(
    station_id = rep(station_id, length(dates)),
    date = dates,
    value = seq(start_val, by = by_val, length.out = length(dates)),
    parameter = parameter,
    unit = unit,
    provider_name = "mock",
    quality_code = NA_character_,
    qf_desc = NA_character_
  )
}

.mock_fetch_flows <- function(stn, s, e) {
  .mock_fetch_realtime(stn, s, e, "water_discharge", "m3/s", 1.0, 1.0)
}
.mock_fetch_levels <- function(stn, s, e) {
  .mock_fetch_realtime(stn, s, e, "water_level", "m", 0.5, 0.01)
}
.mock_fetch_daily_flows <- function(stn, s, e) {
  .mock_fetch_daily(stn, s, e, "water_discharge", "m3/s", 10.0, 1.0)
}
.mock_fetch_daily_levels <- function(stn, s, e) {
  .mock_fetch_daily(stn, s, e, "water_level", "m", 0.5, 0.01)
}

.mock_list_stations_meta <- function() {
  tibble::tibble(
    station_id = .mock_stations,
    station_name = c("Tochi Station", "Hoth Station"),
    provider_name = "mock",
    longitude = c(-114.0, -113.5),
    latitude = c(51.0, 51.5),
    elevation_m = c(1000.0, 1100.0),
    period_start = as.Date(c("2020-01-01", "2021-01-01")),
    period_end = as.Date(c(NA, NA)),
    notes = list(NULL, NULL)
  )
}

mock_adapter <- new_hydrocan_adapter(
  "mock",
  "Mock adapter for offline testing. Returns deterministic data for two hardcoded stations.",
  .mock_list_stations,
  fetch_flows_fn = .mock_fetch_flows,
  fetch_daily_flows_fn = .mock_fetch_daily_flows,
  fetch_levels_fn = .mock_fetch_levels,
  fetch_daily_levels_fn = .mock_fetch_daily_levels,
  list_stations_meta_fn = .mock_list_stations_meta
)

# Adapter with only flows support, for testing the unsupported-type warning.
mock_adapter_flows_only <- new_hydrocan_adapter(
  "mock_rt_only",
  "Mock adapter with flows support only.",
  .mock_list_stations,
  fetch_flows_fn = .mock_fetch_flows
)

# Register an adapter for the duration of one test, restoring the registry
# to its prior state when the test exits. Handles both the case where the
# name was absent (removes it) and where it existed before (puts it back).
local_register_adapter <- function(adapter, env = parent.frame()) {
  name <- adapter$name
  registry <- .hydrocan_registry
  had_previous <- exists(name, envir = registry, inherits = FALSE)
  previous <- if (had_previous) get(name, envir = registry) else NULL

  register_hydrocan_adapter(adapter)

  withr::defer(
    if (had_previous) {
      assign(name, previous, envir = registry)
    } else {
      rm(list = name, envir = registry)
    },
    envir = env
  )
  invisible(adapter)
}

# Clear the entire registry for the duration of one test, restoring all prior
# entries on exit. Used for tests that need a known-empty or isolated registry.
local_clear_registry <- function(env = parent.frame()) {
  registry <- .hydrocan_registry
  saved_names <- ls(envir = registry)
  saved <- mget(saved_names, envir = registry)
  rm(list = saved_names, envir = registry)
  withr::defer(
    {
      rm(list = ls(envir = registry), envir = registry)
      if (length(saved) > 0L) list2env(saved, envir = registry)
    },
    envir = env
  )
  invisible(NULL)
}
