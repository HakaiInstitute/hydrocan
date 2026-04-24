test_that(".route_and_fetch returns data for a known station", {
  local_register_adapter(mock_adapter)
  result <- hydrocan:::.route_and_fetch(
    "TOCHI001",
    as.Date("2024-01-01"),
    as.Date("2024-01-03"),
    source = "mock"
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$station_number), "TOCHI001")
})

test_that(".route_and_fetch warns and skips an unknown station", {
  local_register_adapter(mock_adapter)
  expect_warning(
    result <- hydrocan:::.route_and_fetch(
      "ALDERAAN001",
      as.Date("2024-01-01"),
      as.Date("2024-01-03"),
      source = "mock"
    ),
    "ALDERAAN001"
  )
  expect_equal(nrow(result), 0L)
})

test_that(".route_and_fetch respects the source argument", {
  local_register_adapter(mock_adapter)
  result <- hydrocan:::.route_and_fetch(
    "TOCHI001",
    as.Date("2024-01-01"),
    as.Date("2024-01-01"),
    source = "mock"
  )
  expect_equal(unique(result$source), "mock")
})

test_that(".route_and_fetch errors on unknown source name", {
  expect_error(
    hydrocan:::.route_and_fetch(
      "X",
      as.Date("2024-01-01"),
      as.Date("2024-01-01"),
      source = "doesnotexist"
    ),
    "doesnotexist"
  )
})

test_that(".route_and_fetch errors on station ID collision across sources", {
  # Isolate the registry so the router only scans these two adapters and does
  # not call list_stations_fn() on any live (HTTP) adapters.
  local_clear_registry()
  collision_adapter <- new_hydrocan_adapter(
    "collision",
    "Adapter that claims TOCHI001 to test collision handling.",
    function() c("TOCHI001"),
    fetch_flows_fn = .mock_fetch_flows
  )
  local_register_adapter(mock_adapter)
  local_register_adapter(collision_adapter)
  expect_error(
    hydrocan:::.route_and_fetch(
      "TOCHI001",
      as.Date("2024-01-01"),
      as.Date("2024-01-01")
    ),
    "multiple data sources"
  )
})

test_that(".route_and_fetch handles multiple stations", {
  local_register_adapter(mock_adapter)
  result <- hydrocan:::.route_and_fetch(
    c("TOCHI001", "HOTH001"),
    as.Date("2024-01-01"),
    as.Date("2024-01-02"),
    source = "mock"
  )
  expect_setequal(unique(result$station_number), c("TOCHI001", "HOTH001"))
})

test_that(".route_and_fetch dispatches fetch_daily_flows_fn when type is 'daily'", {
  local_register_adapter(mock_adapter)
  result <- hydrocan:::.route_and_fetch(
    "TOCHI001",
    as.Date("2024-01-01"),
    as.Date("2024-01-03"),
    source = "mock",
    type = "daily"
  )
  expect_true("date" %in% names(result))
  expect_false("datetime" %in% names(result))
  expect_equal(nrow(result), 3L)
})

test_that(".route_and_fetch converts a per-station fetch error to a warning", {
  error_adapter <- new_hydrocan_adapter(
    "error_source",
    "Adapter whose fetch function always throws.",
    function() "ERR001",
    fetch_flows_fn = function(...) stop("simulated fetch failure")
  )
  local_register_adapter(error_adapter)
  expect_warning(
    result <- hydrocan:::.route_and_fetch(
      "ERR001",
      as.Date("2024-01-01"),
      as.Date("2024-01-01"),
      source = "error_source"
    ),
    "simulated fetch failure"
  )
  expect_equal(nrow(result), 0L)
})

test_that(".route_and_fetch errors when no adapters are registered", {
  local_clear_registry()
  expect_error(
    hydrocan:::.route_and_fetch(
      "X",
      as.Date("2024-01-01"),
      as.Date("2024-01-01")
    ),
    "No data sources are registered"
  )
})
