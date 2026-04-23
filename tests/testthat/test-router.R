test_that(".route_and_fetch returns data for a known station", {
  register_hydrocan_adapter(mock_adapter)
  result <- hydrocan:::.route_and_fetch(
    "MOCK001",
    as.Date("2024-01-01"),
    as.Date("2024-01-03"),
    source = "mock"
  )
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0L)
  expect_equal(unique(result$station_number), "MOCK001")
})

test_that(".route_and_fetch warns and skips an unknown station", {
  register_hydrocan_adapter(mock_adapter)
  expect_warning(
    result <- hydrocan:::.route_and_fetch(
      "NOSUCHSTATION",
      as.Date("2024-01-01"),
      as.Date("2024-01-03"),
      source = "mock"
    ),
    "NOSUCHSTATION"
  )
  expect_equal(nrow(result), 0L)
})

test_that(".route_and_fetch respects the source argument", {
  register_hydrocan_adapter(mock_adapter)
  result <- hydrocan:::.route_and_fetch(
    "MOCK001",
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
  # Register two adapters that both claim the same station ID.
  collision_adapter <- new_hydrocan_adapter(
    "collision",
    "Adapter that claims MOCK001 to test collision handling.",
    function() c("MOCK001"),
    fetch_flows_fn = .mock_fetch_flows
  )
  register_hydrocan_adapter(mock_adapter)
  register_hydrocan_adapter(collision_adapter)
  expect_error(
    hydrocan:::.route_and_fetch(
      "MOCK001",
      as.Date("2024-01-01"),
      as.Date("2024-01-01")
    ),
    "multiple data sources"
  )
})

test_that(".route_and_fetch handles multiple stations", {
  register_hydrocan_adapter(mock_adapter)
  result <- hydrocan:::.route_and_fetch(
    c("MOCK001", "MOCK002"),
    as.Date("2024-01-01"),
    as.Date("2024-01-02"),
    source = "mock"
  )
  expect_setequal(unique(result$station_number), c("MOCK001", "MOCK002"))
})
