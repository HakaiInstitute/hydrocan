test_that("new_hydrocan_adapter rejects non-string name", {
  expect_error(
    new_hydrocan_adapter(123L, "desc", identity, fetch_flows_fn = identity),
    "name"
  )
  expect_error(
    new_hydrocan_adapter(
      c("a", "b"),
      "desc",
      identity,
      fetch_flows_fn = identity
    ),
    "name"
  )
  expect_error(
    new_hydrocan_adapter("", "desc", identity, fetch_flows_fn = identity),
    "name"
  )
})

test_that("new_hydrocan_adapter rejects non-string description", {
  expect_error(
    new_hydrocan_adapter("ok", 123L, identity, fetch_flows_fn = identity),
    "description"
  )
})

test_that("new_hydrocan_adapter rejects non-function arguments", {
  expect_error(
    new_hydrocan_adapter("ok", "desc", "not_fn", fetch_flows_fn = identity),
    "list_stations_fn"
  )
  expect_error(
    new_hydrocan_adapter("ok", "desc", identity, fetch_flows_fn = "not_fn"),
    "fetch_flows_fn"
  )
  expect_error(
    new_hydrocan_adapter(
      "ok",
      "desc",
      identity,
      fetch_daily_flows_fn = "not_fn"
    ),
    "fetch_daily_flows_fn"
  )
})

test_that("new_hydrocan_adapter requires at least one fetch function", {
  expect_error(
    new_hydrocan_adapter("ok", "desc", identity),
    "At least one"
  )
})

test_that("new_hydrocan_adapter accepts only fetch_flows_fn", {
  a <- new_hydrocan_adapter(
    "flows_only",
    "desc",
    identity,
    fetch_flows_fn = identity
  )
  expect_s3_class(a, "hydrocan_adapter")
  expect_null(a$fetch_daily_flows_fn)
})

test_that("new_hydrocan_adapter accepts only fetch_daily_flows_fn", {
  a <- new_hydrocan_adapter(
    "daily_only",
    "desc",
    identity,
    fetch_daily_flows_fn = identity
  )
  expect_s3_class(a, "hydrocan_adapter")
  expect_null(a$fetch_flows_fn)
})

test_that("new_hydrocan_adapter returns a correctly structured object", {
  a <- new_hydrocan_adapter(
    "mytest",
    "A test adapter.",
    identity,
    fetch_flows_fn = identity
  )
  expect_s3_class(a, "hydrocan_adapter")
  expect_equal(a$name, "mytest")
  expect_equal(a$description, "A test adapter.")
  expect_identical(a$list_stations_fn, identity)
  expect_identical(a$fetch_flows_fn, identity)
})

test_that("register_hydrocan_adapter rejects non-adapter input", {
  expect_error(register_hydrocan_adapter(list(name = "x")), "hydrocan_adapter")
})

test_that("register_hydrocan_adapter stores adapter in registry", {
  a <- new_hydrocan_adapter(
    "reg_test",
    "desc",
    function() character(),
    fetch_flows_fn = function(...) NULL
  )
  local_register_adapter(a)
  expect_true(exists("reg_test", envir = hydrocan:::.hydrocan_registry))
})

test_that("hc_list_sources returns the correct schema", {
  local_register_adapter(mock_adapter)
  result <- hc_list_sources()
  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("name", "description", "has_flows", "has_daily_flows", "has_stations")
  )
  expect_type(result$has_flows, "logical")
})

test_that("hc_list_sources reflects adapter capabilities correctly", {
  local_register_adapter(mock_adapter)
  local_register_adapter(mock_adapter_flows_only)
  result <- hc_list_sources()
  mock_row <- result[result$name == "mock", ]
  expect_true(mock_row$has_flows)
  expect_true(mock_row$has_daily_flows)
  expect_true(mock_row$has_stations)
  rt_row <- result[result$name == "mock_rt_only", ]
  expect_true(rt_row$has_flows)
  expect_false(rt_row$has_daily_flows)
  expect_false(rt_row$has_stations)
})
