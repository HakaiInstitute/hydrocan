test_that("hc_citation returns a bibentry for a named source", {
  local_register_adapter(mock_adapter)
  result <- hc_citation("mock")
  expect_s3_class(result, "bibentry")
})

test_that("hc_citation errors on unknown source", {
  expect_error(hc_citation("nonexistent"), "nonexistent")
})

test_that("hc_citation uses title when available", {
  a <- new_hydrocan_adapter(
    "titled",
    "desc",
    identity,
    fetch_flows_fn = identity,
    title = "My Dataset Title",
    publisher = "My Organization"
  )
  local_register_adapter(a)
  result <- hc_citation("titled")
  expect_match(paste(format(result), collapse = "\n"), "My Dataset Title")
})

test_that("hc_citation falls back to name when title is NULL", {
  local_register_adapter(mock_adapter)
  result <- hc_citation("mock")
  expect_match(paste(format(result), collapse = "\n"), "mock")
})

test_that("hc_citation includes license in note when available", {
  a <- new_hydrocan_adapter(
    "licensed",
    "desc",
    identity,
    fetch_flows_fn = identity,
    title = "A Dataset",
    publisher = "Some Org",
    license = "CC BY 4.0"
  )
  local_register_adapter(a)
  result <- hc_citation("licensed")
  expect_match(paste(format(result), collapse = "\n"), "CC BY 4.0")
})
