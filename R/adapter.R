#' Create a hydrocan adapter
#'
#' Constructs a validated adapter object that binds a data source name to its
#' station-listing and data-fetching functions.
#'
#' @param name A single non-empty character string identifying this adapter.
#'   Used as the key in the registry and as the `source` field in output.
#' @param list_stations_fn A function taking no arguments that returns a
#'   character vector of station numbers this adapter can serve.
#' @param fetch_flows_fn A function with signature
#'   `function(station_number, start_date, end_date)` that returns a tibble
#'   conforming to the hydrocan schema.
#'
#' @return A list with class `"hydrocan_adapter"`.
#' @export
new_hydrocan_adapter <- function(name, list_stations_fn, fetch_flows_fn) {
  if (!is.character(name) || length(name) != 1L || nchar(name) == 0L) {
    stop("'name' must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.function(list_stations_fn)) {
    stop("'list_stations_fn' must be a function.", call. = FALSE)
  }
  if (!is.function(fetch_flows_fn)) {
    stop("'fetch_flows_fn' must be a function.", call. = FALSE)
  }

  structure(
    list(
      name             = name,
      list_stations_fn = list_stations_fn,
      fetch_flows_fn   = fetch_flows_fn
    ),
    class = "hydrocan_adapter"
  )
}

#' Register a hydrocan adapter
#'
#' Stores an adapter in the package registry so it is visible to the router.
#' Registering an adapter with the same name as an existing one overwrites it.
#'
#' @param adapter A `"hydrocan_adapter"` object created by
#'   [new_hydrocan_adapter()].
#'
#' @return `adapter`, invisibly.
#' @export
register_hydrocan_adapter <- function(adapter) {
  if (!inherits(adapter, "hydrocan_adapter")) {
    stop("'adapter' must be a 'hydrocan_adapter' object.", call. = FALSE)
  }
  assign(adapter$name, adapter, envir = .hydrocan_registry)
  invisible(adapter)
}
