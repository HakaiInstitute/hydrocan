#' Create a hydrocan adapter
#'
#' Constructs a validated adapter object that binds a data source name to its
#' station-listing and data-fetching functions. At least one of
#' `fetch_flows_fn` or `fetch_daily_flows_fn` must be provided; adapters that
#' only publish one temporal resolution should leave the other as `NULL`.
#'
#' @param name A single non-empty character string identifying this adapter.
#'   Used as the key in the registry and as the `source` field in output.
#' @param description A single character string describing the data source,
#'   including any known limitations (e.g. rolling data window, parameter
#'   availability). Shown by [hc_list_sources()].
#' @param list_stations_fn A function taking no arguments that returns a
#'   character vector of station numbers this adapter can serve.
#' @param fetch_flows_fn Optional function with signature
#'   `function(station_number, start_date, end_date)` returning a tibble with
#'   the hydrocan flows schema (contains `datetime`). Supply `NULL` if the
#'   source does not publish sub-daily observations.
#' @param fetch_daily_flows_fn Optional function with signature
#'   `function(station_number, start_date, end_date)` returning a tibble with
#'   the hydrocan daily schema (contains `date`). Supply `NULL` if the source
#'   does not publish daily summaries.
#' @param list_stations_meta_fn Optional function taking no arguments that
#'   returns a tibble with the hydrocan stations schema. Supply `NULL` if the
#'   source does not publish a station metadata endpoint.
#'
#' @return A list with class `"hydrocan_adapter"`.
#' @export
new_hydrocan_adapter <- function(
  name,
  description,
  list_stations_fn,
  fetch_flows_fn = NULL,
  fetch_daily_flows_fn = NULL,
  list_stations_meta_fn = NULL
) {
  if (!is.character(name) || length(name) != 1L || nchar(name) == 0L) {
    stop("'name' must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.character(description) || length(description) != 1L) {
    stop("'description' must be a single character string.", call. = FALSE)
  }
  if (!is.function(list_stations_fn)) {
    stop("'list_stations_fn' must be a function.", call. = FALSE)
  }
  if (!is.null(fetch_flows_fn) && !is.function(fetch_flows_fn)) {
    stop("'fetch_flows_fn' must be a function or NULL.", call. = FALSE)
  }
  if (!is.null(fetch_daily_flows_fn) && !is.function(fetch_daily_flows_fn)) {
    stop("'fetch_daily_flows_fn' must be a function or NULL.", call. = FALSE)
  }
  if (!is.null(list_stations_meta_fn) && !is.function(list_stations_meta_fn)) {
    stop("'list_stations_meta_fn' must be a function or NULL.", call. = FALSE)
  }
  if (is.null(fetch_flows_fn) && is.null(fetch_daily_flows_fn)) {
    stop(
      "At least one of 'fetch_flows_fn' or 'fetch_daily_flows_fn' must be provided.",
      call. = FALSE
    )
  }

  structure(
    list(
      name = name,
      description = description,
      list_stations_fn = list_stations_fn,
      fetch_flows_fn = fetch_flows_fn,
      fetch_daily_flows_fn = fetch_daily_flows_fn,
      list_stations_meta_fn = list_stations_meta_fn
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
