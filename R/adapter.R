#' Create a hydrocan adapter
#'
#' Constructs a validated adapter object for a data source. At least one fetch
#' function must be supplied.
#'
#' @param name Non-empty string identifying this source. Used as the registry
#'   key and as the `source` column in output.
#' @param description String describing the source and any known limitations
#'   (e.g. rolling data window). Shown by [hc_list_sources()].
#' @param list_stations_fn Function with no arguments returning a character
#'   vector of station IDs this source can serve.
#' @param fetch_flows_fn Optional `function(station_number, start_date,
#'   end_date)` returning a tibble matching the flows schema (`datetime`
#'   column). `NULL` if sub-daily flow data is not available.
#' @param fetch_daily_flows_fn Optional `function(station_number, start_date,
#'   end_date)` returning a tibble matching the daily flows schema (`date`
#'   column). `NULL` if daily flow data is not available.
#' @param fetch_levels_fn Optional `function(station_number, start_date,
#'   end_date)` returning a tibble matching the flows schema (`datetime`
#'   column) with `parameter = "level"`. `NULL` if sub-daily level data is not
#'   available.
#' @param fetch_daily_levels_fn Optional `function(station_number, start_date,
#'   end_date)` returning a tibble matching the daily flows schema (`date`
#'   column) with `parameter = "level"`. `NULL` if daily level data is not
#'   available.
#' @param list_stations_meta_fn Optional function with no arguments returning
#'   a tibble matching the stations schema. `NULL` if station metadata is not
#'   available.
#'
#' @return A list with class `"hydrocan_adapter"`.
#' @export
new_hydrocan_adapter <- function(
  name,
  description,
  list_stations_fn,
  fetch_flows_fn = NULL,
  fetch_daily_flows_fn = NULL,
  fetch_levels_fn = NULL,
  fetch_daily_levels_fn = NULL,
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
  fetch_fns <- list(
    fetch_flows_fn = fetch_flows_fn,
    fetch_daily_flows_fn = fetch_daily_flows_fn,
    fetch_levels_fn = fetch_levels_fn,
    fetch_daily_levels_fn = fetch_daily_levels_fn
  )
  for (nm in names(fetch_fns)) {
    if (!is.null(fetch_fns[[nm]]) && !is.function(fetch_fns[[nm]])) {
      stop("'", nm, "' must be a function or NULL.", call. = FALSE)
    }
  }
  if (all(vapply(fetch_fns, is.null, logical(1L)))) {
    stop(
      "At least one fetch function must be provided (",
      paste(names(fetch_fns), collapse = ", "),
      ").",
      call. = FALSE
    )
  }
  if (!is.null(list_stations_meta_fn) && !is.function(list_stations_meta_fn)) {
    stop("'list_stations_meta_fn' must be a function or NULL.", call. = FALSE)
  }

  structure(
    list(
      name = name,
      description = description,
      list_stations_fn = list_stations_fn,
      fetch_flows_fn = fetch_flows_fn,
      fetch_daily_flows_fn = fetch_daily_flows_fn,
      fetch_levels_fn = fetch_levels_fn,
      fetch_daily_levels_fn = fetch_daily_levels_fn,
      list_stations_meta_fn = list_stations_meta_fn
    ),
    class = "hydrocan_adapter"
  )
}

#' Register a hydrocan adapter
#'
#' Adds an adapter to the package registry. Registering under an existing name
#' overwrites it.
#'
#' @param adapter A `"hydrocan_adapter"` object from [new_hydrocan_adapter()].
#' @return `adapter`, invisibly.
#' @export
register_hydrocan_adapter <- function(adapter) {
  if (!inherits(adapter, "hydrocan_adapter")) {
    stop("'adapter' must be a 'hydrocan_adapter' object.", call. = FALSE)
  }
  assign(adapter$name, adapter, envir = .hydrocan_registry)
  invisible(adapter)
}
