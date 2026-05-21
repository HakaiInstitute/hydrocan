#' Create a hydrocan adapter
#'
#' Constructs a validated adapter object for a data source. At least one fetch
#' function must be supplied.
#'
#' @param name Non-empty string identifying this source. Used as the registry
#'   key and as the `provider_name` column in output.
#' @param description String describing the source and any known limitations
#'   (e.g. rolling data window). Shown by [hc_list_sources()].
#' @param list_stations_fn Function with no arguments returning a character
#'   vector of station IDs this source can serve.
#' @param fetch_flows_fn Optional `function(station_id, start_date,
#'   end_date)` returning a tibble matching the flows schema (`timestamp`
#'   column). `NULL` if sub-daily flow data is not available.
#' @param fetch_daily_flows_fn Optional `function(station_id, start_date,
#'   end_date)` returning a tibble matching the daily flows schema (`date`
#'   column). `NULL` if daily flow data is not available.
#' @param fetch_levels_fn Optional `function(station_id, start_date,
#'   end_date)` returning a tibble matching the flows schema (`timestamp`
#'   column) with `parameter = "water_level"`. `NULL` if sub-daily level data
#'   is not available.
#' @param fetch_daily_levels_fn Optional `function(station_id, start_date,
#'   end_date)` returning a tibble matching the daily flows schema (`date`
#'   column) with `parameter = "water_level"`. `NULL` if daily level data is
#'   not available.
#' @param list_stations_meta_fn Optional function with no arguments returning
#'   a tibble matching the stations schema. `NULL` if station metadata is not
#'   available.
#' @param title Optional string with the formal name of the dataset as
#'   published by the provider (used in citations).
#' @param publisher Optional string naming the organization that publishes the
#'   data (used in citations).
#' @param license Optional string naming the data license (e.g. `"CC-BY 4.0"`).
#' @param license_url Optional string with a URL to the license text.
#' @param terms_url Optional string with a URL to the data provider's terms of
#'   use or data policy.
#' @param docs_url Optional string with a URL to human-readable documentation
#'   about the data (field definitions, codes, data structure). A machine-
#'   readable metadata endpoint is acceptable if no human-readable page exists.
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
  list_stations_meta_fn = NULL,
  title = NULL,
  publisher = NULL,
  license = NULL,
  license_url = NULL,
  terms_url = NULL,
  docs_url = NULL
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
  for (nm in c(
    "title",
    "publisher",
    "license",
    "license_url",
    "terms_url",
    "docs_url"
  )) {
    val <- get(nm)
    if (
      (!is.null(val)) && (!is.character(val) || length(val) != 1L || is.na(val))
    ) {
      stop(
        "'",
        nm,
        "' must be a single non-NA character string or NULL.",
        call. = FALSE
      )
    }
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
      list_stations_meta_fn = list_stations_meta_fn,
      title = title,
      publisher = publisher,
      license = license,
      license_url = license_url,
      terms_url = terms_url,
      docs_url = docs_url
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
  adapter$list_stations_fn <- memoise::memoise(adapter$list_stations_fn)
  if (!is.null(adapter$list_stations_meta_fn)) {
    adapter$list_stations_meta_fn <- memoise::memoise(
      adapter$list_stations_meta_fn
    )
  }
  assign(adapter$name, adapter, envir = .hydrocan_registry)
  invisible(adapter)
}
