#' List registered data sources
#'
#' Returns a summary of all data sources currently available via hydrocan,
#' including their description and which data types they support.
#' No network calls are made.
#'
#' @return A tibble with columns `name` (chr), `description` (chr),
#'   `has_flows` (lgl), `has_daily_flows` (lgl), and `has_stations` (lgl).
#' @export
hc_list_sources <- function() {
  adapters <- as.list(.hydrocan_registry)
  if (length(adapters) == 0L) {
    return(tibble::tibble(
      name = character(),
      description = character(),
      has_flows = logical(),
      has_daily_flows = logical(),
      has_stations = logical()
    ))
  }
  tibble::tibble(
    name = vapply(adapters, `[[`, character(1L), "name"),
    description = vapply(adapters, `[[`, character(1L), "description"),
    has_flows = vapply(adapters, \(a) !is.null(a$fetch_flows_fn), logical(1L)),
    has_daily_flows = vapply(
      adapters,
      \(a) !is.null(a$fetch_daily_flows_fn),
      logical(1L)
    ),
    has_stations = vapply(
      adapters,
      \(a) !is.null(a$list_stations_meta_fn),
      logical(1L)
    )
  )
}

#' Retrieve station metadata
#'
#' Returns location and period-of-record information for all stations available
#' across registered data sources. Data sources that do not publish station
#' metadata are skipped with a warning.
#'
#' @param source Optional single character string naming the data source to
#'   query directly. When `NULL` (default) all registered data sources are
#'   queried. See [hc_list_sources()] for available names.
#'
#' @return A tibble with columns `station_number` (chr), `station_name` (chr),
#'   `source` (chr), `longitude` (dbl), `latitude` (dbl), `elevation_m` (dbl),
#'   `period_start` (Date), `period_end` (Date), and `notes` (list).
#' @export
hc_read_stations <- function(source = NULL) {
  if (!is.null(source)) {
    adapter <- get0(source, envir = .hydrocan_registry)
    if (is.null(adapter)) {
      stop("No data source registered with name '", source, "'.", call. = FALSE)
    }
    adapters <- stats::setNames(list(adapter), source)
  } else {
    adapters <- as.list(.hydrocan_registry)
  }

  if (length(adapters) == 0L) {
    stop(
      "No data sources are registered. Has the package loaded correctly?",
      call. = FALSE
    )
  }

  results <- lapply(adapters, function(a) {
    if (is.null(a$list_stations_meta_fn)) {
      warning(
        "Data source '",
        a$name,
        "' does not support station metadata. Skipping.",
        call. = FALSE
      )
      return(NULL)
    }
    tryCatch(
      a$list_stations_meta_fn(),
      error = function(e) {
        warning(
          "Failed to fetch stations from '",
          a$name,
          "': ",
          conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )
  })

  results <- Filter(Negate(is.null), results)
  if (length(results) == 0L) {
    return(.empty_stations_tibble())
  }
  validate_hydrocan_schema(dplyr::bind_rows(results), type = "stations")
}

#' Retrieve sub-daily flow observations
#'
#' Fetches sub-daily observations for one or more stations across the requested
#' date range. The data source is determined automatically from the station
#' number, or fixed explicitly via `source`.
#'
#' @param station_number Character vector of station identifiers.
#' @param start_date Start of the requested period (Date, or character
#'   coercible to Date).
#' @param end_date End of the requested period (Date, or character coercible
#'   to Date). Defaults to today.
#' @param source Optional single character string naming the data source to use
#'   directly. When `NULL` (default) the source is detected automatically from
#'   the station number. See [hc_list_sources()] for available names.
#'
#' @return A tibble with columns `station_number` (chr), `datetime` (POSIXct
#'   UTC), `value` (dbl), `parameter` (chr), `units` (chr), `source` (chr),
#'   `approval` (chr), and `quality_flag` (chr).
#' @export
hc_read_flows <- function(
  station_number,
  start_date,
  end_date = Sys.Date(),
  source = NULL
) {
  dates      <- .validate_date_range(start_date, end_date)
  start_date <- dates$start_date
  end_date   <- dates$end_date

  result <- .route_and_fetch(
    station_number,
    start_date,
    end_date,
    source,
    type = "flows"
  )
  validate_hydrocan_schema(result, type = "flows")
}

#' Retrieve daily flow summaries
#'
#' Returns one value per station per calendar day as published by the data
#' source. Not all data sources publish daily data; those that do not will
#' produce a warning and return no rows for the affected stations.
#'
#' @inheritParams hc_read_flows
#'
#' @return A tibble with columns `station_number` (chr), `date` (Date),
#'   `value` (dbl), `parameter` (chr), `units` (chr), `source` (chr),
#'   `approval` (chr), and `quality_flag` (chr).
#' @export
hc_read_daily_flows <- function(
  station_number,
  start_date,
  end_date = Sys.Date(),
  source = NULL
) {
  dates      <- .validate_date_range(start_date, end_date)
  start_date <- dates$start_date
  end_date   <- dates$end_date

  result <- .route_and_fetch(
    station_number,
    start_date,
    end_date,
    source,
    type = "daily"
  )
  validate_hydrocan_schema(result, type = "daily")
}
