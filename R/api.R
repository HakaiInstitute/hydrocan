#' List registered data sources
#'
#' Returns a summary of all data sources currently available via hydrocan,
#' including their description and which data types they support.
#' No network calls are made.
#'
#' @return A tibble with columns `name` (chr), `description` (chr),
#'   `has_flows` (lgl), `has_daily_flows` (lgl), `has_levels` (lgl),
#'   `has_daily_levels` (lgl), `has_stations` (lgl), `license` (chr),
#'   `license_url` (chr), `terms_url` (chr), and `docs_url` (chr).
#' @export
hc_list_sources <- function() {
  adapters <- as.list(.hydrocan_registry)
  if (length(adapters) == 0L) {
    return(tibble::tibble(
      name = character(),
      description = character(),
      has_flows = logical(),
      has_daily_flows = logical(),
      has_levels = logical(),
      has_daily_levels = logical(),
      has_stations = logical(),
      license = character(),
      license_url = character(),
      terms_url = character(),
      docs_url = character()
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
    has_levels = vapply(
      adapters,
      \(a) !is.null(a$fetch_levels_fn),
      logical(1L)
    ),
    has_daily_levels = vapply(
      adapters,
      \(a) !is.null(a$fetch_daily_levels_fn),
      logical(1L)
    ),
    has_stations = vapply(
      adapters,
      \(a) !is.null(a$list_stations_meta_fn),
      logical(1L)
    ),
    license = vapply(
      adapters,
      \(a) if (!is.null(a$license)) a$license else NA_character_,
      character(1L)
    ),
    license_url = vapply(
      adapters,
      \(a) if (!is.null(a$license_url)) a$license_url else NA_character_,
      character(1L)
    ),
    terms_url = vapply(
      adapters,
      \(a) if (!is.null(a$terms_url)) a$terms_url else NA_character_,
      character(1L)
    ),
    docs_url = vapply(
      adapters,
      \(a) if (!is.null(a$docs_url)) a$docs_url else NA_character_,
      character(1L)
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
#' @return A tibble with columns `station_id` (chr), `station_name` (chr),
#'   `provider_name` (chr), `longitude` (dbl), `latitude` (dbl),
#'   `elevation_m` (dbl), `period_start` (Date), `period_end` (Date), and
#'   `notes` (list).
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
#' ID, or fixed explicitly via `source`.
#'
#' @param station_id Character vector of station identifiers.
#' @param start_date Start of the requested period (Date, or character
#'   coercible to Date).
#' @param end_date End of the requested period (Date, or character coercible
#'   to Date). Defaults to today.
#' @param source Optional single character string naming the data source to use
#'   directly. When `NULL` (default) the source is detected automatically from
#'   the station ID. See [hc_list_sources()] for available names.
#'
#' @return A tibble with columns `station_id` (chr), `timestamp` (POSIXct
#'   UTC), `value` (dbl), `parameter` (chr), `unit` (chr), `provider_name`
#'   (chr), `quality_code` (chr), and `qf_desc` (chr).
#' @export
hc_read_flows <- function(
  station_id,
  start_date,
  end_date = Sys.Date(),
  source = NULL
) {
  dates <- .validate_date_range(start_date, end_date)
  result <- .route_and_fetch(
    station_id,
    dates$start_date,
    dates$end_date,
    source,
    type = "realtime"
  )
  new_hydrocan_realtime(
    validate_hydrocan_schema(result, "realtime"),
    station_id
  )
}

#' Retrieve sub-daily water level observations
#'
#' Fetches sub-daily water level observations for one or more stations across
#' the requested date range. The data source is determined automatically from
#' the station ID, or fixed explicitly via `source`.
#'
#' @inheritParams hc_read_flows
#'
#' @return A tibble with columns `station_id` (chr), `timestamp` (POSIXct
#'   UTC), `value` (dbl), `parameter` (chr: `"water_level"`), `unit` (chr),
#'   `provider_name` (chr), `quality_code` (chr), and `qf_desc` (chr).
#' @export
hc_read_levels <- function(
  station_id,
  start_date,
  end_date = Sys.Date(),
  source = NULL
) {
  dates <- .validate_date_range(start_date, end_date)
  result <- .route_and_fetch(
    station_id,
    dates$start_date,
    dates$end_date,
    source,
    type = "levels"
  )
  new_hydrocan_realtime(
    validate_hydrocan_schema(result, "realtime"),
    station_id
  )
}

#' Retrieve daily flow summaries
#'
#' Returns one value per station per calendar day as published by the data
#' source. Not all data sources publish daily data; those that do not will
#' produce a warning and return no rows for the affected stations.
#'
#' @inheritParams hc_read_flows
#'
#' @return A tibble with columns `station_id` (chr), `date` (Date),
#'   `value` (dbl), `parameter` (chr), `unit` (chr), `provider_name` (chr),
#'   `quality_code` (chr), and `qf_desc` (chr).
#' @export
hc_read_daily_flows <- function(
  station_id,
  start_date,
  end_date = Sys.Date(),
  source = NULL
) {
  dates <- .validate_date_range(start_date, end_date)
  result <- .route_and_fetch(
    station_id,
    dates$start_date,
    dates$end_date,
    source,
    type = "daily"
  )
  new_hydrocan_daily(validate_hydrocan_schema(result, "daily"), station_id)
}

#' Retrieve daily water level summaries
#'
#' Returns one water level value per station per calendar day as published by
#' the data source. Not all data sources publish daily level data; those that
#' do not will produce a warning and return no rows for the affected stations.
#'
#' @inheritParams hc_read_flows
#'
#' @return A tibble with columns `station_id` (chr), `date` (Date),
#'   `value` (dbl), `parameter` (chr: `"water_level"`), `unit` (chr),
#'   `provider_name` (chr), `quality_code` (chr), and `qf_desc` (chr).
#' @export
hc_read_daily_levels <- function(
  station_id,
  start_date,
  end_date = Sys.Date(),
  source = NULL
) {
  dates <- .validate_date_range(start_date, end_date)
  result <- .route_and_fetch(
    station_id,
    dates$start_date,
    dates$end_date,
    source,
    type = "daily_levels"
  )
  new_hydrocan_daily(validate_hydrocan_schema(result, "daily"), station_id)
}
