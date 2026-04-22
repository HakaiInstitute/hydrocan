#' Retrieve real-time flow and level observations
#'
#' Fetches sub-daily observations for one or more stations across the requested
#' date range. The data source is determined automatically by scanning all
#' registered adapters, or fixed explicitly via `source`.
#'
#' @param station_number Character vector of station identifiers.
#' @param start_date Start of the requested period (Date, or character
#'   coercible to Date).
#' @param end_date End of the requested period (Date, or character coercible
#'   to Date).
#' @param source Optional single character string naming the adapter to use
#'   directly. When `NULL` (default) the router scans all registered adapters.
#'
#' @return A tibble with columns `station_number` (chr), `datetime` (POSIXct
#'   UTC), `value` (dbl), `parameter` (chr), `units` (chr), `source` (chr),
#'   `approval` (chr), and `quality_flag` (chr).
#' @export
available_flows <- function(station_number, start_date, end_date, source = NULL) {
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)

  result <- .route_and_fetch(station_number, start_date, end_date, source)
  validate_hydrocan_schema(result, type = "realtime")
  result
}

#' Retrieve daily aggregated flow and level data
#'
#' Calls [available_flows()] then aggregates sub-daily observations into one
#' value per station per calendar day using `fun`.
#'
#' @inheritParams available_flows
#' @param fun Aggregation function applied as `fun(value, na.rm = TRUE)`.
#'   Defaults to [mean()].
#'
#' @return A tibble with columns `station_number` (chr), `date` (Date),
#'   `value` (dbl), `parameter` (chr), `units` (chr), `source` (chr),
#'   `approval` (chr), and `quality_flag` (chr).
#' @export
available_daily_flows <- function(
    station_number, start_date, end_date, source = NULL, fun = mean
) {
  rt     <- available_flows(station_number, start_date, end_date, source)
  result <- .aggregate_daily(rt, fun)
  validate_hydrocan_schema(result, type = "daily")
  result
}
