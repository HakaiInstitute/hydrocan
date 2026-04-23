# Coerce start/end date arguments and validate the resulting range. Stops with
# a clear message if either value cannot be parsed or if the range is inverted.
.try_as_date <- function(x, arg) {
  d <- tryCatch(as.Date(x), error = \(e) NA_real_)
  if (is.na(d)) stop("'", arg, "' could not be parsed as a Date.", call. = FALSE)
  d
}

# Coerce start/end date arguments and validate the resulting range. Stops with
# a clear message if either value cannot be parsed or if the range is inverted.
.validate_date_range <- function(start_date, end_date) {
  start_date <- .try_as_date(start_date, "start_date")
  end_date   <- .try_as_date(end_date,   "end_date")
  if (end_date < start_date) {
    stop(
      "'end_date' (", end_date, ") is before 'start_date' (", start_date, ").",
      call. = FALSE
    )
  }
  list(start_date = start_date, end_date = end_date)
}

# Base request constructor used by all adapters. Sets a consistent User-Agent
# so that server logs can identify traffic from this package.
.hydrocan_request <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent(
      'https://github.com/HakaiInstitute/hydrocan'
    )
}

# Build a next_req callback for offset-based pagination. Suitable for any API
# that uses limit/offset query parameters and returns total_count in the
# response body. Each call returns a fresh closure with its own offset counter,
# so sequential or concurrent fetches do not share state.
.offset_next_req <- function(limit) {
  offset <- 0L
  function(resp, req) {
    body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
    offset <<- offset + limit
    if (is.null(body$total_count) || offset >= body$total_count) {
      return(NULL)
    }
    httr2::req_url_query(req, offset = offset)
  }
}

.empty_stations_tibble <- function() {
  tibble::tibble(
    station_number = character(),
    station_name = character(),
    source = character(),
    longitude = double(),
    latitude = double(),
    elevation_m = double(),
    period_start = as.Date(character()),
    period_end = as.Date(character()),
    notes = list()
  )
}

.empty_flows_tibble <- function() {
  tibble::tibble(
    station_number = character(),
    datetime = as.POSIXct(character(), tz = "UTC"),
    value = numeric(),
    parameter = character(),
    units = character(),
    source = character(),
    approval = character(),
    quality_flag = character()
  )
}

.empty_daily_flows_tibble <- function() {
  tibble::tibble(
    station_number = character(),
    date = as.Date(character()),
    value = numeric(),
    parameter = character(),
    units = character(),
    source = character(),
    approval = character(),
    quality_flag = character()
  )
}
