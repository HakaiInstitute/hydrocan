# Hydro-Quebec adapter -------------------------------------------------------
#
# Data source: Hydro-Quebec open data portal (Opendatasoft platform)
# URL: https://donnees.hydroquebec.com/explore/dataset/donnees-hydrometriques/
#
# Provides geolocated flow measurements at Hydro-Quebec facilities (inflows,
# spill flows, turbine flows). Data is updated twice daily and covers a rolling
# ~10-day window. No authentication required.
#
# Station IDs use Hydro-Quebec's internal "identifiant" format (e.g., "3-230").
# These are entirely separate from the WSC federal network.

.HQ_API_URL <- "https://donnees.hydroquebec.com/api/explore/v2.1/catalog/datasets/donnees-hydrometriques/records"

# Fetch one page of records from the Hydro-Quebec API using ODSQL filters.
.hq_fetch_page <- function(where, select, limit = 100L, offset = 0L) {
  req <- httr2::request(.HQ_API_URL) |>
    httr2::req_url_query(
      where    = where,
      select   = select,
      order_by = "split_date asc",
      limit    = limit,
      offset   = offset
    )
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

# Returns all unique station identifiers available in the dataset.
.hq_list_stations <- function() {
  resp <- httr2::request(.HQ_API_URL) |>
    httr2::req_url_query(
      select   = "identifiant",
      group_by = "identifiant",
      limit    = 200L
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  resp$results$identifiant
}

# Parse Hydro-Quebec's non-standard datetime format ("2026/04/14T00:00:00Z")
# which uses slashes instead of hyphens in the date portion.
.hq_parse_datetime <- function(x) {
  as.POSIXct(
    gsub("/", "-", x, fixed = TRUE),
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz     = "UTC"
  )
}

# Fetch all flow observations for one station within [start_date, end_date].
# The API's split_date field is stored as text ("YYYY/MM/DDTHH:MM:SSZ"), so
# date-range filtering is applied in R after fetching all records for the
# station. Pagination is handled automatically.
.hq_fetch_flows <- function(station_number, start_date, end_date) {
  where  <- paste0('identifiant="', station_number, '"')
  select <- paste(
    "identifiant", "split_date", "split_value",
    "depil_json_nom_unite_mesure", "depil_json_type_point_donnee",
    sep = ","
  )

  all_records <- list()
  offset <- 0L
  limit  <- 100L

  repeat {
    page    <- .hq_fetch_page(where, select, limit, offset)
    records <- page$results

    if (is.null(records) || nrow(records) == 0L) break

    all_records[[length(all_records) + 1L]] <- records
    offset <- offset + limit

    if (offset >= page$total_count) break
  }

  if (length(all_records) == 0L) {
    return(.empty_realtime_tibble())
  }

  df <- dplyr::bind_rows(all_records)

  result <- tibble::tibble(
    station_number = df$identifiant,
    datetime       = .hq_parse_datetime(df$split_date),
    value          = suppressWarnings(as.numeric(df$split_value)),
    parameter      = "flow",
    units          = df$depil_json_nom_unite_mesure,
    source         = "hydroquebec",
    approval       = "provisional",
    quality_flag   = NA_character_
  )

  # Apply date-range filter in R since split_date is stored as text in the API.
  start_posix <- as.POSIXct(paste0(format(start_date), " 00:00:00"), tz = "UTC")
  end_posix   <- as.POSIXct(paste0(format(end_date),   " 23:59:59"), tz = "UTC")
  result[!is.na(result$datetime) &
           result$datetime >= start_posix &
           result$datetime <= end_posix, ]
}

#' @keywords internal
hydrocan_adapter_hydroquebec <- function() {
  new_hydrocan_adapter("hydroquebec", .hq_list_stations, .hq_fetch_flows)
}
