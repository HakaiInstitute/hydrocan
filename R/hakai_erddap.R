# Hakai ERDDAP adapter -------------------------------------------------------
#
# Data source: Hakai Institute watershed stream stations (provisional)
# ERDDAP server: https://catalogue.hakai.org/erddap
# Dataset: HakaiWatershedsStreamStationsProvisional
#
# Provides sub-daily stage and discharge for small watersheds on Calvert and
# Hecate Islands, BC. Data is served via ERDDAP's tabledap REST API, which
# returns CSV with a variable-names header row followed by a units row. All
# queries are built with httr2 directly; no rerddap dependency is needed.
#
# Station IDs follow the Hakai internal format (e.g., "703"). These are
# entirely separate from WSC federal station IDs.

.HAKAI_ERDDAP_BASE_URL <- "https://catalogue.hakai.org/erddap"
.HAKAI_ERDDAP_DATASET <- "HakaiWatershedsStreamStationsProvisional"

# Build and execute a tabledap CSV query. The ERDDAP CSV format has a variable-
# names header on row 1 and a units row on row 2; this function drops the units
# row before returning, so all columns are character class data values.
#
# Returns NULL on 404 (no matching rows), otherwise a data.frame.
.hakai_erddap_query <- function(fields, constraints = character(0)) {
  query <- paste(
    c(paste(fields, collapse = ","), constraints),
    collapse = "&"
  )
  # Double quotes wrapping string constraint values are not valid unencoded in
  # a URL; encode them explicitly while leaving operators like >= and <= as-is,
  # which ERDDAP accepts in both encoded and unencoded form.
  query <- gsub('"', "%22", query, fixed = TRUE)

  url <- paste0(
    .HAKAI_ERDDAP_BASE_URL,
    "/tabledap/",
    .HAKAI_ERDDAP_DATASET,
    ".csv?",
    query
  )

  resp <- tryCatch(
    httr2::req_perform(.hydrocan_request(url)),
    httr2_http_404 = function(e) NULL
  )
  if (is.null(resp)) {
    return(NULL)
  }

  df <- utils::read.csv(
    text = httr2::resp_body_string(resp),
    colClasses = "character",
    na.strings = c("NaN", "NA"),
    check.names = FALSE
  )
  df[-1L, , drop = FALSE]
}

.hakai_erddap_list_stations <- function() {
  hakai_erddap_stations$station_id
}

.hakai_erddap_list_stations_meta <- function() {
  hakai_erddap_stations
}

.hakai_erddap_fetch_flows <- function(station_id, start_date, end_date) {
  df <- .hakai_erddap_query(
    c("station_id", "time", "discharge_rate", "discharge_rate_qc"),
    c(
      paste0('station_id="', station_id, '"'),
      paste0("time>=", format(start_date, "%Y-%m-%dT00:00:00Z")),
      paste0("time<=", format(end_date, "%Y-%m-%dT23:59:59Z"))
    )
  )
  if (is.null(df) || nrow(df) == 0L) {
    return(.empty_realtime_tibble())
  }

  tibble::tibble(
    station_id = df$station_id,
    timestamp = as.POSIXct(df$time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    value = as.double(df$discharge_rate),
    parameter = "water_discharge",
    unit = "m3/s",
    provider_name = "hakai_erddap",
    quality_code = df$discharge_rate_qc,
    qf_desc = NA_character_
  )
}

.hakai_erddap_fetch_levels <- function(station_id, start_date, end_date) {
  df <- .hakai_erddap_query(
    c("station_id", "time", "stage", "stage_qc"),
    c(
      paste0('station_id="', station_id, '"'),
      paste0("time>=", format(start_date, "%Y-%m-%dT00:00:00Z")),
      paste0("time<=", format(end_date, "%Y-%m-%dT23:59:59Z"))
    )
  )
  if (is.null(df) || nrow(df) == 0L) {
    return(.empty_realtime_tibble())
  }

  tibble::tibble(
    station_id = df$station_id,
    timestamp = as.POSIXct(df$time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    value = as.double(df$stage),
    parameter = "water_level",
    unit = "m",
    provider_name = "hakai_erddap",
    quality_code = df$stage_qc,
    qf_desc = NA_character_
  )
}

#' @keywords internal
hydrocan_adapter_hakai_erddap <- function() {
  new_hydrocan_adapter(
    "hakai_erddap",
    paste(
      "Hakai Institute watershed stream stations (provisional).",
      "Sub-daily flows and water levels for small watersheds on Calvert and",
      "Hecate Islands, BC, Canada. Served via ERDDAP tabledap."
    ),
    .hakai_erddap_list_stations,
    fetch_flows_fn = .hakai_erddap_fetch_flows,
    fetch_levels_fn = .hakai_erddap_fetch_levels,
    list_stations_meta_fn = .hakai_erddap_list_stations_meta,
    title = "Watersheds Stream Stations Timeseries, Calvert and Hecate Islands, BC, Canada (Provisional)",
    publisher = "Hakai Institute",
    license = "CC BY 4.0",
    license_url = "https://creativecommons.org/licenses/by/4.0/",
    terms_url = "https://catalogue.hakai.org/erddap/tabledap/HakaiWatershedsStreamStationsProvisional.html",
    docs_url = "https://catalogue.hakai.org/erddap/info/HakaiWatershedsStreamStationsProvisional/index.html"
  )
}
