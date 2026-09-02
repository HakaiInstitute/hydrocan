# Yukon Water Science and Stewardship adapter
#
# Adapter written by Marek Boulerice
#
# Adapter for public hydrometric data from the Yukon Small Stream Network.
# Data retrieved from the Government of Yukon Water Data API.
#
# The adapter provides:
# - station identifiers and metadata;
# - sub-daily flow and water-level measurements;
# - calculated daily flow and water-level measurements.
#
# Internal workflow:
# location_code -> timeseries_id -> measurements -> HydroCan schema

YUKON_API_BASE <-
  "https://service.yukon.ca/water-data/api/v2"

YUKON_NETWORK <-
  "Yukon Small Stream Network"

YUKON_FLOW_PARAMETER <- "water flow"
YUKON_LEVEL_PARAMETER <- "water level"

YUKON_FLOW_UNIT <- "m\u00B3/s"
YUKON_LEVEL_UNIT <- "m"

YUKON_SOURCE_AGGREGATION_TYPE <- "instantaneous"

YUKON_PROVIDER_NAME <- "yukon_wss"

YUKON_MEASUREMENT_TIMEZONE <- "UTC"

# 1. Locations ------------------------------------------------------------

#' Retrieve Yukon Small Stream Network locations
#'
#' Requests all public locations from the Government of Yukon Water Data API
#' and retains only locations belonging to the Yukon Small Stream Network.
#'
#' This is an internal data-access helper. It returns fields from the Yukon API
#' and does not transform them into HydroCan's station metadata schema.
#'
#' @return A data frame with one row per Yukon Small Stream Network location.
#'   Fields include the internal location identifier, public location code,
#'   station name, coordinates, elevation, datum, network membership, and
#'   other location metadata supplied by the Yukon API.
#'
#' @keywords internal
#' @noRd
.yukon_get_locations <- function() {
  response <- httr2::request(
    paste0(YUKON_API_BASE, "/locations")
  ) |>
    httr2::req_url_query(
      lang = "en",
      format = "json"
    ) |>
    httr2::req_perform()

  locations <- httr2::resp_body_json(
    response,
    simplifyVector = TRUE
  )

  locations |>
    dplyr::filter(
      grepl(
        YUKON_NETWORK,
        locations$networks,
        fixed = TRUE
      )
    )
}

#' List Yukon Small Stream Network station identifiers
#'
#' Retrieves Yukon Small Stream Network locations and returns the public
#' location codes used as station identifiers by the HydroCan adapter.
#'
#' @return A character vector of unique Yukon station identifiers.
#'
#' @keywords internal
#' @noRd
.yukon_list_stations <- function() {
  locations <- .yukon_get_locations()

  unique(locations$location_code)
}


#' Retrieve Yukon station metadata
#'
#' Retrieves location information for the Yukon Small Stream Network and
#' transforms it into HydroCan's standard station metadata schema.
#'
#' Note: Record start and end dates currently returned as NA as
#' they are not available from the Yukon API locations endpoint.
#' Additional Yukon-specific metadata retained in the `notes` list-column.
#'
#' @return A tibble with the following columns:
#'   \describe{
#'     \item{station_id}{Character Yukon location code.}
#'     \item{station_name}{Character station name.}
#'     \item{provider_name}{Character HydroCan provider identifier.}
#'     \item{longitude}{Numeric longitude in decimal degrees.}
#'     \item{latitude}{Numeric latitude in decimal degrees.}
#'     \item{elevation_m}{Numeric station elevation in metres.}
#'     \item{period_start}{Start of the available record as a `Date`.}
#'     \item{period_end}{End of the available record as a `Date`.}
#'     \item{notes}{List-column containing Yukon-specific fields such as datum,
#'       location type, alias, and location note.}
#'   }
#'
#' @keywords internal
#' @noRd
.yukon_list_stations_meta <- function() {
  locations <- .yukon_get_locations()
  locations$start <- locations$end <- NA

  ts <- .yukon_get_timeseries_catalogue()

  # Take the min and max datetime reported for each location
  starts <- tapply(ts$start_datetime, ts$location_id, min, na.rm = TRUE)
  ends <- tapply(ts$end_datetime, ts$location_id, max, na.rm = TRUE)

  locations$start <- unname(starts[as.character(locations$location_id)])
  locations$end <- unname(ends[as.character(locations$location_id)])

  tibble::tibble(
    station_id = as.character(locations$location_code),
    station_name = as.character(locations$name),
    provider_name = YUKON_PROVIDER_NAME,
    longitude = as.double(locations$longitude),
    latitude = as.double(locations$latitude),
    elevation_m = as.double(locations$elevation),
    period_start = as.Date(locations$start),
    period_end = as.Date(locations$end),
    notes = lapply(
      seq_len(nrow(locations)),
      function(i) {
        list(
          datum = locations$datum[[i]],
          location_type = locations$location_type[[i]],
          alias = locations$alias[[i]],
          note = locations$note[[i]]
        )
      }
    )
  )
}

# 2. Timeseries catalogue -------------------------------------------------

#' Retrieve a Yukon timeseries catalogue
#'
#' Requests a catalogue of Yukon Water Data API timeseries, retains active and
#' publicly visible series belonging to the Yukon Small Stream Network, and
#' joins each series to its public Yukon location code.
#'
#' Each returned row describes an available timeseries, such as instantaneous
#' water flow or water level at one station. This function does not retrieve
#' measurement values.
#'
#' @return A data frame containing Yukon timeseries metadata. Important fields
#'   include `timeseries_id`, `location_id`, `location_code`,
#'   `parameter_name`, `units`, `aggregation_type`, `start_datetime`, and
#'   `end_datetime`.
#'
#' @keywords internal
#' @noRd

.yukon_get_timeseries_catalogue <- function() {
  response <- httr2::request(
    paste0(YUKON_API_BASE, "/timeseries")
  ) |>
    httr2::req_url_query(
      lang = "en",
      format = "json"
    ) |>
    httr2::req_perform()

  timeseries <- httr2::resp_body_json(
    response,
    simplifyVector = TRUE
  )

  locations <- .yukon_get_locations()

  timeseries <- timeseries |>
    dplyr::filter(
      grepl(
        YUKON_NETWORK,
        timeseries$networks,
        fixed = TRUE
      ),
      timeseries$publicly_visible,
      timeseries$active
    ) |>
    dplyr::left_join(
      locations |>
        dplyr::select(
          "location_id",
          "location_code"
        ),
      by = "location_id"
    )
  timeseries[!is.na(timeseries$location_code), ]
}

# 3. Measurements ---------------------------------------------------------

#' Retrieve Yukon sub-daily measurements
#'
#' Requests corrected sub-daily measurements for one Yukon API timeseries over
#' a specified date range.
#'
#' This is an internal helper function. It returns Yukon API fields and does
#' not transform them into HydroCan's realtime schema.
#'
#' @param timeseries_id Integer or character identifier for a Yukon API timeseries.
#' @param start_date Start of the requested range. A `Date` or character value
#'   coercible to the format `YYYY-MM-DD`.
#' @param end_date End of the requested range. A `Date` or character value
#'   coercible to the format `YYYY-MM-DD`.
#' @param limit Positive integer specifying the maximum number of records
#'   requested from the API. Defaults to `100000L`.
#' @return A data frame containing Yukon measurement fields, including
#'   `timeseries_id`, `datetime`, `value_raw`, `value_corrected`, and available
#'   grade, approval, qualifier, owner, and contributor metadata. When no data
#'   exist for the requested range, the API returns a one-row data frame with
#'   `status` and `message` columns.
#'
#' @keywords internal
#' @noRd

.yukon_get_measurements <- function(
  timeseries_id,
  start_date,
  end_date,
  limit = 100000L
) {
  response <- httr2::request(
    paste0(YUKON_API_BASE, "/timeseries/measurements")
  ) |>
    httr2::req_url_query(
      id = timeseries_id,
      start = as.character(start_date),
      end = as.character(end_date),
      limit = limit,
      format = "json"
    ) |>
    httr2::req_perform()

  measurements <- httr2::resp_body_json(
    response,
    simplifyVector = TRUE
  )

  measurements
}

#' Retrieve Yukon calculated daily measurements
#'
#' Requests calculated daily measurements for one Yukon API timeseries over a
#' specified date range.
#'
#' Daily values are calculated by the Yukon data system from the underlying
#' corrected measurements using the aggregation method and daily timezone
#' configured for the timeseries.
#'
#' This is an internal helper function. It returns Yukon API fields and does
#' not transform them into HydroCan's daily schema.
#'
#' @param timeseries_id Integer or character identifier for a Yukon API
#'   timeseries.
#' @param start_date Start of the requested range. A `Date` or character value
#'   coercible to the format `YYYY-MM-DD`.
#' @param end_date End of the requested range. A `Date` or character value
#'   coercible to the format `YYYY-MM-DD`.
#' @param stats Logical value indicating whether the API should include
#'   additional daily and historical summary statistics. Defaults to `FALSE`.
#'
#' @return A data frame containing `timeseries_id`, `date`, `day_timezone`,
#'   `value`, and `imputed` when `stats = FALSE`. Additional statistical fields
#'   may be returned when `stats = TRUE`. When no data exist for the requested
#'   range, the API returns a one-row data frame with `status` and `message`
#'   columns.
#'
#' @keywords internal
#' @noRd

.yukon_get_daily_measurements <- function(
  timeseries_id,
  start_date,
  end_date,
  stats = FALSE
) {
  response <- httr2::request(
    paste0(
      YUKON_API_BASE,
      "/timeseries/measurementsDaily"
    )
  ) |>
    httr2::req_url_query(
      id = timeseries_id,
      start = as.character(start_date),
      end = as.character(end_date),
      stats = stats,
      format = "json"
    ) |>
    httr2::req_perform()

  httr2::resp_body_json(
    response,
    simplifyVector = TRUE
  )
}

#' Fetch sub-daily Yukon flow measurements
#'
#' Finds the active public instantaneous flow timeseries for a Yukon Small
#' Stream Network station, requests corrected measurements for the specified
#' date range, and transforms the result into HydroCan's realtime schema.
#'
#' @param station_id Character Yukon station identifier.
#' @param start_date Start of the requested range as a `Date` or `YYYY-MM-DD`
#'   character value.
#' @param end_date End of the requested range as a `Date` or `YYYY-MM-DD`
#'   character value.
#'
#' @return A tibble with columns `station_id`, `timestamp`, `value`,
#'   `parameter`, `unit`, `provider_name`, `quality_code`, and `qf_desc`.
#'   Returns a zero-row tibble with the same schema when the station has no
#'   matching public flow series or the requested range contains no
#'   measurements.
#'
#' @keywords internal
#' @noRd

.yukon_fetch_flows <- function(
  station_id,
  start_date,
  end_date
) {
  # Retrieve the filtered Yukon Small Stream timeseries catalogue.
  catalogue <- .yukon_get_timeseries_catalogue()

  # Find the single instantaneous flow series belonging to this station.
  flow_series <- catalogue |>
    dplyr::filter(
      catalogue$location_code == station_id,
      catalogue$parameter_name == YUKON_FLOW_PARAMETER,
      catalogue$units == YUKON_FLOW_UNIT,
      catalogue$aggregation_type == YUKON_SOURCE_AGGREGATION_TYPE
    )

  # Some listed stations may not have a public flow series.
  if (nrow(flow_series) == 0L) {
    return(.empty_realtime_tibble())
  }

  # Multiple matching series would be ambiguous and should be investigated.
  if (nrow(flow_series) > 1L) {
    stop(
      "Multiple active public flow timeseries found for station '",
      station_id,
      "'.",
      call. = FALSE
    )
  }

  # Request raw measurements using the matching Yukon timeseries ID.
  measurements <- .yukon_get_measurements(
    timeseries_id = flow_series$timeseries_id[[1]],
    start_date = start_date,
    end_date = end_date
  )

  # API returns a status/message response when no measurements exist.
  if (
    all(c("status", "message") %in% names(measurements)) &&
      identical(measurements$status[[1]], "info")
  ) {
    return(.empty_realtime_tibble())
  }

  # Transform Yukon API fields into HydroCan's realtime schema.
  tibble::tibble(
    station_id = as.character(station_id),
    timestamp = as.POSIXct(
      measurements$datetime,
      format = "%Y-%m-%dT%H:%M:%S",
      tz = YUKON_MEASUREMENT_TIMEZONE
    ),
    value = as.double(measurements$value_corrected),
    parameter = "water_discharge",
    unit = as.character(flow_series$units[[1]]),
    provider_name = YUKON_PROVIDER_NAME,
    quality_code = as.character(measurements$grade_type_id),
    qf_desc = paste(
      measurements$grade_type_description,
      measurements$approval_type_description,
      measurements$qualifier_type_descriptions,
      sep = "; "
    )
  )
}

#' Fetch sub-daily Yukon water-level measurements
#'
#' Finds the active public instantaneous water-level timeseries for a Yukon
#' Small Stream Network station, requests corrected measurements for the
#' specified date range, and transforms the result into HydroCan's realtime
#' schema.
#'
#' @param station_id Character Yukon station identifier.
#' @param start_date Start of the requested range as a `Date` or `YYYY-MM-DD`
#'   character value.
#' @param end_date End of the requested range as a `Date` or `YYYY-MM-DD`
#'   character value.
#'
#' @return A tibble with columns `station_id`, `timestamp`, `value`,
#'   `parameter`, `unit`, `provider_name`, `quality_code`, and `qf_desc`.
#'   Returns a zero-row tibble with the same schema when the station has no
#'   matching public water-level series or the requested range contains no
#'   measurements.
#'
#' @keywords internal
#' @noRd

.yukon_fetch_levels <- function(
  station_id,
  start_date,
  end_date
) {
  # Retrieve the filtered Yukon Small Stream timeseries catalogue.
  catalogue <- .yukon_get_timeseries_catalogue()

  # Find the single instantaneous level series belonging to this station.
  level_series <- catalogue |>
    dplyr::filter(
      catalogue$location_code == station_id,
      catalogue$parameter_name == YUKON_LEVEL_PARAMETER,
      catalogue$units == YUKON_LEVEL_UNIT,
      catalogue$aggregation_type == YUKON_SOURCE_AGGREGATION_TYPE
    )

  # Some listed stations may not have a public level series.
  if (nrow(level_series) == 0L) {
    return(.empty_realtime_tibble())
  }

  # Multiple matching series would be ambiguous and should be investigated.
  if (nrow(level_series) > 1L) {
    stop(
      "Multiple active public level timeseries found for station '",
      station_id,
      "'.",
      call. = FALSE
    )
  }

  # Request raw measurements using the matching Yukon timeseries ID.
  measurements <- .yukon_get_measurements(
    timeseries_id = level_series$timeseries_id[[1]],
    start_date = start_date,
    end_date = end_date
  )

  # The API returns a status/message response when no measurements exist.
  if (
    all(c("status", "message") %in% names(measurements)) &&
      identical(measurements$status[[1]], "info")
  ) {
    return(.empty_realtime_tibble())
  }

  # Transform Yukon API fields into HydroCan's realtime schema.
  tibble::tibble(
    station_id = as.character(station_id),
    timestamp = as.POSIXct(
      measurements$datetime,
      format = "%Y-%m-%dT%H:%M:%S",
      tz = YUKON_MEASUREMENT_TIMEZONE
    ),
    value = as.double(measurements$value_corrected),
    parameter = "water_level",
    unit = as.character(level_series$units[[1]]),
    provider_name = YUKON_PROVIDER_NAME,
    quality_code = as.character(measurements$grade_type_id),
    qf_desc = paste(
      measurements$grade_type_description,
      measurements$approval_type_description,
      measurements$qualifier_type_descriptions,
      sep = "; "
    )
  )
}

#' Fetch daily Yukon flow measurements
#'
#' Finds the active public instantaneous flow timeseries for a Yukon Small
#' Stream Network station, requests its calculated daily values, and transforms
#' the result into HydroCan's daily schema.
#'
#' Daily quality fields returned as missing since daily Yukon API
#' response does not currently include provider grade, approval, or qualifier
#' information.
#'
#' @param station_id Character Yukon station identifier.
#' @param start_date Start of the requested range as a `Date` or `YYYY-MM-DD`
#'   character value.
#' @param end_date End of the requested range as a `Date` or `YYYY-MM-DD`
#'   character value.
#'
#' @return A tibble with columns `station_id`, `date`, `value`, `parameter`,
#'   `unit`, `provider_name`, `quality_code`, and `qf_desc`. Returns a zero-row
#'   tibble with the same schema when the station has no matching public flow
#'   series or the requested range contains no daily measurements.
#'
#' @keywords internal
#' @noRd

.yukon_fetch_daily_flows <- function(
  station_id,
  start_date,
  end_date
) {
  # Retrieve the filtered Yukon Small Stream timeseries catalogue.
  catalogue <- .yukon_get_timeseries_catalogue()

  # Daily measurements are calculated from the station's instantaneous
  # flow series, so the same timeseries catalogue filters are used here.
  flow_series <- catalogue |>
    dplyr::filter(
      catalogue$location_code == station_id,
      catalogue$parameter_name == YUKON_FLOW_PARAMETER,
      catalogue$units == YUKON_FLOW_UNIT,
      catalogue$aggregation_type == YUKON_SOURCE_AGGREGATION_TYPE
    )

  # Some listed stations may not have a public flow series.
  if (nrow(flow_series) == 0L) {
    return(.empty_daily_tibble())
  }

  # Multiple matching series would be ambiguous and should be investigated.
  if (nrow(flow_series) > 1L) {
    stop(
      "Multiple active public flow timeseries found for station '",
      station_id,
      "'.",
      call. = FALSE
    )
  }

  # Request calculated daily measurements using the matching
  # Yukon timeseries ID.
  measurements <- .yukon_get_daily_measurements(
    timeseries_id = flow_series$timeseries_id[[1]],
    start_date = start_date,
    end_date = end_date
  )

  # The API returns a status/message response when no measurements exist.
  if (
    all(c("status", "message") %in% names(measurements)) &&
      identical(measurements$status[[1]], "info")
  ) {
    return(.empty_daily_tibble())
  }

  # Transform Yukon API fields into HydroCan's daily schema.
  tibble::tibble(
    station_id = as.character(station_id),
    date = as.Date(measurements$date),
    value = as.double(measurements$value),
    parameter = "water_discharge",
    unit = as.character(flow_series$units[[1]]),
    provider_name = YUKON_PROVIDER_NAME,
    quality_code = NA_character_,
    qf_desc = NA_character_
  )
}

#' Fetch daily Yukon water-level measurements
#'
#' Finds the active public instantaneous water-level timeseries for a Yukon
#' Small Stream Network station, requests its calculated daily values, and
#' transforms the result into HydroCan's daily schema.
#'
#' Daily quality fields are returned as missing as daily Yukon API
#' response does not currently include provider grade, approval, or qualifier
#' information.
#'
#' @param station_id Character Yukon station identifier.
#' @param start_date Start of the requested range as a `Date` or `YYYY-MM-DD`
#'   character value.
#' @param end_date End of the requested range as a `Date` or `YYYY-MM-DD`
#'   character value.
#'
#' @return A tibble with columns `station_id`, `date`, `value`, `parameter`,
#'   `unit`, `provider_name`, `quality_code`, and `qf_desc`. Returns a zero-row
#'   tibble with the same schema when the station has no matching public
#'   water-level series or the requested range contains no daily measurements.
#'
#' @keywords internal
#' @noRd

.yukon_fetch_daily_levels <- function(
  station_id,
  start_date,
  end_date
) {
  # Retrieve the filtered Yukon Small Stream timeseries catalogue.
  catalogue <- .yukon_get_timeseries_catalogue()

  # Daily measurements are calculated from the station's instantaneous
  # level series, so the same timeseries catalogue filters are used here.
  level_series <- catalogue |>
    dplyr::filter(
      catalogue$location_code == station_id,
      catalogue$parameter_name == YUKON_LEVEL_PARAMETER,
      catalogue$units == YUKON_LEVEL_UNIT,
      catalogue$aggregation_type == YUKON_SOURCE_AGGREGATION_TYPE
    )

  # Some listed stations may not have a public level series.
  if (nrow(level_series) == 0L) {
    return(.empty_daily_tibble())
  }

  # Multiple matching series would be ambiguous and should be investigated.
  if (nrow(level_series) > 1L) {
    stop(
      "Multiple active public level timeseries found for station '",
      station_id,
      "'.",
      call. = FALSE
    )
  }

  # Request calculated daily measurements using the matching
  # Yukon timeseries ID.
  measurements <- .yukon_get_daily_measurements(
    timeseries_id = level_series$timeseries_id[[1]],
    start_date = start_date,
    end_date = end_date
  )

  # API returns a status/message response when no measurements exist.
  if (
    all(c("status", "message") %in% names(measurements)) &&
      identical(measurements$status[[1]], "info")
  ) {
    return(.empty_daily_tibble())
  }

  # Transform Yukon API fields into HydroCan's daily schema.
  tibble::tibble(
    station_id = as.character(station_id),
    date = as.Date(measurements$date),
    value = as.double(measurements$value),
    parameter = "water_level",
    unit = as.character(level_series$units[[1]]),
    provider_name = YUKON_PROVIDER_NAME,
    quality_code = NA_character_,
    qf_desc = NA_character_
  )
}

#' Create the Yukon Water Science and Stewardship adapter
#'
#' Constructs a HydroCan adapter for public hydrometric data from the Yukon
#' Small Stream Network. The adapter provides station metadata, sub-daily flow
#' and water-level observations, and calculated daily flow and water-level
#' values from the Government of Yukon Water Data API.
#'
#' @return A `hydrocan_adapter` object containing the Yukon station-list,
#'   station-metadata, realtime measurement, and daily measurement functions,
#'   together with source and citation metadata.
#'
#' @keywords internal
#' @noRd

hydrocan_adapter_yukon_wss <- function() {
  new_hydrocan_adapter(
    name = YUKON_PROVIDER_NAME,
    description = "Government of Yukon hydrometric data for the Yukon Small Stream Network, administered by the Department of Environment Water Science and Stewardship Branch. Provides sub-daily and calculated daily flow and water-level data for all periods of record and in real time. Active and inactive locations are included. Be aware that data may be revised from time to time to reflect field-measured conditions and rating curve adjustments.",
    list_stations_fn = .yukon_list_stations,
    fetch_flows_fn = .yukon_fetch_flows,
    fetch_daily_flows_fn = .yukon_fetch_daily_flows,
    fetch_levels_fn = .yukon_fetch_levels,
    fetch_daily_levels_fn = .yukon_fetch_daily_levels,
    list_stations_meta_fn = .yukon_list_stations_meta,
    title = "Yukon Small Stream Network Hydrometric Data",
    publisher = "Government of Yukon, Department of Environment, Water Science and Stewardship Branch",
    license = "Open Government Licence - Yukon",
    license_url = "https://yukon.ca/en/your-government/open-government/open-government-licence-yukon",
    terms_url = NULL,
    docs_url = paste0(
      YUKON_API_BASE,
      "/__docs__/"
    )
  )
}
