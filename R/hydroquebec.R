# Hydro-Quebec adapter -------------------------------------------------------
#
# Data source: Hydro-Quebec open data portal (Opendatasoft platform)
# URL: https://donnees.hydroquebec.com/explore/dataset/donnees-hydrometriques/
#
# Provides flow measurements at Hydro-Quebec generation facilities. The dataset
# contains both hourly ("Horaire") and pre-computed daily ("Journalier")
# records. fetch_flows_fn returns hourly observations only; fetch_daily_flows_fn
# returns the source-native daily values. No authentication required.
#
# Station IDs use Hydro-Quebec's internal "identifiant" format (e.g., "3-230").
# These are entirely separate from the WSC federal network.

.HQ_API_URL <- "https://donnees.hydroquebec.com/api/explore/v2.1/catalog/datasets/donnees-hydrometriques/records"

# The Hydro-Quebec server returns a malformed content-security-policy header
# that libcurl rejects over HTTP/2. Forcing HTTP/1.1 avoids the framing error.
.hq_request <- function() {
  .hydrocan_request(.HQ_API_URL) |>
    httr2::req_options(http_version = 2L)
}

# Collect the $results data.frame from each ODS API response and bind into one.
.hq_bind_pages <- function(resps) {
  pages <- Filter(
    \(x) !is.null(x) && nrow(x) > 0L,
    lapply(resps, \(r) httr2::resp_body_json(r, simplifyVector = TRUE)$results)
  )
  if (length(pages) == 0L) NULL else dplyr::bind_rows(pages)
}

# Paginate through all records matching an ODSQL where clause, returning a
# combined data.frame. Uses req_perform_iterative() so the loop is driven by
# httr2 rather than a manual repeat construct.
.hq_collect <- function(where, select) {
  limit <- 100L

  req <- .hq_request() |>
    httr2::req_url_query(
      where = where,
      select = select,
      order_by = "split_date asc",
      limit = limit,
      offset = 0L
    )

  .hq_bind_pages(httr2::req_perform_iterative(
    req,
    next_req = .offset_next_req(limit)
  ))
}

# Returns all unique station identifiers available in the dataset, paginating
# through the full station list rather than relying on a fixed upper bound.
.hq_list_stations <- function() {
  limit <- 100L

  req <- .hq_request() |>
    httr2::req_url_query(
      select = "identifiant",
      group_by = "identifiant",
      limit = limit,
      offset = 0L
    )

  resps <- httr2::req_perform_iterative(req, next_req = .offset_next_req(limit))

  ids <- lapply(resps, \(r) {
    httr2::resp_body_json(r, simplifyVector = TRUE)$results$identifiant
  })
  unique(unlist(ids))
}

# Parse Hydro-Quebec's non-standard datetime format ("2026/04/14T00:00:00Z")
# which uses slashes instead of hyphens in the date portion.
.hq_parse_datetime <- function(x) {
  as.POSIXct(
    gsub("/", "-", x, fixed = TRUE),
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
}

# Map depil_json_type_point_donnee to hydrocan parameter vocabulary. "Débit
# total" is the total outflow; spilled and turbined flows are sub-types keyed
# by prefix; "Apport filtré" is the naturalized catchment inflow computed by
# removing upstream reservoir operations from the water balance.
.hq_map_parameter <- function(type) {
  dplyr::case_when(
    type == "D\u00e9bit total" ~ "water_discharge",
    type == "Apport filtr\u00e9" ~ "water_inflow",
    startsWith(
      type,
      "D\u00e9bit d\u00e9vers\u00e9"
    ) ~ "water_discharge_spilled",
    startsWith(type, "D\u00e9bit turbin\u00e9") ~ "water_discharge_turbined",
    TRUE ~ NA_character_
  )
}

# Common select fields used by both fetch functions.
.HQ_SELECT <- "identifiant,split_date,split_value,depil_json_nom_unite_mesure,depil_json_type_point_donnee"

# Fetch hourly observations for one station within [start_date, end_date].
.hq_fetch_flows <- function(station_id, start_date, end_date) {
  where <- paste0(
    'identifiant="',
    station_id,
    '" AND depil_json_pas_temps="Horaire"'
  )
  df <- .hq_collect(where, .HQ_SELECT)
  if (is.null(df)) {
    return(.empty_realtime_tibble())
  }

  result <- tibble::tibble(
    station_id = df$identifiant,
    timestamp = .hq_parse_datetime(df$split_date),
    value = suppressWarnings(as.numeric(df$split_value)),
    parameter = .hq_map_parameter(df$depil_json_type_point_donnee),
    unit = df$depil_json_nom_unite_mesure,
    provider_name = "hydroquebec",
    quality_code = NA_character_,
    qf_desc = NA_character_
  )

  # Date-range filter applied in R: split_date is stored as text in the API.
  start_posix <- as.POSIXct(paste0(format(start_date), " 00:00:00"), tz = "UTC")
  end_posix <- as.POSIXct(paste0(format(end_date), " 23:59:59"), tz = "UTC")
  result[
    !is.na(result$timestamp) &
      result$timestamp >= start_posix &
      result$timestamp <= end_posix,
  ]
}

# Fetch source-native daily summaries for one station within [start_date, end_date].
.hq_fetch_daily_flows <- function(station_id, start_date, end_date) {
  where <- paste0(
    'identifiant="',
    station_id,
    '" AND depil_json_pas_temps="Journalier"'
  )
  df <- .hq_collect(where, .HQ_SELECT)
  if (is.null(df)) {
    return(.empty_daily_tibble())
  }

  result <- tibble::tibble(
    station_id = df$identifiant,
    date = as.Date(.hq_parse_datetime(df$split_date), tz = "UTC"),
    value = suppressWarnings(as.numeric(df$split_value)),
    parameter = .hq_map_parameter(df$depil_json_type_point_donnee),
    unit = df$depil_json_nom_unite_mesure,
    provider_name = "hydroquebec",
    quality_code = NA_character_,
    qf_desc = NA_character_
  )

  result[
    !is.na(result$date) & result$date >= start_date & result$date <= end_date,
  ]
}

# Fetch station metadata for all Hydro-Quebec sites. Each row in the returned
# tibble corresponds to one unique station; observation-level fields are
# excluded by selecting only the station descriptor columns.
.hq_list_stations_meta <- function() {
  select <- "identifiant,nom,xcoord,ycoord,zcoord,date_debut,date_fin,coderegionqc,regionqc"
  limit <- 100L

  req <- .hq_request() |>
    httr2::req_url_query(
      select = select,
      group_by = "identifiant,nom,xcoord,ycoord,zcoord,date_debut,date_fin,coderegionqc,regionqc",
      limit = limit,
      offset = 0L
    )

  df <- .hq_bind_pages(httr2::req_perform_iterative(
    req,
    next_req = .offset_next_req(limit)
  ))
  if (is.null(df)) {
    return(.empty_stations_tibble())
  }

  tibble::tibble(
    station_id = df$identifiant,
    station_name = df$nom,
    provider_name = "hydroquebec",
    longitude = df$xcoord,
    latitude = df$ycoord,
    elevation_m = suppressWarnings(as.double(df$zcoord)),
    period_start = as.Date(df$date_debut),
    period_end = as.Date(df$date_fin),
    notes = lapply(
      seq_len(nrow(df)),
      \(i) {
        list(region_code = df$coderegionqc[[i]], region_name = df$regionqc[[i]])
      }
    )
  )
}

#' @keywords internal
hydrocan_adapter_hydroquebec <- function() {
  new_hydrocan_adapter(
    "hydroquebec",
    paste(
      "Hydro-Quebec open data (Opendatasoft platform).",
      "Flow data only; no water level.",
      "Rolling window of approximately 10 days - historical data is not available."
    ),
    .hq_list_stations,
    fetch_flows_fn = .hq_fetch_flows,
    fetch_daily_flows_fn = .hq_fetch_daily_flows,
    list_stations_meta_fn = .hq_list_stations_meta,
    title = "D\u00e9bits et apports naturels aux installations d\u2019Hydro-Qu\u00e9bec",
    publisher = "Hydro-Qu\u00e9bec",
    license = "CC BY-NC 4.0",
    license_url = "https://creativecommons.org/licenses/by-nc/4.0/",
    terms_url = "https://www.hydroquebec.com/documents-data/open-data/licence.html",
    docs_url = "https://donnees.hydroquebec.com/explore/dataset/donnees-hydrometriques/information/"
  )
}
