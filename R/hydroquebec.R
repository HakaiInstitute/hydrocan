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

  req <- .hydrocan_request(.HQ_API_URL) |>
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

  req <- .hydrocan_request(.HQ_API_URL) |>
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

# Common select fields used by both fetch functions.
.HQ_SELECT <- "identifiant,split_date,split_value,depil_json_nom_unite_mesure,depil_json_type_point_donnee"

# Fetch hourly observations for one station within [start_date, end_date].
.hq_fetch_flows <- function(station_number, start_date, end_date) {
  where <- paste0(
    'identifiant="',
    station_number,
    '" AND depil_json_pas_temps="Horaire"'
  )
  df <- .hq_collect(where, .HQ_SELECT)
  if (is.null(df)) {
    return(.empty_flows_tibble())
  }

  result <- tibble::tibble(
    station_number = df$identifiant,
    datetime = .hq_parse_datetime(df$split_date),
    value = suppressWarnings(as.numeric(df$split_value)),
    parameter = "flow",
    units = df$depil_json_nom_unite_mesure,
    source = "hydroquebec",
    approval = NA_character_,
    quality_flag = df$depil_json_type_point_donnee
  )

  # Date-range filter applied in R: split_date is stored as text in the API.
  start_posix <- as.POSIXct(paste0(format(start_date), " 00:00:00"), tz = "UTC")
  end_posix <- as.POSIXct(paste0(format(end_date), " 23:59:59"), tz = "UTC")
  result[
    !is.na(result$datetime) &
      result$datetime >= start_posix &
      result$datetime <= end_posix,
  ]
}

# Fetch source-native daily summaries for one station within [start_date, end_date].
.hq_fetch_daily_flows <- function(station_number, start_date, end_date) {
  where <- paste0(
    'identifiant="',
    station_number,
    '" AND depil_json_pas_temps="Journalier"'
  )
  df <- .hq_collect(where, .HQ_SELECT)
  if (is.null(df)) {
    return(.empty_daily_flows_tibble())
  }

  result <- tibble::tibble(
    station_number = df$identifiant,
    date = as.Date(.hq_parse_datetime(df$split_date), tz = "UTC"),
    value = suppressWarnings(as.numeric(df$split_value)),
    parameter = "flow",
    units = df$depil_json_nom_unite_mesure,
    source = "hydroquebec",
    approval = NA_character_,
    quality_flag = df$depil_json_type_point_donnee
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

  req <- .hydrocan_request(.HQ_API_URL) |>
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
    station_number = df$identifiant,
    station_name = df$nom,
    source = "hydroquebec",
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
    fetch_flows_fn        = .hq_fetch_flows,
    fetch_daily_flows_fn  = .hq_fetch_daily_flows,
    list_stations_meta_fn = .hq_list_stations_meta
  )
}
