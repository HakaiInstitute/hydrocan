# CEHQ adapter ---------------------------------------------------------------
#
# Data source: Centre d'expertise hydrique du Québec (CEHQ)
# Station metadata: donneesquebec.ca open data portal (CC-BY 4.0)
# Flow data: cehq.gouv.qc.ca per-station historical text files
#
# Provides validated daily flow records for Quebec's provincial hydrometric
# network. This is entirely separate from the Hydro-Quebec utility adapter,
# which covers reservoir inflows; CEHQ covers natural river gauges.
#
# No authentication required for either source. Sub-daily data exists in the
# CEHQ file system but is essentially empty for all stations examined; only
# fetch_daily_flows_fn is implemented.
#
# Station IDs follow CEHQ's six-digit format (e.g., "030101"). These do not
# overlap with WSC station IDs (which use alphanumeric codes like "05AA008").

.CEHQ_STATIONS_CSV_URL <- paste0(
  "https://www.donneesquebec.ca/recherche/dataset/",
  "c31e2bee-a899-46ca-ad84-5798f0f49676/resource/",
  "6b2d32ef-80e2-445b-9bd1-97ddc39b5d59/download/stations_hydrometriques.csv"
)

.CEHQ_DATA_BASE_URL <-
  "https://www.cehq.gouv.qc.ca/depot/historique_donnees/fichier"

# Fetch and parse the station metadata CSV from the Quebec open data portal.
# Returns the raw data.frame for use by both list_stations_fn and
# list_stations_meta_fn, filtering to open flow stations that have a CEHQ
# historical data link.
.cehq_fetch_stations_csv <- function() {
  resp <- httr2::req_perform(.hydrocan_request(.CEHQ_STATIONS_CSV_URL))
  df <- utils::read.csv(
    text = httr2::resp_body_string(resp, encoding = "UTF-8"),
    colClasses = "character",
    check.names = FALSE
  )
  has_flow <- grepl("D\u00e9bit", df$type, fixed = TRUE)
  is_open <- df$etat == "Ouverte"
  has_link <- startsWith(df$lien_historique, "https://www.cehq.gouv.qc.ca")
  df[has_flow & is_open & has_link, ]
}

.cehq_list_stations <- function() {
  .cehq_fetch_stations_csv()$no
}

.cehq_list_stations_meta <- function() {
  df <- .cehq_fetch_stations_csv()

  # debut/fin are stored as year integers; convert to January 1 of that year.
  year_to_date <- function(x) {
    x <- suppressWarnings(as.integer(x))
    as.Date(ifelse(is.na(x), NA_character_, paste0(x, "-01-01")))
  }

  tibble::tibble(
    station_number = df$no,
    station_name = df$nom,
    source = "cehq",
    longitude = suppressWarnings(as.double(df$longitude)),
    latitude = suppressWarnings(as.double(df$latitude)),
    elevation_m = NA_real_,
    period_start = year_to_date(df$debut),
    period_end = year_to_date(df$fin),
    notes = lapply(
      seq_len(nrow(df)),
      \(i) {
        list(
          regime = df$regime[[i]],
          cours_eau = df$cours_eau[[i]],
          type = df$type[[i]]
        )
      }
    )
  )
}

# Map CEHQ remark codes to hydrocan's approval vocabulary. Codes documented
# in the CEHQ data file headers:
#   E        = estimated
#   P, P*    = provisional
#   all else = approved (gauged, converted, corrected, etc.)
.cehq_remark_to_approval <- function(remark) {
  ifelse(
    is.na(remark),
    "approved",
    ifelse(
      remark == "E",
      "estimated",
      ifelse(
        startsWith(remark, "P"),
        "provisional",
        "approved"
      )
    )
  )
}

# Parse a CEHQ daily observation file. The file suffix selects the parameter:
# "_Q.txt" for discharge (flow, m³/s) or "_N.txt" for stage (level, m).
#
# Each station's full period of record is in one file; there is no server-side
# date filtering so the full file is fetched and trimmed in R. Data lines are
# identified by a leading six-digit station number.
#
# The third column is ambiguous: it is the value when numeric, but some rows
# carry only a remark code with no measurement. The field is tested for numeric
# content to resolve the ambiguity.
.cehq_fetch_daily <- function(
  station_number,
  start_date,
  end_date,
  suffix,
  parameter,
  units
) {
  url <- paste0(.CEHQ_DATA_BASE_URL, "/", station_number, suffix)
  resp <- tryCatch(
    httr2::req_perform(.hydrocan_request(url)),
    error = function(e) NULL
  )
  if (is.null(resp) || httr2::resp_status(resp) != 200L) {
    return(.empty_daily_tibble())
  }

  # Header lines contain French text in windows-1252, but all data lines are
  # pure ASCII. Encoding only matters for header rows, which are discarded.
  text <- httr2::resp_body_string(resp, encoding = "latin1")
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  data_lines <- grep("^[0-9]{6}[[:space:]]", lines, value = TRUE)

  if (length(data_lines) == 0L) {
    return(.empty_daily_tibble())
  }

  raw <- utils::read.table(
    text = paste(data_lines, collapse = "\n"),
    header = FALSE,
    col.names = c("station_id", "date_str", "v_or_r", "remark"),
    colClasses = "character",
    fill = TRUE,
    na.strings = ""
  )

  # When value is absent the remark code sits in the v_or_r column.
  numeric_value <- suppressWarnings(as.numeric(raw$v_or_r))
  has_value <- !is.na(numeric_value)

  result <- tibble::tibble(
    station_number = station_number,
    date = as.Date(raw$date_str, format = "%Y/%m/%d"),
    value = ifelse(has_value, numeric_value, NA_real_),
    parameter = parameter,
    units = units,
    source = "cehq",
    approval = .cehq_remark_to_approval(
      ifelse(has_value, raw$remark, raw$v_or_r)
    ),
    quality_flag = ifelse(has_value, raw$remark, raw$v_or_r)
  )

  result[
    !is.na(result$date) &
      result$date >= start_date &
      result$date <= end_date,
  ]
}

.cehq_fetch_daily_flows <- function(station_number, start_date, end_date) {
  .cehq_fetch_daily(
    station_number,
    start_date,
    end_date,
    "_Q.txt",
    "flow",
    "m3/s"
  )
}

.cehq_fetch_daily_levels <- function(station_number, start_date, end_date) {
  .cehq_fetch_daily(
    station_number,
    start_date,
    end_date,
    "_N.txt",
    "level",
    "m"
  )
}

#' @keywords internal
hydrocan_adapter_cehq <- function() {
  new_hydrocan_adapter(
    "cehq",
    paste(
      "Centre d'expertise hydrique du Quebec (CEHQ).",
      "Provincial river gauge network for Quebec; natural rivers only,",
      "distinct from the Hydro-Quebec utility adapter.",
      "Daily validated flows and levels; sub-daily data not available.",
      "Full period of record per station fetched and filtered locally."
    ),
    .cehq_list_stations,
    fetch_daily_flows_fn = .cehq_fetch_daily_flows,
    fetch_daily_levels_fn = .cehq_fetch_daily_levels,
    list_stations_meta_fn = .cehq_list_stations_meta
  )
}
