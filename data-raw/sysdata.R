# Internal package data
#
# Builds all objects stored in R/sysdata.rda. Re-run this script and commit the
# updated R/sysdata.rda whenever source data changes.

# CEHQ remark code lookup table ----------------------------------------------
#
# Downloads a real CEHQ historical data file and parses the Lexique/Remarque
# section from the file header, where CEHQ documents all remark codes inline.
# Using a real file as the source ensures the descriptions stay authoritative.

raw <- readLines(
  "https://www.cehq.gouv.qc.ca/depot/historique_donnees/fichier/030101_Q.txt",
  encoding = "latin1",
  warn = FALSE
)

# The Lexique block runs from the "Lexique:" line to just before the data.
lexique_start <- grep("^Lexique", raw)
data_start <- grep("^[0-9]{6}[[:space:]]", raw)[1L]
lexique_lines <- raw[lexique_start:(data_start - 1L)]

# Strip the line-prefix labels ("Lexique:" and "(Remarque)") so the code
# entries that follow them are left-aligned for consistent parsing.
lexique_lines <- gsub("^(Lexique:|\\(Remarque\\))", "", lexique_lines)

# Keep only lines that contain a code entry.
entry_lines <- grep(
  "^[[:space:]]*[A-Z][A-Z0-9*]*[[:space:]]*:",
  lexique_lines,
  value = TRUE
)

# Split compound lines: "P:  desc1; P* : desc2" -> two separate entries.
entries <- trimws(unlist(strsplit(entry_lines, ";")))

# Extract "CODE : description" from each entry.
parsed <- regmatches(
  entries,
  regexpr("[A-Z][A-Z0-9*]*[[:space:]]*:[[:space:]]+.+", entries)
)

codes <- trimws(sub("([A-Z][A-Z0-9*]*)[[:space:]]*:.*", "\\1", parsed))
descs <- trimws(sub("[A-Z][A-Z0-9*]*[[:space:]]*:[[:space:]]+", "", parsed))

cehq_remark_codes <- data.frame(
  quality_code = codes,
  qf_desc = descs,
  stringsAsFactors = FALSE
)

print(cehq_remark_codes)

# Hakai ERDDAP station table -------------------------------------------------
#
# Fetches station metadata and period of record from the Hakai ERDDAP server.
# Station locations and identifiers change rarely; re-run this script and
# commit the updated R/sysdata.rda when stations are added or modified.

base_url <- "https://catalogue.hakai.org/erddap"
dataset <- "HakaiWatershedsStreamStationsProvisional"

erddap_csv <- function(fields, constraints = character(0)) {
  query <- paste(c(paste(fields, collapse = ","), constraints), collapse = "&")
  query <- gsub('"', "%22", query, fixed = TRUE)
  url <- paste0(base_url, "/tabledap/", dataset, ".csv?", query)
  df <- utils::read.csv(url, colClasses = "character", check.names = FALSE)
  df[-1L, , drop = FALSE]
}

df <- erddap_csv(
  c("station_id", "station_description", "latitude", "longitude", "elevation"),
  "distinct()"
)

df_time <- erddap_csv(
  c("station_id", "time"),
  'orderByMinMax("station_id,time")'
)

period_start <- as.Date(as.POSIXct(
  tapply(df_time$time, df_time$station_id, min)[df$station_id],
  format = "%Y-%m-%dT%H:%M:%SZ",
  tz = "UTC"
))
period_end <- as.Date(as.POSIXct(
  tapply(df_time$time, df_time$station_id, max)[df$station_id],
  format = "%Y-%m-%dT%H:%M:%SZ",
  tz = "UTC"
))

hakai_erddap_stations <- tibble::tibble(
  station_id = df$station_id,
  station_name = df$station_description,
  provider_name = "hakai_erddap",
  longitude = suppressWarnings(as.double(df$longitude)),
  latitude = suppressWarnings(as.double(df$latitude)),
  elevation_m = suppressWarnings(as.double(df$elevation)),
  period_start = period_start,
  period_end = period_end,
  notes = vector("list", nrow(df))
)

print(hakai_erddap_stations)

# BC Aquarius station table --------------------------------------------------
#
# Builds the BC Government Aquarius station list from the portal's Data_List
# JSON endpoint, the same source the List view's grid is built on. The portal
# gates content behind a disclaimer that sets a session cookie, so the flow is:
# accept the disclaimer, scrape the Discharge/Stage parameter ids from the List
# page dropdown, then page through Data_List for each parameter. Stations are
# the union of those serving Discharge or Stage, matching what the adapter can
# read. Re-run this script and commit R/sysdata.rda when the network changes.

bc_aquarius_base <- "https://bcmoe-prod.aquaticinformatics.net"
bc_aquarius_cookies <- tempfile()

# Accept the disclaimer to obtain a session cookie. The form carries an
# anti-forgery token that must be echoed back with the POST.
bc_disclaimer <- httr2::request(
  paste0(bc_aquarius_base, "/Disclaimer?returnUrl=%2FData%2FList%2F")
) |>
  httr2::req_cookie_preserve(bc_aquarius_cookies) |>
  httr2::req_perform()

bc_token <- httr2::resp_body_html(bc_disclaimer) |>
  rvest::html_element("input[name='__RequestVerificationToken']") |>
  rvest::html_attr("value")

httr2::request(paste0(bc_aquarius_base, "/AcceptDisclaimer")) |>
  httr2::req_cookie_preserve(bc_aquarius_cookies) |>
  httr2::req_body_form(
    returnUrl = "/Data/List/",
    `__RequestVerificationToken` = bc_token
  ) |>
  httr2::req_perform()

# Parameter ids are not fixed across Aquarius deployments; read them from the
# List page dropdown, where each option carries a data-code attribute.
bc_list_page <- httr2::request(paste0(bc_aquarius_base, "/Data/List/")) |>
  httr2::req_cookie_preserve(bc_aquarius_cookies) |>
  httr2::req_perform() |>
  httr2::resp_body_html()

bc_param_options <- rvest::html_elements(bc_list_page, "option[data-code]")
bc_param_ids <- stats::setNames(
  rvest::html_attr(bc_param_options, "value"),
  rvest::html_attr(bc_param_options, "data-code")
)
bc_param_ids <- bc_param_ids[!duplicated(names(bc_param_ids))]

# Page through Data_List for a parameter id and return all dataset rows. The
# params must be sent as a form-encoded body; a query string yields an empty
# response. The endpoint omits its content-type header, so parse unchecked.
bc_fetch_datasets <- function(param_id, page_size = 5000L) {
  rows <- list()
  page <- 1L
  repeat {
    payload <- httr2::request(paste0(bc_aquarius_base, "/Data/Data_List")) |>
      httr2::req_cookie_preserve(bc_aquarius_cookies) |>
      httr2::req_body_form(
        page = page,
        pageSize = page_size,
        `parameters[0]` = param_id
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = TRUE, check_type = FALSE)

    rows[[page]] <- payload$Data
    fetched <- sum(vapply(rows, NROW, integer(1)))
    if (fetched >= payload$Total || NROW(payload$Data) == 0L) {
      break
    }
    page <- page + 1L
  }
  dplyr::bind_rows(rows)
}

bc_discharge <- bc_fetch_datasets(bc_param_ids[["Discharge"]])
bc_stage <- bc_fetch_datasets(bc_param_ids[["Stage"]])

bc_datasets <- dplyr::bind_rows(bc_discharge, bc_stage)

# The adapter fetches the "Working" series for each parameter
# ({parameter}.Working@{id}). Only stations exposing a Working discharge or
# stage dataset are actually retrievable, so restrict the station list to
# those. Other dataset labels (Field Visits, Logger, Telemetry, ...) catalogue
# data the adapter cannot request and would otherwise list stations that
# return no rows. A station is kept if it has a Working discharge or stage
# series; the parameter prefix records which are available.
bc_datasets <- bc_datasets |>
  dplyr::filter(
    grepl("^(Discharge|Stage)\\.Working@", DatasetIdentifier)
  ) |>
  dplyr::mutate(
    parameter = sub("\\..*", "", DatasetIdentifier)
  )

# Collapse datasets to one row per station, taking the widest period of record
# across that station's Working datasets. StartOfRecord/EndOfRecord are ISO
# timestamps with optional fractional seconds; parse the date portion only.
bc_stations_meta <- bc_datasets |>
  dplyr::mutate(
    period_start = as.Date(substr(StartOfRecord, 1L, 10L)),
    period_end = as.Date(substr(EndOfRecord, 1L, 10L))
  ) |>
  dplyr::group_by(LocationIdentifier) |>
  dplyr::summarise(
    station_name = dplyr::first(Location),
    longitude = dplyr::first(LocX),
    latitude = dplyr::first(LocY),
    parameters = list(sort(unique(parameter))),
    period_start = suppressWarnings(min(period_start, na.rm = TRUE)),
    period_end = suppressWarnings(max(period_end, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    period_start = dplyr::if_else(
      is.finite(period_start),
      period_start,
      as.Date(NA)
    ),
    period_end = dplyr::if_else(is.finite(period_end), period_end, as.Date(NA))
  )

# Hakai-operated stations also published to the BC Aquarius portal. Dropped
# here to not duplicate what is already in hakai_erddap, which
# carries the QC'd record. Keep this list in sync if Hakai stations are
# added to or removed from the BC portal.
bc_hakai_duplicates <- c(
  "H08KC0626",
  "H08KC0693",
  "H08KC0703",
  "H08KC0708",
  "H08KC0844")

bc_stations_meta <- bc_stations_meta |>
  dplyr::filter(!LocationIdentifier %in% bc_hakai_duplicates)

bc_aquarius_stations <- tibble::tibble(
  station_id = bc_stations_meta$LocationIdentifier,
  station_name = bc_stations_meta$station_name,
  provider_name = "bc_aquarius",
  longitude = as.double(bc_stations_meta$longitude),
  latitude = as.double(bc_stations_meta$latitude),
  elevation_m = NA_real_,
  period_start = bc_stations_meta$period_start,
  period_end = bc_stations_meta$period_end,
  # notes is a list column to align with hakai_erddap_stations so the two
  # combine cleanly in hc_read_stations(); it records which Working parameters
  # (Discharge, Stage) the station exposes.
  notes = lapply(
    bc_stations_meta$parameters,
    function(p) c(parameters = paste(p, collapse = ", "))
  )
)

print(bc_aquarius_stations)

# Save all internal datasets together ----------------------------------------
usethis::use_data(
  cehq_remark_codes,
  hakai_erddap_stations,
  bc_aquarius_stations,
  internal = TRUE,
  overwrite = TRUE
)
