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

# Save all internal datasets together ----------------------------------------
usethis::use_data(cehq_remark_codes, hakai_erddap_stations, internal = TRUE, overwrite = TRUE)
