# Hakai ERDDAP station table
#
# Fetches station metadata and period of record from the Hakai ERDDAP server
# and bundles the result as internal package data. Station locations and
# identifiers change rarely; re-run this script and commit the updated
# R/sysdata.rda when stations are added or modified.

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

usethis::use_data(hakai_erddap_stations, internal = TRUE, overwrite = TRUE)
