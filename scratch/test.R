install.packages("hydrocan")
devtools::load_all(".")
exists(".yukon_list_stations")
stations <- .yukon_list_stations()
length(stations)
head(stations, 20)


locations_raw <- .yukon_get_locations()
nrow(locations_raw)
length(stations)  # should match nrow(locations_raw) roughly (minus dupes)

# and the full unfiltered picture, if you want to sanity check the network filter:
resp <- httr2::request("https://service.yukon.ca/water-data/api/v2/locations") |>
  httr2::req_url_query(lang = "en", format = "json") |>
  httr2::req_perform()
all_locations <- httr2::resp_body_json(resp, simplifyVector = TRUE)
nrow(all_locations)  # total public locations before the Yukon Small Stream Network filter
class(all_locations$networks)  # the list-column check from before
head(unique(all_locations$networks), 10)

grep(YUKON_NETWORK, all_locations$networks, fixed = TRUE, value = TRUE) |> unique()

starts <- tapply(ts$start_datetime, ts$location_id, min, na.rm = TRUE)
ends <- tapply(ts$end_datetime, ts$location_id, max, na.rm = TRUE)


# Repro: qf_desc silently embeds "NA" text instead of returning a true NA
# Source: R/yukon-wss.R, .yukon_fetch_flows(), qf_desc block (~line 401)
# Confirmed live on station 29CB007, timeseries 179, 2026-07-06

grade_type_description      <- NA_character_
approval_type_description   <- "Not reviewed"
qualifier_type_descriptions <- "Unspecified"

qf_desc <- paste(
  grade_type_description,
  approval_type_description,
  qualifier_type_descriptions,
  sep = "; "
)

qf_desc          # "NA; Not reviewed; Unspecified"
is.na(qf_desc)   # FALSE -- but this row genuinely has no grade info





library(hydrocan)
library(tidyverse)
stn<-hc_list_sources()
hakai<-hc_read_stations("hakai_erddap")

daily_flows <- hc_read_daily_flows("hakai_erdapp") |>
  filter(str_detect(station_name, regex("H08KC703", ignore_case = TRUE)))


library(hydrocan)
library(dplyr)
library(stringr)

library(hydrocan)
library(dplyr)

daily_flows <- hc_read_flows(
  source     = "hakai_erddap",
  station_id = "H08KC0703",
  start_date = as.Date("2020-01-01"),   # adjust to your record start
  end_date   = Sys.Date()
)

library(devtools)
library(dplyr)
library(lubridate)

daily_flows_agg <- daily_flows |>
  mutate(date = as_date(datetime)) |>
  group_by(station_id, date) |>
  summarise(
    mean_flow    = mean(flow, na.rm = TRUE),
    min_flow     = min(flow, na.rm = TRUE),
    max_flow     = max(flow, na.rm = TRUE),
    n_obs        = n(),
    n_valid      = sum(!is.na(flow)),
    pct_complete = round(100 * n_valid / n_obs, 1),
    .groups = "drop"
  )
