# R/get_hakai_flow_data.R
#
# Ian wrote the code, then had Claude clean it up.
#
# Pulls 5-minute discharge data for the specified Hakai gauges from Hakai's
# ERDDAP (via hydrocan), filters out unreliable readings, and aggregates to
# daily mean flow in a format matching the WSC data frame (STATION_NUMBER,
# Date, Value).
#
# Expects the following objects to exist in the calling environment before
# sourcing:
#   SiteList_hydrocan  - character vector of Hakai station IDs to pull
#                        (controls which gauges are included)
#   HAKAI_START_DATE    - start date for the pull (as.Date or string). Use a
#                         deliberately early date (e.g. "1900-01-01") to
#                         pull the full available record.
#   MIN_OBS_PER_DAY     - minimum number of 5-min readings required to keep
#                         a day (out of a max of 288/day). Days below this
#                         threshold are dropped as unreliable daily means.
#
# Produces: HI_Q_data (data frame: STATION_NUMBER, Date, Value)

# --- 1. Pull 5-min data ---
HI_Q_subdaily_data <- hc_read_flows(
  station_id = SiteList_hydrocan,
  start_date = HAKAI_START_DATE,
  end_date   = Sys.Date()
)

# --- 2. Quality filtering ---
# quality_code combines a code and a description (e.g. "SVC: ..."); extract
# just the code for filtering.
HI_Q_subdaily_data$QC_code <- sub(":.*", "", HI_Q_subdaily_data$quality_code)

# Null out SVC-flagged (suspect) values so they don't get used yet the time series stays complete.
HI_Q_subdaily_data$value[HI_Q_subdaily_data$QC_code == "SVC"] <- NA

# --- 3. Aggregate to daily mean flow ---
HI_Q_subdaily_data$Date <- date(HI_Q_subdaily_data$timestamp)

HI_Q_data <- HI_Q_subdaily_data |>
  group_by(station_id, Date) |>
  summarise(
    Value = round(mean(value, na.rm = TRUE), 4),
    n_obs_per_day = sum(!is.na(value)),
    .groups = "drop"
  )

# --- 4. Drop days with insufficient 5-min data ---
# Max possible readings/day = 288 (5-min intervals). MIN_OBS_PER_DAY is set
# by the caller (see qmd) so this threshold can be tuned without editing
# this script.
HI_Q_data <- subset(HI_Q_data, n_obs_per_day >= MIN_OBS_PER_DAY)

# --- 5. Match WSC data frame schema ---
HI_Q_data$STATION_NUMBER <- HI_Q_data$station_id
HI_Q_data <- HI_Q_data[c("STATION_NUMBER", "Date", "Value")]