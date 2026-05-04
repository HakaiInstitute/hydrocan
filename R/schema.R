# Required columns for each output schema.
.REALTIME_COLS <- c(
  "station_id",
  "timestamp",
  "value",
  "parameter",
  "unit",
  "provider_name",
  "quality_code",
  "qf_desc"
)
.DAILY_COLS <- c(
  "station_id",
  "date",
  "value",
  "parameter",
  "unit",
  "provider_name",
  "quality_code",
  "qf_desc"
)
.STATIONS_COLS <- c(
  "station_id",
  "station_name",
  "provider_name",
  "longitude",
  "latitude",
  "elevation_m",
  "period_start",
  "period_end",
  "notes"
)

# Maps raw unit strings from any data source to canonical hydrocan forms.
# Add entries here as new sources are integrated.
.UNIT_MAP <- c(
  # --- Flow ---
  "m3/s" = "m3/s",
  "m\u00b3/s" = "m3/s", # m³/s - Unicode superscript from some APIs
  "cms" = "m3/s",
  "m^3/s" = "m3/s",
  "m3/sec" = "m3/s",
  "ft3/s" = "ft3/s",
  "cfs" = "ft3/s",
  "ft3/sec" = "ft3/s",
  # --- Level ---
  "m" = "m",
  "metres" = "m",
  "meters" = "m",
  "ft" = "ft",
  "feet" = "ft"
)

# Unknown units pass through unchanged with a warning so new sources surface
# their raw strings rather than silently producing incorrect output.
.normalize_units <- function(units) {
  normalized <- .UNIT_MAP[units]
  unknown <- is.na(normalized) & !is.na(units)
  if (any(unknown)) {
    warning(
      "Unrecognized unit(s) passed through unchanged: ",
      paste(unique(units[unknown]), collapse = ", "),
      ". Add them to .UNIT_MAP in R/schema.R.",
      call. = FALSE
    )
    normalized[unknown] <- units[unknown]
  }
  unname(normalized)
}

# Validate required columns and normalize units. Stops on the first structural
# violation; unit normalization is skipped for the stations schema.
validate_hydrocan_schema <- function(
  df,
  type = c("realtime", "daily", "stations")
) {
  type <- match.arg(type)

  required <- switch(
    type,
    realtime = .REALTIME_COLS,
    daily = .DAILY_COLS,
    stations = .STATIONS_COLS
  )

  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0L) {
    stop(
      "Data source output is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (type != "stations") {
    df$unit <- .normalize_units(df$unit)
  }

  df
}
