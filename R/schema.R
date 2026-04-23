# Required columns for each output schema.
.FLOWS_COLS <- c(
  "station_number",
  "datetime",
  "value",
  "parameter",
  "units",
  "source",
  "approval",
  "quality_flag"
)
.DAILY_FLOWS_COLS <- c(
  "station_number",
  "date",
  "value",
  "parameter",
  "units",
  "source",
  "approval",
  "quality_flag"
)
.STATIONS_COLS <- c(
  "station_number",
  "station_name",
  "source",
  "longitude",
  "latitude",
  "elevation_m",
  "period_start",
  "period_end",
  "notes"
)

.VALID_APPROVAL <- c("provisional", "approved", "estimated")

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
  type = c("flows", "daily", "stations")
) {
  type <- match.arg(type)

  required <- switch(
    type,
    flows = .FLOWS_COLS,
    daily = .DAILY_FLOWS_COLS,
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
    bad <- !is.na(df$approval) & !(df$approval %in% .VALID_APPROVAL)
    if (any(bad)) {
      stop(
        "Column 'approval' contains invalid values: ",
        paste(unique(df$approval[bad]), collapse = ", "),
        ". Must be one of: ",
        paste(.VALID_APPROVAL, collapse = ", "),
        call. = FALSE
      )
    }
    df$units <- .normalize_units(df$units)
  }

  df
}
