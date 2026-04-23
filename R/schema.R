# Column sets and valid vocabulary for the hydrocan standard output schema.
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

# Canonical unit strings used across all adapters. The names are the raw
# strings an adapter might return; the values are the hydrocan standard forms.
# Add new entries here as additional data sources are integrated.
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

# Map raw unit strings from adapter output to canonical hydrocan unit strings.
# Unknown units are passed through unchanged with a warning so that new
# sources surface their unit strings rather than silently producing bad output.
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

# Validate and normalize a tibble returned by an adapter. Stops on the first
# structural violation; normalizes the units column for flow/level schemas.
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
      "Adapter output is missing required columns: ",
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
