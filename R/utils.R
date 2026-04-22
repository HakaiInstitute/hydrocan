# Coerce a value to POSIXct in UTC, accepting character, Date, or POSIXct input.
.to_posixct_utc <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(as.POSIXct(format(x), tz = "UTC"))
  }
  as.POSIXct(as.character(x), tz = "UTC")
}

# Group a real-time tibble by station and calendar date, apply `fun` to value.
# The first observation within each group determines the metadata fields
# (parameter, units, source, approval, quality_flag).
.aggregate_daily <- function(df, fun) {
  df <- dplyr::mutate(df, date = as.Date(.data$datetime))

  dplyr::summarise(
    dplyr::group_by(df, .data$station_number, .data$date, .data$parameter),
    value       = fun(.data$value, na.rm = TRUE),
    units       = dplyr::first(.data$units),
    source      = dplyr::first(.data$source),
    approval    = dplyr::first(.data$approval),
    quality_flag = dplyr::first(.data$quality_flag),
    .groups = "drop"
  ) |>
    dplyr::select(
      "station_number", "date", "value", "parameter",
      "units", "source", "approval", "quality_flag"
    )
}
