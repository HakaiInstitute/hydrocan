# Column sets and valid vocabulary for the hydrocan standard output schema.
.REALTIME_COLS <- c(
  "station_number", "datetime", "value", "parameter",
  "units", "source", "approval", "quality_flag"
)

.DAILY_COLS <- c(
  "station_number", "date", "value", "parameter",
  "units", "source", "approval", "quality_flag"
)

.VALID_APPROVAL <- c("provisional", "approved", "estimated")

# Checks that a tibble returned by an adapter matches the hydrocan schema.
# Stops with an informative message on the first violation found.
validate_hydrocan_schema <- function(df, type = c("realtime", "daily")) {
  type <- match.arg(type)
  required <- if (type == "realtime") .REALTIME_COLS else .DAILY_COLS

  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0L) {
    stop(
      "Adapter output is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

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

  invisible(df)
}
