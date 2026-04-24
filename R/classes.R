# Constructors attach the class and record which stations were originally
# requested so the print method can report any that came back empty.

new_hydrocan_flows <- function(x, station_number) {
  structure(
    x,
    class = c("hydrocan_flows", class(x)),
    requested_stations = station_number
  )
}

new_hydrocan_daily_flows <- function(x, station_number) {
  structure(
    x,
    class = c("hydrocan_daily_flows", class(x)),
    requested_stations = station_number
  )
}

# Shared header printed above the tibble for both flow classes.
.print_hydrocan_header <- function(x, time_col) {
  n_obs <- nrow(x)
  sources <- unique(x$source)
  params <- unique(x$parameter)
  returned <- unique(x$station_number)
  requested <- attr(x, "requested_stations")
  missing <- setdiff(requested, returned)

  if (n_obs > 0L) {
    t <- x[[time_col]]
    date_rng <- paste(format(min(t)), "to", format(max(t)))
  } else {
    date_rng <- "no data"
  }

  cli::cli_rule(left = "hydrocan")
  cli::cli_bullets(c(
    " " = "Observations:   {format(n_obs, big.mark = ',')}",
    " " = "{cli::qty(length(sources))}Source{?s}:      {paste(sources, collapse = ', ')}",
    " " = "{cli::qty(length(params))}Parameter{?s}:   {paste(params, collapse = ', ')}",
    " " = "Date range:     {date_rng}",
    " " = "{cli::qty(length(returned))}Station{?s}:     {length(returned)} returned"
  ))
  if (length(missing) > 0L) {
    cli::cli_alert_warning(
      "Stations requested but not returned: {.val {missing}}"
    )
  } else {
    cli::cli_alert_success("All stations returned.")
  }
  cli::cli_rule()
}

#' @export
print.hydrocan_flows <- function(x, ...) {
  .print_hydrocan_header(x, "datetime")
  NextMethod()
  invisible(x)
}

#' @export
print.hydrocan_daily_flows <- function(x, ...) {
  .print_hydrocan_header(x, "date")
  NextMethod()
  invisible(x)
}
