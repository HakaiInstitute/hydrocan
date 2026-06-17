# Constructors attach the class and record which stations were originally
# requested so the print method can report any that came back empty.

new_hydrocan_realtime <- function(x, station_id) {
  structure(
    x,
    class = c("hydrocan_realtime", class(x)),
    requested_stations = station_id
  )
}

new_hydrocan_daily <- function(x, station_id) {
  structure(
    x,
    class = c("hydrocan_daily", class(x)),
    requested_stations = station_id
  )
}

# Shared header printed above the tibble for both classes.
.print_hydrocan_header <- function(x, time_col) {
  n_obs <- nrow(x)
  sources <- unique(x$provider_name)
  params <- unique(x$parameter)
  returned <- unique(x$station_id)
  requested <- attr(x, "requested_stations")
  missing <- setdiff(requested, returned)

  if (n_obs > 0L) {
    t <- x[[time_col]]
    date_rng <- paste(format(min(t)), "to", format(max(t)))
  } else {
    date_rng <- "no data"
  }

  cli::cat_line(cli::cli_fmt({
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
  }))
}

#' @export
print.hydrocan_realtime <- function(x, ...) {
  .print_hydrocan_header(x, "timestamp")
  NextMethod()
  invisible(x)
}

#' @export
print.hydrocan_daily <- function(x, ...) {
  .print_hydrocan_header(x, "date")
  NextMethod()
  invisible(x)
}
