# Resolve which adapter(s) can serve each requested station, fetch data, and
# combine results. This is the single point of dispatch for all user-facing
# API functions.
#
# type = "flows" dispatches to fetch_flows_fn (returns datetime column)
# type = "daily"    dispatches to fetch_daily_flows_fn (returns date column)
.route_and_fetch <- function(
  station_number,
  start_date,
  end_date,
  source = NULL,
  type = c("flows", "daily")
) {
  type <- match.arg(type)
  fetch_fn_field <- if (type == "flows") {
    "fetch_flows_fn"
  } else {
    "fetch_daily_flows_fn"
  }
  empty_fn <- if (type == "flows") {
    .empty_flows_tibble
  } else {
    .empty_daily_flows_tibble
  }

  # Build the candidate adapter list.
  if (!is.null(source)) {
    adapter <- get0(source, envir = .hydrocan_registry)
    if (is.null(adapter)) {
      stop("No data source registered with name '", source, "'.", call. = FALSE)
    }
    adapters <- stats::setNames(list(adapter), source)
  } else {
    adapters <- as.list(.hydrocan_registry)
  }

  if (length(adapters) == 0L) {
    stop(
      "No data sources are registered. Has the package loaded correctly?",
      call. = FALSE
    )
  }

  # Fetch each adapter's station list once, outside the per-station loop, so
  # adapters with live station endpoints are not called repeatedly.
  station_lists <- lapply(adapters, \(a) a$list_stations_fn())

  # For each requested station, find its adapter and fetch data.
  results <- lapply(station_number, function(stn) {
    matching <- adapters[vapply(
      station_lists,
      \(stns) stn %in% stns,
      logical(1L)
    )]

    if (length(matching) == 0L) {
      warning(
        "Station '",
        stn,
        "' not found in any data source. Skipping.",
        call. = FALSE
      )
      return(NULL)
    }

    if (length(matching) > 1L) {
      adapter_names <- vapply(matching, `[[`, character(1L), "name")
      stop(
        "Station '",
        stn,
        "' exists in multiple data sources: ",
        paste(adapter_names, collapse = ", "),
        ". Use the `source` argument to specify which one.",
        call. = FALSE
      )
    }

    fetch_fn <- matching[[1L]][[fetch_fn_field]]
    if (is.null(fetch_fn)) {
      warning(
        "Data source '",
        matching[[1L]]$name,
        "' does not support ",
        type,
        " data for station '",
        stn,
        "'. Skipping.",
        call. = FALSE
      )
      return(NULL)
    }

    tryCatch(
      fetch_fn(stn, start_date, end_date),
      error = function(e) {
        warning(
          "Failed to fetch '",
          stn,
          "' from '",
          matching[[1L]]$name,
          "': ",
          conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )
  })

  results <- Filter(Negate(is.null), results)
  if (length(results) == 0L) {
    return(empty_fn())
  }
  dplyr::bind_rows(results)
}
