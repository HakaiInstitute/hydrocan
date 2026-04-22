# Resolve which adapter(s) can serve each requested station, fetch data, and
# combine results. This is the single point of dispatch for all user-facing
# API functions.
.route_and_fetch <- function(station_number, start_date, end_date, source = NULL) {
  # Build the candidate adapter list.
  if (!is.null(source)) {
    adapter <- get0(source, envir = .hydrocan_registry)
    if (is.null(adapter)) {
      stop("No adapter registered with name '", source, "'.", call. = FALSE)
    }
    adapters <- stats::setNames(list(adapter), source)
  } else {
    adapters <- as.list(.hydrocan_registry)
  }

  if (length(adapters) == 0L) {
    stop(
      "No adapters are registered. Has the package loaded correctly?",
      call. = FALSE
    )
  }

  # For each requested station, find its adapter and fetch data.
  results <- lapply(station_number, function(stn) {
    matching <- Filter(
      function(a) stn %in% a$list_stations_fn(),
      adapters
    )

    if (length(matching) == 0L) {
      warning("Station '", stn, "' not found in any adapter. Skipping.",
              call. = FALSE)
      return(NULL)
    }

    if (length(matching) > 1L) {
      adapter_names <- vapply(matching, `[[`, character(1L), "name")
      warning(
        "Station '", stn, "' found in multiple adapters (",
        paste(adapter_names, collapse = ", "),
        "). Using '", adapter_names[[1L]], "'.",
        call. = FALSE
      )
      matching <- matching[1L]
    }

    tryCatch(
      matching[[1L]]$fetch_flows_fn(stn, start_date, end_date),
      error = function(e) {
        warning(
          "Failed to fetch '", stn, "' from adapter '",
          matching[[1L]]$name, "': ", conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )
  })

  results <- Filter(Negate(is.null), results)

  if (length(results) == 0L) {
    return(.empty_realtime_tibble())
  }

  dplyr::bind_rows(results)
}

.empty_realtime_tibble <- function() {
  tibble::tibble(
    station_number = character(),
    datetime       = as.POSIXct(character(), tz = "UTC"),
    value          = numeric(),
    parameter      = character(),
    units          = character(),
    source         = character(),
    approval       = character(),
    quality_flag   = character()
  )
}
