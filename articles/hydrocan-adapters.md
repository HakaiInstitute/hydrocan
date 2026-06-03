# The hydrocan adapter system

## Overview

hydrocan normalises data from multiple Canadian hydrometric networks
into one consistent output schema. The mechanism that makes this
possible is the *adapter*: a small object that binds a data source name
to a description and a set of fetch functions.

This vignette explains:

1.  What an adapter is and what it must provide.
2.  How the router uses adapters to dispatch calls.
3.  How the built-in adapters are implemented.
4.  How to write and register your own adapter.

## The adapter contract

An adapter is created with
[`new_hydrocan_adapter()`](https://hakaiinstitute.github.io/hydrocan/reference/new_hydrocan_adapter.md):

``` r

new_hydrocan_adapter(
  name,
  description,
  list_stations_fn,
  fetch_flows_fn = NULL,
  fetch_daily_flows_fn = NULL,
  fetch_levels_fn = NULL,
  fetch_daily_levels_fn = NULL,
  list_stations_meta_fn = NULL,
  license = NULL,
  license_url = NULL,
  terms_url = NULL
)
```

| Argument | Type | Contract |
|----|----|----|
| `name` | single character | Unique identifier; becomes the `provider_name` column in all output and the registry key |
| `description` | single character | Human-readable description of the source and its limitations; shown by [`hc_list_sources()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_list_sources.md) |
| `list_stations_fn` | `function()` | No arguments; returns a character vector of station IDs this adapter can serve |
| `fetch_flows_fn` | `function(station_id, start_date, end_date)` or `NULL` | Returns a tibble matching the realtime schema; `NULL` if sub-daily flow data is not available |
| `fetch_daily_flows_fn` | `function(station_id, start_date, end_date)` or `NULL` | Returns a tibble matching the daily schema; `NULL` if daily flow data is not available |
| `fetch_levels_fn` | `function(station_id, start_date, end_date)` or `NULL` | Returns a tibble matching the realtime schema with `parameter = "water_level"`; `NULL` if sub-daily level data is not available |
| `fetch_daily_levels_fn` | `function(station_id, start_date, end_date)` or `NULL` | Returns a tibble matching the daily schema with `parameter = "water_level"`; `NULL` if daily level data is not available |
| `list_stations_meta_fn` | `function()` or `NULL` | No arguments; returns a tibble matching the stations schema; `NULL` if station metadata is not available |
| `license` | single character or `NULL` | Optional license name (e.g. `"CC-BY 4.0"`); exposed by [`hc_list_sources()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_list_sources.md) |
| `license_url` | single character or `NULL` | Optional URL to the license text |
| `terms_url` | single character or `NULL` | Optional URL to the data provider’s terms of use |

At least one fetch function must be non-`NULL`.

### Output schemas

#### Realtime (sub-daily) - `fetch_flows_fn` / `fetch_levels_fn`

| Column | Type | Notes |
|----|----|----|
| `station_id` | chr | As provided by the caller |
| `timestamp` | POSIXct UTC | Sub-daily observations |
| `value` | dbl |  |
| `parameter` | chr | `"water_discharge"` or `"water_level"` |
| `unit` | chr | Canonical form after normalization (e.g. `"m3/s"`, `"m"`) |
| `provider_name` | chr | Must equal the adapter name |
| `quality_code` | chr | Raw provider quality code; `NA` if unavailable |
| `qf_desc` | chr | Provider description of the quality code; `NA` if unavailable |

#### Daily - `fetch_daily_flows_fn` / `fetch_daily_levels_fn`

Same as the realtime schema above, but with `date` (Date) in place of
`timestamp` (POSIXct).

#### Stations - `list_stations_meta_fn`

| Column          | Type | Notes                                               |
|-----------------|------|-----------------------------------------------------|
| `station_id`    | chr  |                                                     |
| `station_name`  | chr  |                                                     |
| `provider_name` | chr  | Must equal the adapter name                         |
| `longitude`     | dbl  |                                                     |
| `latitude`      | dbl  |                                                     |
| `elevation_m`   | dbl  | `NA` if unavailable                                 |
| `period_start`  | Date | `NA` if unavailable                                 |
| `period_end`    | Date | `NA` if station is still active                     |
| `notes`         | list | Adapter-specific metadata; `NULL` per row if unused |

## How the router works

When you call
[`hc_read_flows()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_read_flows.md),
the router:

1.  Calls `list_stations_fn()` on every registered adapter.
2.  Finds which adapter(s) claim the requested station.
3.  If more than one adapter matches, stops with an error asking you to
    supply `source =` explicitly. Station IDs must be unambiguous across
    the registry.
4.  Calls the appropriate fetch function on the matched adapter, wrapped
    in `tryCatch` so a failure for one station does not abort the whole
    request.
5.  Binds all results with
    [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).

Passing `source = "adaptername"` restricts the router to that adapter,
but it still calls `list_stations_fn()` for that adapter and checks that
the requested station is present before fetching data.

[`hc_list_sources()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_list_sources.md)
returns a tibble of all registered adapters with their descriptions and
a logical column per data type indicating what each adapter supports.
[`hc_read_stations()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_read_stations.md)
queries all adapters for station metadata, skipping those that do not
implement `list_stations_meta_fn`.

## Built-in adapters

### Hydro-Quebec (`hydroquebec`)

The `hydroquebec` adapter wraps the [Hydro-Quebec open data
portal](https://donnees.hydroquebec.com/explore/dataset/donnees-hydrometriques/),
which provides flow measurements at Hydro-Quebec reservoir facilities
via an Opendatasoft REST API. No authentication is required.

**Key characteristics:**

- Station IDs use Hydro-Quebec’s internal format, e.g. `"3-230"`.
- The dataset covers a rolling window of approximately 10 days;
  historical data is not available.
- Only flow data is available (`parameter = "water_discharge"`); no
  water level.
- The `approval` column is `NA` for all records (the source does not
  publish approval status); `quality_flag` carries the source’s point
  type field.

**Station listing and data access:**

``` r

library(hydrocan)

# Sub-daily (hourly) flows
flows <- hc_read_flows(
  station_id = "3-230",
  start_date = Sys.Date() - 5,
  end_date = Sys.Date(),
  source = "hydroquebec"
)

# Source-native daily flows
daily <- hc_read_daily_flows(
  station_id = "3-230",
  start_date = Sys.Date() - 5,
  end_date = Sys.Date(),
  source = "hydroquebec"
)
```

The adapter pages through the API (100 records per request) and filters
the returned records to the requested date range in R, because the API
stores `split_date` as a text field rather than a datetime field.

**Source code:** `R/hydroquebec.R`. Registered via:

``` r

hydrocan_adapter_hydroquebec <- function() {
  new_hydrocan_adapter(
    "hydroquebec",
    paste(
      "Hydro-Quebec open data (Opendatasoft platform).",
      "Flow data only; no water level.",
      "Rolling window of approximately 10 days - historical data is not available."
    ),
    .hq_list_stations,
    fetch_flows_fn       = .hq_fetch_flows,
    fetch_daily_flows_fn = .hq_fetch_daily_flows,
    list_stations_meta_fn = .hq_list_stations_meta
  )
}
```

### Registration

Adapters are registered at load time in `R/hydrocan-package.R`. Use
[`hc_list_sources()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_list_sources.md)
to see all currently registered sources and which data types each
supports.

## Writing a new adapter

Suppose you want to add a hypothetical provincial network called
“MyProv” that exposes a JSON API. The steps are:

### Step 1 - Implement the internal functions

Create `R/myprov.R`:

``` r

.MYPROV_URL <- "https://data.myprov.ca/api/hydro"

.myprov_list_stations <- function() {
  resp <- httr2::request(.MYPROV_URL) |>
    httr2::req_url_query(endpoint = "stations", format = "json") |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  resp$station_id  # character vector
}

.myprov_fetch_flows <- function(station_id, start_date, end_date) {
  resp <- httr2::request(.MYPROV_URL) |>
    httr2::req_url_query(
      endpoint = "timeseries",
      station  = station_id,
      from     = format(start_date),
      to       = format(end_date),
      format   = "json"
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  tibble::tibble(
    station_id    = station_id,
    timestamp     = as.POSIXct(resp$timestamp, tz = "UTC"),
    value         = as.numeric(resp$discharge_cms),
    parameter     = "water_discharge",
    unit          = "m3/s",
    provider_name = "myprov",
    quality_code  = resp$quality_code,
    qf_desc       = NA_character_
  )
}

hydrocan_adapter_myprov <- function() {
  new_hydrocan_adapter(
    "myprov",
    "MyProv provincial hydrometric network. Sub-daily flows only.",
    .myprov_list_stations,
    fetch_flows_fn = .myprov_fetch_flows
  )
}
```

If your source also provides daily data, levels, or station metadata,
supply the corresponding optional function arguments. Only the
capabilities you implement will be advertised by
[`hc_list_sources()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_list_sources.md).

#### Using a stored station list when no endpoint exists

Some sources do not expose a station-listing endpoint. In those cases,
bundle a character vector of known station IDs directly in the package
and return it from `list_stations_fn`:

``` r

.MYPROV_STATIONS <- c("MP001", "MP002", "MP003")

.myprov_list_stations <- function() .MYPROV_STATIONS
```

The tradeoff is that the list must be maintained manually as the network
changes. The router only requires that `list_stations_fn()` return a
character vector; how that vector is produced is left entirely to the
adapter.

### Step 2 - Register the adapter

Add one line to the `.onLoad` block in `R/hydrocan-package.R`:

``` r

.onLoad <- function(libname, pkgname) {
  register_hydrocan_adapter(hydrocan_adapter_hydroquebec())
  register_hydrocan_adapter(hydrocan_adapter_cehq())
  register_hydrocan_adapter(hydrocan_adapter_myprov())   # add this
}
```

### Step 3 - Add tests

Tests for adapters are written against a mock adapter rather than
hitting the live network. This keeps the test suite fast and fully
offline. The pattern, established in `tests/testthat/helper-mocks.R`,
is:

1.  Write a `list_stations_fn` that returns a hardcoded character
    vector.
2.  Write fetch functions that generate deterministic tibbles from their
    date arguments without making any HTTP requests.
3.  Assemble these into an adapter with
    [`new_hydrocan_adapter()`](https://hakaiinstitute.github.io/hydrocan/reference/new_hydrocan_adapter.md).
4.  Register it for the duration of a single test with
    `local_register_adapter()`, which restores the prior registry state
    on exit.

``` r

.myprov_stations <- c("MP001", "MP002")

.myprov_mock_fetch_flows <- function(station_id, start_date, end_date) {
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  tibble::tibble(
    station_id    = station_id,
    timestamp     = as.POSIXct(dates, tz = "UTC"),
    value         = seq_along(dates) * 1.0,
    parameter     = "water_discharge",
    unit          = "m3/s",
    provider_name = "myprov",
    quality_code  = NA_character_,
    qf_desc       = NA_character_
  )
}

mock_myprov_adapter <- new_hydrocan_adapter(
  "myprov",
  "Mock MyProv adapter for offline testing.",
  function() .myprov_stations,
  fetch_flows_fn = .myprov_mock_fetch_flows
)

test_that("myprov adapter returns correct schema", {
  local_register_adapter(mock_myprov_adapter)
  result <- hc_read_flows(
    station_id = "MP001",
    start_date = "2024-01-01",
    end_date   = "2024-01-03",
    source     = "myprov"
  )
  expect_s3_class(result, "hydrocan_realtime")
  expect_equal(nrow(result), 3L)
})
```

`local_register_adapter()` and `local_clear_registry()` are defined in
`tests/testthat/helper-mocks.R` and are available to all test files
automatically.

### What the schema validator will catch

`validate_hydrocan_schema()` is called automatically after every
data-fetching API call
([`hc_read_flows()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_read_flows.md),
[`hc_read_daily_flows()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_read_daily_flows.md),
[`hc_read_levels()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_read_levels.md),
[`hc_read_daily_levels()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_read_daily_levels.md)).
It will stop with a clear message if:

- Any required column is missing from the returned tibble.

It also normalises the `unit` column: common variants such as `"m³/s"`,
`"cms"`, or `"m^3/s"` are all mapped to the canonical `"m3/s"`.
Unrecognised unit strings pass through unchanged with a warning,
identifying the raw string so it can be added to the mapping table in
`R/schema.R`.
