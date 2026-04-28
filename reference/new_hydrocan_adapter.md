# Create a hydrocan adapter

Constructs a validated adapter object for a data source. At least one of
`fetch_flows_fn` or `fetch_daily_flows_fn` must be supplied.

## Usage

``` r
new_hydrocan_adapter(
  name,
  description,
  list_stations_fn,
  fetch_flows_fn = NULL,
  fetch_daily_flows_fn = NULL,
  list_stations_meta_fn = NULL
)
```

## Arguments

- name:

  Non-empty string identifying this source. Used as the registry key and
  as the `source` column in output.

- description:

  String describing the source and any known limitations (e.g. rolling
  data window). Shown by
  [`hc_list_sources()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_list_sources.md).

- list_stations_fn:

  Function with no arguments returning a character vector of station IDs
  this source can serve.

- fetch_flows_fn:

  Optional `function(station_number, start_date, end_date)` returning a
  tibble matching the flows schema (`datetime` column). `NULL` if
  sub-daily data is not available.

- fetch_daily_flows_fn:

  Optional `function(station_number, start_date, end_date)` returning a
  tibble matching the daily flows schema (`date` column). `NULL` if
  daily data is not available.

- list_stations_meta_fn:

  Optional function with no arguments returning a tibble matching the
  stations schema. `NULL` if station metadata is not available.

## Value

A list with class `"hydrocan_adapter"`.
