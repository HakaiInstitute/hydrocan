# Retrieve sub-daily water level observations

Fetches sub-daily water level observations for one or more stations
across the requested date range. The data source is determined
automatically from the station number, or fixed explicitly via `source`.

## Usage

``` r
hc_read_levels(
  station_number,
  start_date,
  end_date = Sys.Date(),
  source = NULL
)
```

## Arguments

- station_number:

  Character vector of station identifiers.

- start_date:

  Start of the requested period (Date, or character coercible to Date).

- end_date:

  End of the requested period (Date, or character coercible to Date).
  Defaults to today.

- source:

  Optional single character string naming the data source to use
  directly. When `NULL` (default) the source is detected automatically
  from the station number. See
  [`hc_list_sources()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_list_sources.md)
  for available names.

## Value

A tibble with columns `station_number` (chr), `datetime` (POSIXct UTC),
`value` (dbl), `parameter` (chr: `"level"`), `units` (chr), `source`
(chr), `approval` (chr), and `quality_flag` (chr).
