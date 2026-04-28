# Retrieve daily flow summaries

Returns one value per station per calendar day as published by the data
source. Not all data sources publish daily data; those that do not will
produce a warning and return no rows for the affected stations.

## Usage

``` r
hc_read_daily_flows(
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

A tibble with columns `station_number` (chr), `date` (Date), `value`
(dbl), `parameter` (chr), `units` (chr), `source` (chr), `approval`
(chr), and `quality_flag` (chr).
