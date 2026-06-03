# Retrieve station metadata

Returns location and period-of-record information for all stations
available across registered data sources. Data sources that do not
publish station metadata are skipped with a warning.

## Usage

``` r
hc_read_stations(source = NULL)
```

## Arguments

- source:

  Optional single character string naming the data source to query
  directly. When `NULL` (default) all registered data sources are
  queried. See
  [`hc_list_sources()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_list_sources.md)
  for available names.

## Value

A tibble with columns `station_id` (chr), `station_name` (chr),
`provider_name` (chr), `longitude` (dbl), `latitude` (dbl),
`elevation_m` (dbl), `period_start` (Date), `period_end` (Date), and
`notes` (list).

## Examples

``` r
if (FALSE) { # \dontrun{
try(hc_read_stations())
try(hc_read_stations(source = "cehq"))
} # }
```
