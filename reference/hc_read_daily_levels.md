# Retrieve daily water level summaries

Returns one water level value per station per calendar day as published
by the data source. Not all data sources publish daily level data; those
that do not will produce a warning and return no rows for the affected
stations.

## Usage

``` r
hc_read_daily_levels(
  station_id,
  start_date,
  end_date = Sys.Date(),
  source = NULL
)
```

## Arguments

- station_id:

  Character vector of station identifiers.

- start_date:

  Start of the requested period (Date, or character coercible to Date).

- end_date:

  End of the requested period (Date, or character coercible to Date).
  Defaults to today.

- source:

  Optional single character string naming the data source to use
  directly. When `NULL` (default) the source is detected automatically
  from the station ID. See
  [`hc_list_sources()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_list_sources.md)
  for available names.

## Value

A tibble with columns `station_id` (chr), `date` (Date), `value` (dbl),
`parameter` (chr: `"water_level"`), `unit` (chr), `provider_name` (chr),
`quality_code` (chr), and `qf_desc` (chr).

## Examples

``` r
# \donttest{
try(hc_read_daily_levels("030101", start_date = "2020-01-01", end_date = "2020-01-31"))
#> ── hydrocan ────────────────────────────────────────────────────────────────────
#>   Observations: 0
#>   Sources:
#>   Parameters:
#>   Date range: no data
#>   Stations: 0 returned
#> ! Stations requested but not returned: "030101"
#> ────────────────────────────────────────────────────────────────────────────────
#> # A tibble: 0 × 8
#> # ℹ 8 variables: station_id <chr>, date <date>, value <dbl>, parameter <chr>,
#> #   unit <chr>, provider_name <chr>, quality_code <chr>, qf_desc <chr>
# }
```
