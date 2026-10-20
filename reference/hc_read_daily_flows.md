# Retrieve daily flow summaries

Returns one value per station per calendar day as published by the data
source. Not all data sources publish daily data; those that do not will
produce a warning and return no rows for the affected stations.

## Usage

``` r
hc_read_daily_flows(
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
`parameter` (chr), `unit` (chr), `provider_name` (chr), `quality_code`
(chr), and `qf_desc` (chr).

## Examples

``` r
# \donttest{
try(hc_read_daily_flows("030101", start_date = "2020-01-01", end_date = "2020-01-31"))
#> ── hydrocan ────────────────────────────────────────────────────────────────────
#>   Observations: 31
#>   Source: cehq
#>   Parameter: water_discharge
#>   Date range: 2020-01-01 to 2020-01-31
#>   Station: 1 returned
#> ✔ All stations returned.
#> ────────────────────────────────────────────────────────────────────────────────
#> # A tibble: 31 × 8
#>    station_id date       value parameter       unit  provider_name quality_code
#>  * <chr>      <date>     <dbl> <chr>           <chr> <chr>         <chr>       
#>  1 030101     2020-01-01  4.13 water_discharge m3/s  cehq          R           
#>  2 030101     2020-01-02  3.84 water_discharge m3/s  cehq          R           
#>  3 030101     2020-01-03  3.7  water_discharge m3/s  cehq          R           
#>  4 030101     2020-01-04  3.53 water_discharge m3/s  cehq          R           
#>  5 030101     2020-01-05  3.91 water_discharge m3/s  cehq          R           
#>  6 030101     2020-01-06  5.41 water_discharge m3/s  cehq          R           
#>  7 030101     2020-01-07  5.17 water_discharge m3/s  cehq          R           
#>  8 030101     2020-01-08  4.95 water_discharge m3/s  cehq          J           
#>  9 030101     2020-01-09  4.75 water_discharge m3/s  cehq          R           
#> 10 030101     2020-01-10  4.59 water_discharge m3/s  cehq          R           
#> # ℹ 21 more rows
#> # ℹ 1 more variable: qf_desc <chr>
# }
```
