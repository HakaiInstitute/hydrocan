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
# \donttest{
try(hc_read_stations())
#> # A tibble: 446 × 9
#>    station_id station_name          provider_name longitude latitude elevation_m
#>    <chr>      <chr>                 <chr>             <dbl>    <dbl>       <dbl>
#>  1 3-100      Outardes-4            hydroquebec       -68.9     49.7          NA
#>  2 3-101      Outardes-3            hydroquebec       -68.7     49.6          NA
#>  3 3-102      Outardes-2            hydroquebec       -68.4     49.1          NA
#>  4 3-103      Petit Lac Manicouagan hydroquebec       -67.8     51.8           0
#>  5 3-104      Barrage Intermédiaire hydroquebec       -67.9     51.8          NA
#>  6 3-105      Hart-Jaune            hydroquebec       -67.9     51.8          NA
#>  7 3-106      Manic-5               hydroquebec       -68.7     50.6          NA
#>  8 3-107      Manic-3               hydroquebec       -68.6     49.7          NA
#>  9 3-109      Manic-2               hydroquebec       -68.3     49.3          NA
#> 10 3-110      Manic-1               hydroquebec       -68.3     49.2          NA
#> # ℹ 436 more rows
#> # ℹ 3 more variables: period_start <date>, period_end <date>, notes <list>
try(hc_read_stations(source = "cehq"))
#> # A tibble: 204 × 9
#>    station_id station_name          provider_name longitude latitude elevation_m
#>    <chr>      <chr>                 <chr>             <dbl>    <dbl>       <dbl>
#>  1 030355     Noire                 cehq              -72.3     45.5          NA
#>  2 010802     Bonaventure           cehq              -65.6     48.2          NA
#>  3 010902     Petite rivière Casca… cehq              -65.7     48.2          NA
#>  4 011003     Cascapédia            cehq              -66.2     48.6          NA
#>  5 011204     Nouvelle              cehq              -66.3     48.2          NA
#>  6 011508     Décharge du lac Mata… cehq              -67.4     48.5          NA
#>  7 011509     Matapédia             cehq              -67.1     48.1          NA
#>  8 030356     Le Renne              cehq              -72.6     45.7          NA
#>  9 020404     York                  cehq              -64.9     48.8          NA
#> 10 020602     Dartmouth             cehq              -64.7     49.0          NA
#> # ℹ 194 more rows
#> # ℹ 3 more variables: period_start <date>, period_end <date>, notes <list>
# }
```
