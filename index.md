# hydrocan

> **Warning:** This package is in very early development. The API may
> change without notice and it should not be used in production.

hydrocan provides unified access to Canadian hydrometric data from
diverse web-published sources. It is complementary to
[tidyhydat](https://docs.ropensci.org/tidyhydat/), which covers the
federal Water Survey of Canada (WSC) network. hydrocan adds provincial
and utility networks that have no existing R package.

Every function returns a consistent tibble regardless of the underlying
data source, so analysis code works unchanged as new sources are added.

## Installation

You can install the development version of hydrocan from the Hakai
Institute r-universe server:

``` r

install.packages("hydrocan", repos = "https://hakaiinstitute.r-universe.dev")
```

## Usage

The main functions are
[`hc_read_flows()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_read_flows.md)
for sub-daily observations and
[`hc_read_daily_flows()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_read_daily_flows.md)
for daily aggregates. Both accept one or more station numbers, a date
range, and an optional `source` argument to target a specific adapter
directly. When `source` is omitted, the router matches each station
number to its data source automatically.

``` r

library(hydrocan)

# Sub-daily flow observations from a single Hydro-Quebec station
hc_read_flows(
  station_number = "3-230",
  start_date = Sys.Date() - 7
)
#> ── hydrocan ────────────────────────────────────────────────────────────────────
#>   Observations: 192
#>   Source: hydroquebec
#>   Parameter: flow
#>   Date range: 2026-04-22 to 2026-04-25 23:00:00
#>   Station: 1 returned
#> ✔ All stations returned.
#> ────────────────────────────────────────────────────────────────────────────────
#> # A tibble: 192 × 8
#>    station_number datetime            value parameter units source      approval
#>  * <chr>          <dttm>              <dbl> <chr>     <chr> <chr>       <chr>   
#>  1 3-230          2026-04-22 00:00:00  213. flow      m3/s  hydroquebec <NA>    
#>  2 3-230          2026-04-22 00:00:00  213. flow      m3/s  hydroquebec <NA>    
#>  3 3-230          2026-04-22 01:00:00  214. flow      m3/s  hydroquebec <NA>    
#>  4 3-230          2026-04-22 01:00:00  214. flow      m3/s  hydroquebec <NA>    
#>  5 3-230          2026-04-22 02:00:00  214. flow      m3/s  hydroquebec <NA>    
#>  6 3-230          2026-04-22 02:00:00  214. flow      m3/s  hydroquebec <NA>    
#>  7 3-230          2026-04-22 03:00:00  214. flow      m3/s  hydroquebec <NA>    
#>  8 3-230          2026-04-22 03:00:00  214. flow      m3/s  hydroquebec <NA>    
#>  9 3-230          2026-04-22 04:00:00  215. flow      m3/s  hydroquebec <NA>    
#> 10 3-230          2026-04-22 04:00:00  215. flow      m3/s  hydroquebec <NA>    
#> # ℹ 182 more rows
#> # ℹ 1 more variable: quality_flag <chr>

# Daily flows from two stations across two sources in one call -
# the router detects that "030101" belongs to CEHQ and "3-230" to Hydro-Quebec
hc_read_daily_flows(
  station_number = c("030101", "3-230"),
  start_date = Sys.Date() - 7
)
#> ── hydrocan ────────────────────────────────────────────────────────────────────
#>   Observations: 8
#>   Sources: cehq, hydroquebec
#>   Parameter: flow
#>   Date range: 2026-04-22 to 2026-04-25
#>   Stations: 2 returned
#> ✔ All stations returned.
#> ────────────────────────────────────────────────────────────────────────────────
#> # A tibble: 8 × 8
#>   station_number date       value parameter units source   approval quality_flag
#> * <chr>          <date>     <dbl> <chr>     <chr> <chr>    <chr>    <chr>       
#> 1 030101         2026-04-22 17.7  flow      m3/s  cehq     approved MJ          
#> 2 030101         2026-04-23 13.8  flow      m3/s  cehq     approved MJ          
#> 3 030101         2026-04-24 11.4  flow      m3/s  cehq     approved MJ          
#> 4 030101         2026-04-25  9.32 flow      m3/s  cehq     approved MJ          
#> 5 3-230          2026-04-22 24.3  flow      m3/s  hydroqu… <NA>     Apport filt…
#> 6 3-230          2026-04-23 25.3  flow      m3/s  hydroqu… <NA>     Apport filt…
#> 7 3-230          2026-04-24 27.5  flow      m3/s  hydroqu… <NA>     Apport filt…
#> 8 3-230          2026-04-25 29.7  flow      m3/s  hydroqu… <NA>     Apport filt…
```

## Supported sources

``` r

hc_list_sources()
```
