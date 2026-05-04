
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hydrocan <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/HakaiInstitute/hydrocan/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HakaiInstitute/hydrocan/actions/workflows/R-CMD-check.yaml)
[![R-universe
version](https://HakaiInstitute.r-universe.dev/gaplightr/badges/version)](https://HakaiInstitute.r-universe.dev/gaplightr)
[![r-universe
status](https://HakaiInstitute.r-universe.dev/gaplightr/badges/checks)](https://HakaiInstitute.r-universe.dev/gaplightr)
<!-- badges: end --> <!-- badges: end -->

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

The main functions are `hc_read_flows()` for sub-daily observations and
`hc_read_daily_flows()` for daily aggregates. Both accept one or more
station IDs, a date range, and an optional `source` argument to target a
specific adapter directly. When `source` is omitted, the router matches
each station ID to its data source automatically.

``` r
library(hydrocan)

# Sub-daily flow observations from a single Hydro-Quebec station
hc_read_flows(
  station_id = "3-230",
  start_date = Sys.Date() - 7
)
#> ── hydrocan ────────────────────────────────────────────────────────────────────
#>   Observations: 192
#>   Source: hydroquebec
#>   Parameters: water_discharge_spilled, water_discharge
#>   Date range: 2026-04-27 to 2026-04-30 23:00:00
#>   Station: 1 returned
#> ✔ All stations returned.
#> ────────────────────────────────────────────────────────────────────────────────
#> # A tibble: 192 × 8
#>    station_id timestamp           value parameter            unit  provider_name
#>  * <chr>      <dttm>              <dbl> <chr>                <chr> <chr>        
#>  1 3-230      2026-04-27 00:00:00  238. water_discharge_spi… m3/s  hydroquebec  
#>  2 3-230      2026-04-27 00:00:00  238. water_discharge      m3/s  hydroquebec  
#>  3 3-230      2026-04-27 01:00:00  238. water_discharge      m3/s  hydroquebec  
#>  4 3-230      2026-04-27 01:00:00  238. water_discharge_spi… m3/s  hydroquebec  
#>  5 3-230      2026-04-27 02:00:00  239. water_discharge_spi… m3/s  hydroquebec  
#>  6 3-230      2026-04-27 02:00:00  239. water_discharge      m3/s  hydroquebec  
#>  7 3-230      2026-04-27 03:00:00  239. water_discharge      m3/s  hydroquebec  
#>  8 3-230      2026-04-27 03:00:00  239. water_discharge_spi… m3/s  hydroquebec  
#>  9 3-230      2026-04-27 04:00:00  239. water_discharge      m3/s  hydroquebec  
#> 10 3-230      2026-04-27 04:00:00  239. water_discharge_spi… m3/s  hydroquebec  
#> # ℹ 182 more rows
#> # ℹ 2 more variables: quality_code <chr>, qf_desc <chr>

# Daily flows from two stations across two sources in one call -
# the router detects that "030101" belongs to CEHQ and "3-230" to Hydro-Quebec
hc_read_daily_flows(
  station_id = c("030101", "3-230"),
  start_date = Sys.Date() - 7
)
#> ── hydrocan ────────────────────────────────────────────────────────────────────
#>   Observations: 9
#>   Sources: cehq, hydroquebec
#>   Parameters: water_discharge, water_inflow
#>   Date range: 2026-04-27 to 2026-05-01
#>   Stations: 2 returned
#> ✔ All stations returned.
#> ────────────────────────────────────────────────────────────────────────────────
#> # A tibble: 9 × 8
#>   station_id date       value parameter unit  provider_name quality_code qf_desc
#> * <chr>      <date>     <dbl> <chr>     <chr> <chr>         <chr>        <chr>  
#> 1 030101     2026-04-27  6.67 water_di… m3/s  cehq          MJ           <NA>   
#> 2 030101     2026-04-28  5.79 water_di… m3/s  cehq          MJ           <NA>   
#> 3 030101     2026-04-29  5.01 water_di… m3/s  cehq          MJ           <NA>   
#> 4 030101     2026-04-30  4.76 water_di… m3/s  cehq          MJ           <NA>   
#> 5 030101     2026-05-01  5.63 water_di… m3/s  cehq          MJ           <NA>   
#> 6 3-230      2026-04-27 35.4  water_in… m3/s  hydroquebec   <NA>         <NA>   
#> 7 3-230      2026-04-28 40.3  water_in… m3/s  hydroquebec   <NA>         <NA>   
#> 8 3-230      2026-04-29 47.5  water_in… m3/s  hydroquebec   <NA>         <NA>   
#> 9 3-230      2026-04-30 50.2  water_in… m3/s  hydroquebec   <NA>         <NA>
```

## Supported sources

``` r
hc_list_sources()
```
