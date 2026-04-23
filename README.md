
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hydrocan

<!-- badges: start -->

[![R-CMD-check](https://github.com/HakaiInstitute/hydrocan/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HakaiInstitute/hydrocan/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

hydrocan provides unified access to Canadian hydrometric data from
diverse web-published sources. It is complementary to
[tidyhydat](https://docs.ropensci.org/tidyhydat/), which covers the
federal Water Survey of Canada (WSC) network. hydrocan adds provincial
and utility networks that have no existing R package.

Every function returns a consistent tibble regardless of the underlying
data source, so analysis code works unchanged as new sources are added.

## Installation

``` r
# install.packages("pak")
pak::pak("HakaiInstitute/hydrocan")
```

## Usage

The two main functions are `hc_read_flows()` for sub-daily observations
and `hc_read_daily_flows()` for daily aggregates. Both accept one or
more station numbers, a date range, and an optional `source` argument to
target a specific adapter directly.

``` r
library(hydrocan)

# Sub-daily flow observations - source auto-detected from station number
hc_read_flows(
  station_number = "3-230",
  start_date = Sys.Date() - 7,
  end_date = Sys.Date()
)
#> iterating ■■■■■■■                           20% | ETA:  5s
#> # A tibble: 192 × 8
#>    station_number datetime            value parameter units source      approval
#>    <chr>          <dttm>              <dbl> <chr>     <chr> <chr>       <chr>   
#>  1 3-230          2026-04-16 00:00:00  191. flow      m3/s  hydroquebec <NA>    
#>  2 3-230          2026-04-16 00:00:00  191. flow      m3/s  hydroquebec <NA>    
#>  3 3-230          2026-04-16 01:00:00  191. flow      m3/s  hydroquebec <NA>    
#>  4 3-230          2026-04-16 01:00:00  191. flow      m3/s  hydroquebec <NA>    
#>  5 3-230          2026-04-16 02:00:00  191. flow      m3/s  hydroquebec <NA>    
#>  6 3-230          2026-04-16 02:00:00  191. flow      m3/s  hydroquebec <NA>    
#>  7 3-230          2026-04-16 03:00:00  191  flow      m3/s  hydroquebec <NA>    
#>  8 3-230          2026-04-16 03:00:00  191  flow      m3/s  hydroquebec <NA>    
#>  9 3-230          2026-04-16 04:00:00  191. flow      m3/s  hydroquebec <NA>    
#> 10 3-230          2026-04-16 04:00:00  191. flow      m3/s  hydroquebec <NA>    
#> # ℹ 182 more rows
#> # ℹ 1 more variable: quality_flag <chr>

hc_read_daily_flows(
  station_number = "3-230",
  start_date = Sys.Date() - 7,
  end_date = Sys.Date()
)
#> # A tibble: 4 × 8
#>   station_number date       value parameter units source   approval quality_flag
#>   <chr>          <date>     <dbl> <chr>     <chr> <chr>    <chr>    <chr>       
#> 1 3-230          2026-04-16  13.5 flow      m3/s  hydroqu… <NA>     Apport filt…
#> 2 3-230          2026-04-17  16.6 flow      m3/s  hydroqu… <NA>     Apport filt…
#> 3 3-230          2026-04-18  19.4 flow      m3/s  hydroqu… <NA>     Apport filt…
#> 4 3-230          2026-04-19  21.6 flow      m3/s  hydroqu… <NA>     Apport filt…
```

## Supported sources

``` r
hc_list_sources()
#> # A tibble: 1 × 5
#>   name        description                 has_flows has_daily_flows has_stations
#>   <chr>       <chr>                       <lgl>     <lgl>           <lgl>       
#> 1 hydroquebec Hydro-Quebec open data (Op… TRUE      TRUE            TRUE
```
