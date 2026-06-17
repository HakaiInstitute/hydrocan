
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hydrocan <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/HakaiInstitute/hydrocan/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HakaiInstitute/hydrocan/actions/workflows/R-CMD-check.yaml)
[![R-universe
version](https://HakaiInstitute.r-universe.dev/hydrocan/badges/version)](https://HakaiInstitute.r-universe.dev/hydrocan)
[![r-universe
status](https://HakaiInstitute.r-universe.dev/hydrocan/badges/checks)](https://HakaiInstitute.r-universe.dev/hydrocan)
<!-- badges: end -->

hydrocan provides unified access to Canadian hydrometric data from
diverse web-published sources. It is complementary to
[tidyhydat](https://docs.ropensci.org/tidyhydat/), which covers the
federal Water Survey of Canada (WSC) network. hydrocan adds provincial
and utility networks data sources that have no existing R package.

Every function returns a consistent tibble regardless of the underlying
data source, so analysis code works unchanged as new sources are added.

## Installation

You can install the CRAN version via:

``` r
install.packages("hydrocan")
```

You can install the development version of hydrocan from the Hakai
Institute r-universe server:

``` r
install.packages("hydrocan", repos = "https://hakaiinstitute.r-universe.dev")
```

## Usage

The reading functions accept one or more station IDs, a date range, and
an optional `source` argument to target a specific adapter directly.
When `source` is omitted, the router matches each station ID to its data
source automatically. Every function returns the same tibble columns
regardless of source.

### Station metadata

If you don’t know the station that you want to query, start with
`hc_read_stations()`. The station IDs it returns can be passed to the
reading functions below.

``` r
library(hydrocan)

hc_read_stations("hydroquebec")
#> # A tibble: 94 × 9
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
#> # ℹ 84 more rows
#> # ℹ 3 more variables: period_start <date>, period_end <date>, notes <list>
```

### Daily flows

`hc_read_daily_flows()` returns daily aggregates. Note that not all
providers publish daily data: check the `has_daily_flows` column of
`hc_list_sources()` to see which ones do.

You can query daily flows from two stations across two sources in one
call. hydrocan detects that “030101” belongs to CEHQ and “3-230” to
Hydro-Quebec:

``` r
hc_read_daily_flows(
  station_id = c("030101", "3-230"),
  start_date = Sys.Date() - 30
)
#> ── hydrocan ────────────────────────────────────────────────────────────────────
#>   Observations: 35
#>   Sources: cehq, hydroquebec
#>   Parameters: water_discharge, water_inflow
#>   Date range: 2026-05-18 to 2026-06-14
#>   Stations: 2 returned
#> ✔ All stations returned.
#> ────────────────────────────────────────────────────────────────────────────────
#> # A tibble: 35 × 8
#>    station_id date       value parameter       unit  provider_name quality_code
#>  * <chr>      <date>     <dbl> <chr>           <chr> <chr>         <chr>       
#>  1 030101     2026-05-18  8.92 water_discharge m3/s  cehq          MJ          
#>  2 030101     2026-05-19  7.14 water_discharge m3/s  cehq          MJ          
#>  3 030101     2026-05-20  6.21 water_discharge m3/s  cehq          MJ          
#>  4 030101     2026-05-21  5.07 water_discharge m3/s  cehq          MJ          
#>  5 030101     2026-05-22  4.01 water_discharge m3/s  cehq          MJ          
#>  6 030101     2026-05-23  3.37 water_discharge m3/s  cehq          MJ          
#>  7 030101     2026-05-24  2.90 water_discharge m3/s  cehq          MJ          
#>  8 030101     2026-05-25  3.16 water_discharge m3/s  cehq          MJ          
#>  9 030101     2026-05-26  5.20 water_discharge m3/s  cehq          MJ          
#> 10 030101     2026-05-27  5.23 water_discharge m3/s  cehq          MJ          
#> # ℹ 25 more rows
#> # ℹ 1 more variable: qf_desc <chr>
```

### Sub-daily flows

`hc_read_flows()` returns sub-daily observations.

``` r
# Sub-daily flow observations from a single Hydro-Quebec station
hc_read_flows(
  station_id = "3-100",
  start_date = Sys.Date() - 7
)
#> ── hydrocan ────────────────────────────────────────────────────────────────────
#>   Observations: 288
#>   Source: hydroquebec
#>   Parameters: water_discharge, water_discharge_turbined,
#>   water_discharge_spilled
#>   Date range: 2026-06-10 to 2026-06-13 23:00:00
#>   Station: 1 returned
#> ✔ All stations returned.
#> ────────────────────────────────────────────────────────────────────────────────
#> # A tibble: 288 × 8
#>    station_id timestamp           value parameter            unit  provider_name
#>  * <chr>      <dttm>              <dbl> <chr>                <chr> <chr>        
#>  1 3-100      2026-06-10 00:00:00  266. water_discharge      m3/s  hydroquebec  
#>  2 3-100      2026-06-10 00:00:00  266. water_discharge_tur… m3/s  hydroquebec  
#>  3 3-100      2026-06-10 00:00:00    0  water_discharge_spi… m3/s  hydroquebec  
#>  4 3-100      2026-06-10 01:00:00  266. water_discharge      m3/s  hydroquebec  
#>  5 3-100      2026-06-10 01:00:00    0  water_discharge_spi… m3/s  hydroquebec  
#>  6 3-100      2026-06-10 01:00:00  266. water_discharge_tur… m3/s  hydroquebec  
#>  7 3-100      2026-06-10 02:00:00  266. water_discharge      m3/s  hydroquebec  
#>  8 3-100      2026-06-10 02:00:00    0  water_discharge_spi… m3/s  hydroquebec  
#>  9 3-100      2026-06-10 02:00:00  266. water_discharge_tur… m3/s  hydroquebec  
#> 10 3-100      2026-06-10 03:00:00    0  water_discharge_spi… m3/s  hydroquebec  
#> # ℹ 278 more rows
#> # ℹ 2 more variables: quality_code <chr>, qf_desc <chr>
```

## Supported sources

``` r
hc_list_sources()
```

## Citation

If you use data accessed through hydrocan in your work, please cite the
original data source. Use `hc_citation()` to get a formatted citation
and BibTeX entry:

``` r
hc_citation("hydroquebec")
#> To cite 'hydroquebec' data in publications use:
#> 
#>   Hydro-Québec (2026). "Débits et apports naturels aux installations
#>   d’Hydro-Québec." Accessed via the hydrocan R package License: CC
#>   BY-NC 4.0,
#>   <https://donnees.hydroquebec.com/explore/dataset/donnees-hydrometriques/information/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Misc{,
#>     title = {Débits et apports naturels aux installations d’Hydro-Québec},
#>     year = {2026},
#>     note = {Accessed via the hydrocan R package License: CC BY-NC 4.0},
#>     author = {{Hydro-Québec}},
#>     url = {https://donnees.hydroquebec.com/explore/dataset/donnees-hydrometriques/information/},
#>   }
```
