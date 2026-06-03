# hydrocan

hydrocan provides unified access to Canadian hydrometric data from
diverse web-published sources. It is complementary to
[tidyhydat](https://docs.ropensci.org/tidyhydat/), which covers the
federal Water Survey of Canada (WSC) network. hydrocan adds provincial
and utility networks data sources that have no existing R package.

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
for daily aggregates. Both accept one or more station IDs, a date range,
and an optional `source` argument to target a specific adapter directly.
When `source` is omitted, the router matches each station ID to its data
source automatically. If you don’t know the station that you want to
query, you can make use of
[`hc_read_stations()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_read_stations.md)

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

`hy_read_stations` station IDs can be passed to `hc_read_flows` to
return hydrometric data for that station.

``` r

# Sub-daily flow observations from a single Hydro-Quebec station
hc_read_flows(
  station_id = "3-100",
  start_date = Sys.Date() - 7
)
#> ── hydrocan ────────────────────────────────────────────────────────────────────
#>   Observations: 288
#>   Source: hydroquebec
#>   Parameters: water_discharge_spilled, water_discharge,
#>   water_discharge_turbined
#>   Date range: 2026-05-26 to 2026-05-29 23:00:00
#>   Station: 1 returned
#> ✔ All stations returned.
#> ────────────────────────────────────────────────────────────────────────────────
#> # A tibble: 288 × 8
#>    station_id timestamp           value parameter            unit  provider_name
#>  * <chr>      <dttm>              <dbl> <chr>                <chr> <chr>        
#>  1 3-100      2026-05-26 00:00:00    0  water_discharge_spi… m3/s  hydroquebec  
#>  2 3-100      2026-05-26 00:00:00  328. water_discharge      m3/s  hydroquebec  
#>  3 3-100      2026-05-26 00:00:00  328. water_discharge_tur… m3/s  hydroquebec  
#>  4 3-100      2026-05-26 01:00:00  327. water_discharge_tur… m3/s  hydroquebec  
#>  5 3-100      2026-05-26 01:00:00  327. water_discharge      m3/s  hydroquebec  
#>  6 3-100      2026-05-26 01:00:00    0  water_discharge_spi… m3/s  hydroquebec  
#>  7 3-100      2026-05-26 02:00:00    0  water_discharge_spi… m3/s  hydroquebec  
#>  8 3-100      2026-05-26 02:00:00  328. water_discharge      m3/s  hydroquebec  
#>  9 3-100      2026-05-26 02:00:00  328. water_discharge_tur… m3/s  hydroquebec  
#> 10 3-100      2026-05-26 03:00:00  328  water_discharge      m3/s  hydroquebec  
#> # ℹ 278 more rows
#> # ℹ 2 more variables: quality_code <chr>, qf_desc <chr>
```

You can also query daily flows from two stations across two sources in
one call. hydrocan detects that “030101” belongs to CEHQ and “3-230” to
Hydro-Quebec:

``` r

hc_read_daily_flows(
  station_id = c("030101", "3-230"),
  start_date = Sys.Date() - 30
)
#> ── hydrocan ────────────────────────────────────────────────────────────────────
#>   Observations: 30
#>   Sources: cehq, hydroquebec
#>   Parameters: water_discharge, water_inflow
#>   Date range: 2026-05-03 to 2026-05-29
#>   Stations: 2 returned
#> ✔ All stations returned.
#> ────────────────────────────────────────────────────────────────────────────────
#> # A tibble: 30 × 8
#>    station_id date       value parameter       unit  provider_name quality_code
#>  * <chr>      <date>     <dbl> <chr>           <chr> <chr>         <chr>       
#>  1 030101     2026-05-03  5.28 water_discharge m3/s  cehq          MJ          
#>  2 030101     2026-05-04  5.11 water_discharge m3/s  cehq          MJ          
#>  3 030101     2026-05-05  6.32 water_discharge m3/s  cehq          MJ          
#>  4 030101     2026-05-06 10.2  water_discharge m3/s  cehq          MJ          
#>  5 030101     2026-05-07 24.0  water_discharge m3/s  cehq          MJ          
#>  6 030101     2026-05-08 18.6  water_discharge m3/s  cehq          MJ          
#>  7 030101     2026-05-09 12.0  water_discharge m3/s  cehq          MJ          
#>  8 030101     2026-05-10  8.51 water_discharge m3/s  cehq          MJ          
#>  9 030101     2026-05-11  8.02 water_discharge m3/s  cehq          MJ          
#> 10 030101     2026-05-12  6.94 water_discharge m3/s  cehq          MJ          
#> # ℹ 20 more rows
#> # ℹ 1 more variable: qf_desc <chr>
```

## Supported sources

``` r

hc_list_sources()
```

## Citation

If you use data accessed through hydrocan in your work, please cite the
original data source. Use
[`hc_citation()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_citation.md)
to get a formatted citation and BibTeX entry:

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
