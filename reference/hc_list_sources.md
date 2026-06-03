# List registered data sources

Returns a summary of all data sources currently available via hydrocan,
including their description and which data types they support. No
network calls are made.

## Usage

``` r
hc_list_sources()
```

## Value

A tibble with columns `name` (chr), `description` (chr), `has_flows`
(lgl), `has_daily_flows` (lgl), `has_levels` (lgl), `has_daily_levels`
(lgl), `has_stations` (lgl), `license` (chr), `license_url` (chr),
`terms_url` (chr), and `docs_url` (chr).

## Examples

``` r
hc_list_sources()
#> # A tibble: 3 × 11
#>   name         description has_flows has_daily_flows has_levels has_daily_levels
#>   <chr>        <chr>       <lgl>     <lgl>           <lgl>      <lgl>           
#> 1 hydroquebec  Hydro-Queb… TRUE      TRUE            FALSE      FALSE           
#> 2 cehq         Centre d'e… FALSE     TRUE            FALSE      TRUE            
#> 3 hakai_erddap Hakai Inst… TRUE      FALSE           TRUE       FALSE           
#> # ℹ 5 more variables: has_stations <lgl>, license <chr>, license_url <chr>,
#> #   terms_url <chr>, docs_url <chr>
```
