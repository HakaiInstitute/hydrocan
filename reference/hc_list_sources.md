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
