# Retrieve sub-daily flow observations

Fetches sub-daily observations for one or more stations across the
requested date range. The data source is determined automatically from
the station ID, or fixed explicitly via `source`.

## Usage

``` r
hc_read_flows(station_id, start_date, end_date = Sys.Date(), source = NULL)
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

A tibble with columns `station_id` (chr), `timestamp` (POSIXct UTC),
`value` (dbl), `parameter` (chr), `unit` (chr), `provider_name` (chr),
`quality_code` (chr), and `qf_desc` (chr).

## Examples

``` r
if (FALSE) { # \dontrun{
try(hc_read_flows("703", start_date = Sys.Date() - 7))
} # }
```
