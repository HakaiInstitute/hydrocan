# Combining data from multiple sources

``` r

library(hydrocan)
library(dplyr)
```

## What sources are available?

``` r

hc_list_sources()
```

## Station metadata across all sources

``` r

stations <- hc_read_stations()
stations

# Filter to a region
stations |>
  filter(latitude > 48, latitude < 50, longitude > -74, longitude < -72)
```

## Fetch data - router auto-detects the source

Pass station IDs from different providers in one call. The router
matches each ID to its adapter automatically.

``` r

# CEHQ station (natural river gauge) + Hydro-Quebec station (reservoir)
daily <- hc_read_daily_flows(
  station_id = c("023301", "3-230"),
  start_date = Sys.Date() - 7,
  end_date = Sys.Date()
)

daily
```

## Combine with bind_rows (same schema, all sources)

Because every adapter returns the same column set, data can be stacked
directly and analysed together.

``` r

cehq_data <- hc_read_daily_flows(
  station_id = c("023301", "030101"),
  start_date = "2015-01-01",
  end_date = "2020-12-31",
  source = "cehq"
)

hq_data <- hc_read_daily_flows(
  station_id = "3-230",
  start_date = Sys.Date() - 7,
  end_date = Sys.Date(),
  source = "hydroquebec"
)

# Stack: works because the schema is identical
all_flows <- bind_rows(cehq_data, hq_data)

all_flows |>
  count(provider_name)
```

## Annual summary across providers

``` r

daily |>
  mutate(year = as.integer(format(date, "%Y"))) |>
  group_by(station_id, provider_name, year) |>
  summarise(
    mean_flow = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(year, provider_name)
```

## Explicit source bypasses the router

Use `source =` to skip station detection entirely - useful when you know
the provider or when working with large station lists.

``` r

hc_read_daily_flows(
  station_id = c("023301", "030101", "040110"),
  start_date = "2010-01-01",
  end_date = "2023-12-31",
  source = "cehq"
)
```
