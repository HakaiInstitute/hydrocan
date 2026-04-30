# print.hydrocan_realtime snapshot

    Code
      print(result)
    Message
      -- hydrocan --------------------------------------------------------------------
        Observations: 2
        Source: mock
        Parameter: water_discharge
        Date range: 2024-01-01 06:00:00 to 2024-01-01 18:00:00
        Station: 1 returned
      v All stations returned.
      --------------------------------------------------------------------------------
    Output
      # A tibble: 2 x 8
        station_id timestamp           value parameter    unit  provider_name approval
      * <chr>      <dttm>              <dbl> <chr>        <chr> <chr>         <chr>   
      1 TOCHI001   2024-01-01 06:00:00     1 water_disch~ m3/s  mock          provisi~
      2 TOCHI001   2024-01-01 18:00:00     2 water_disch~ m3/s  mock          provisi~
      # i 1 more variable: quality_flag <chr>

# print.hydrocan_daily snapshot

    Code
      print(result)
    Message
      -- hydrocan --------------------------------------------------------------------
        Observations: 1
        Source: mock
        Parameter: water_discharge
        Date range: 2024-01-01 to 2024-01-01
        Station: 1 returned
      v All stations returned.
      --------------------------------------------------------------------------------
    Output
      # A tibble: 1 x 8
        station_id date       value parameter       unit  provider_name approval   
      * <chr>      <date>     <dbl> <chr>           <chr> <chr>         <chr>      
      1 TOCHI001   2024-01-01    10 water_discharge m3/s  mock          provisional
      # i 1 more variable: quality_flag <chr>

# print.hydrocan_realtime reports stations that were requested but not returned

    Code
      print(result)
    Message
      -- hydrocan --------------------------------------------------------------------
        Observations: 2
        Source: mock
        Parameter: water_discharge
        Date range: 2024-01-01 06:00:00 to 2024-01-01 18:00:00
        Station: 1 returned
      ! Stations requested but not returned: "ALDERAAN001"
      --------------------------------------------------------------------------------
    Output
      # A tibble: 2 x 8
        station_id timestamp           value parameter    unit  provider_name approval
      * <chr>      <dttm>              <dbl> <chr>        <chr> <chr>         <chr>   
      1 TOCHI001   2024-01-01 06:00:00     1 water_disch~ m3/s  mock          provisi~
      2 TOCHI001   2024-01-01 18:00:00     2 water_disch~ m3/s  mock          provisi~
      # i 1 more variable: quality_flag <chr>

