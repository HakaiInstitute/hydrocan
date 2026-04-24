# print.hydrocan_flows snapshot

    Code
      print(result)
    Message
      -- hydrocan --------------------------------------------------------------------
        Observations: 2
        Source: mock
        Parameter: flow
        Date range: 2024-01-01 06:00:00 to 2024-01-01 18:00:00
        Station: 1 returned
      v All stations returned.
      --------------------------------------------------------------------------------
    Output
      # A tibble: 2 x 8
        station_number datetime            value parameter units source approval   
      * <chr>          <dttm>              <dbl> <chr>     <chr> <chr>  <chr>      
      1 TOCHI001       2024-01-01 06:00:00     1 flow      m3/s  mock   provisional
      2 TOCHI001       2024-01-01 18:00:00     2 flow      m3/s  mock   provisional
      # i 1 more variable: quality_flag <chr>

# print.hydrocan_daily_flows snapshot

    Code
      print(result)
    Message
      -- hydrocan --------------------------------------------------------------------
        Observations: 1
        Source: mock
        Parameter: flow
        Date range: 2024-01-01 to 2024-01-01
        Station: 1 returned
      v All stations returned.
      --------------------------------------------------------------------------------
    Output
      # A tibble: 1 x 8
        station_number date       value parameter units source approval   quality_flag
      * <chr>          <date>     <dbl> <chr>     <chr> <chr>  <chr>      <chr>       
      1 TOCHI001       2024-01-01    10 flow      m3/s  mock   provision~ <NA>        

# print.hydrocan_flows reports stations that were requested but not returned

    Code
      print(result)
    Message
      -- hydrocan --------------------------------------------------------------------
        Observations: 2
        Source: mock
        Parameter: flow
        Date range: 2024-01-01 06:00:00 to 2024-01-01 18:00:00
        Station: 1 returned
      ! Stations requested but not returned: "ALDERAAN001"
      --------------------------------------------------------------------------------
    Output
      # A tibble: 2 x 8
        station_number datetime            value parameter units source approval   
      * <chr>          <dttm>              <dbl> <chr>     <chr> <chr>  <chr>      
      1 TOCHI001       2024-01-01 06:00:00     1 flow      m3/s  mock   provisional
      2 TOCHI001       2024-01-01 18:00:00     2 flow      m3/s  mock   provisional
      # i 1 more variable: quality_flag <chr>

