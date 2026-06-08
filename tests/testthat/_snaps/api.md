# print.hydrocan_realtime snapshot

    Code
      print(result)
    Output
      -- hydrocan --------------------------------------------------------------------
        Observations: 2
        Source: mock
        Parameter: water_discharge
        Date range: 2024-01-01 06:00:00 to 2024-01-01 18:00:00
        Station: 1 returned
      v All stations returned.
      --------------------------------------------------------------------------------
      # A tibble: 2 x 8
        station_id timestamp           value parameter       unit  provider_name
      * <chr>      <dttm>              <dbl> <chr>           <chr> <chr>        
      1 TOCHI001   2024-01-01 06:00:00     1 water_discharge m3/s  mock         
      2 TOCHI001   2024-01-01 18:00:00     2 water_discharge m3/s  mock         
      # i 2 more variables: quality_code <chr>, qf_desc <chr>

# print.hydrocan_daily snapshot

    Code
      print(result)
    Output
      -- hydrocan --------------------------------------------------------------------
        Observations: 1
        Source: mock
        Parameter: water_discharge
        Date range: 2024-01-01 to 2024-01-01
        Station: 1 returned
      v All stations returned.
      --------------------------------------------------------------------------------
      # A tibble: 1 x 8
        station_id date       value parameter unit  provider_name quality_code qf_desc
      * <chr>      <date>     <dbl> <chr>     <chr> <chr>         <chr>        <chr>  
      1 TOCHI001   2024-01-01    10 water_di~ m3/s  mock          <NA>         <NA>   

# print.hydrocan_realtime reports stations that were requested but not returned

    Code
      print(result)
    Output
      -- hydrocan --------------------------------------------------------------------
        Observations: 2
        Source: mock
        Parameter: water_discharge
        Date range: 2024-01-01 06:00:00 to 2024-01-01 18:00:00
        Station: 1 returned
      ! Stations requested but not returned: "ALDERAAN001"
      --------------------------------------------------------------------------------
      # A tibble: 2 x 8
        station_id timestamp           value parameter       unit  provider_name
      * <chr>      <dttm>              <dbl> <chr>           <chr> <chr>        
      1 TOCHI001   2024-01-01 06:00:00     1 water_discharge m3/s  mock         
      2 TOCHI001   2024-01-01 18:00:00     2 water_discharge m3/s  mock         
      # i 2 more variables: quality_code <chr>, qf_desc <chr>

