# hydrocan 0.0.2

- Exclude Hakai station IDs from the bundled `bc_aquarius_stations` list so that
  stations already served by the Hakai ERDDAP adapter are no longer duplicated by
  the BC Aquarius adapter (#28).

# hydrocan 0.0.1

- First CRAN release.
- Add BC Aquarius adapter (sub-daily and daily discharge and stage), with the
  station list bundled in package data (#24).
- Add Hakai ERDDAP streamflow adapter (#20).
- Add `hc_list_sources()` exposing each adapter's license and terms information.
- Add introductory and adapter-authoring vignettes (#13, #22).
- Add print methods for hydrocan objects (#5).

# hydrocan 0.0.0.9000

- Add Hydro-Quebec adapter (realtime and daily flows)
- Add CEHQ adapter (daily flows and levels)
- Initial adapter infrastructure with router and schema validation
