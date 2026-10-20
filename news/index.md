# Changelog

## hydrocan 0.0.2.9000

- BC Aquarius requests now include `end_date`. Previously the last
  requested day was dropped from both daily and sub-daily results,
  because the Aquarius API treats the end time as an exclusive cutoff at
  00:00 UTC
  ([\#35](https://github.com/HakaiInstitute/hydrocan/issues/35),
  [\#36](https://github.com/HakaiInstitute/hydrocan/issues/36)).

## hydrocan 0.0.2

CRAN release: 2026-10-19

- Exclude Hakai station IDs from the bundled `bc_aquarius_stations` list
  so that stations already served by the Hakai ERDDAP adapter are no
  longer duplicated by the BC Aquarius adapter
  ([\#28](https://github.com/HakaiInstitute/hydrocan/issues/28)).

## hydrocan 0.0.1

CRAN release: 2026-06-16

- First CRAN release.
- Add BC Aquarius adapter (sub-daily and daily discharge and stage),
  with the station list bundled in package data
  ([\#24](https://github.com/HakaiInstitute/hydrocan/issues/24)).
- Add Hakai ERDDAP streamflow adapter
  ([\#20](https://github.com/HakaiInstitute/hydrocan/issues/20)).
- Add
  [`hc_list_sources()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_list_sources.md)
  exposing each adapter’s license and terms information.
- Add introductory and adapter-authoring vignettes
  ([\#13](https://github.com/HakaiInstitute/hydrocan/issues/13),
  [\#22](https://github.com/HakaiInstitute/hydrocan/issues/22)).
- Add print methods for hydrocan objects
  ([\#5](https://github.com/HakaiInstitute/hydrocan/issues/5)).

## hydrocan 0.0.0.9000

- Add Hydro-Quebec adapter (realtime and daily flows)
- Add CEHQ adapter (daily flows and levels)
- Initial adapter infrastructure with router and schema validation
