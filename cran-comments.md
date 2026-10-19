## hydrocan 0.0.2

- Exclude Hakai station IDs from the bundled `bc_aquarius_stations` list so that
  stations already served by the Hakai ERDDAP adapter are no longer duplicated by
  the BC Aquarius adapter (#28)

## R CMD check results


## Test environments
* win-builder (via `devtools::check_win_oldrelease()`, `devtools::check_win_devel()` and `devtools::check_win_release()`)
* local macOS, R 4.5.2 (via R CMD check --as-cran)
* ubuntu-20.04, r: 'release' (github actions)
* ubuntu-20.04, r: 'devel' (github actions)
* macOS,        r: 'release' (github actions)
* windows,      r: 'release' (github actions)
* ubuntu-devel, r: 'release' (rhub)

0 errors | 0 warnings | 0 notes
