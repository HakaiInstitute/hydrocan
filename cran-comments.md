## Re-submission comments:

- all \dontrun changed to \donttest or removed.
- hydrocan is a data-access wrapper - it implements no published statistical/scientific method, so there are no method papers to cite. The data-source URLs are already documented per-adapter and surfaced via hc_citation() / hc_list_sources(), not method references.

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

* This is a new release.
