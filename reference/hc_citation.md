# Cite a hydrocan data source

Returns a citation for a registered data source, formatted as a
[`bibentry()`](https://rdrr.io/r/utils/bibentry.html) object. The output
behaves like [`citation()`](https://rdrr.io/r/utils/citation.html): it
prints a human-readable reference and a BibTeX entry, and can be passed
to [`toBibtex()`](https://rdrr.io/r/utils/toLatex.html).

## Usage

``` r
hc_citation(source)
```

## Arguments

- source:

  Single character string naming the data source to cite. See
  [`hc_list_sources()`](https://hakaiinstitute.github.io/hydrocan/reference/hc_list_sources.md)
  for available names.

## Value

A [`bibentry()`](https://rdrr.io/r/utils/bibentry.html) object.

## Examples

``` r
hc_citation("cehq")
#> To cite 'cehq' data in publications use:
#> 
#>   Ministère de l’Environnement, de la Lutte contre les changements
#>   climatiques, de la Faune et des Parcs (2026). “Stations
#>   hydrométriques.” Accessed via the hydrocan R package License: CC BY
#>   4.0, <https://www.cehq.gouv.qc.ca/hydrometrie/index.htm>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Misc{,
#>     title = {Stations hydrométriques},
#>     year = {2026},
#>     note = {Accessed via the hydrocan R package License: CC BY 4.0},
#>     author = {{Ministère de l’Environnement, de la Lutte contre les changements climatiques, de la Faune et des Parcs}},
#>     url = {https://www.cehq.gouv.qc.ca/hydrometrie/index.htm},
#>   }
toBibtex(hc_citation("hydroquebec"))
#> @Misc{,
#>   title = {Débits et apports naturels aux installations d’Hydro-Québec},
#>   year = {2026},
#>   note = {Accessed via the hydrocan R package License: CC BY-NC 4.0},
#>   author = {{Hydro-Québec}},
#>   url = {https://donnees.hydroquebec.com/explore/dataset/donnees-hydrometriques/information/},
#> }
```
