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
if (FALSE) { # \dontrun{
hc_citation("cehq")
toBibtex(hc_citation("hydroquebec"))
} # }
```
