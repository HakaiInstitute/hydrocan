# Rebuild precomputed vignettes. Run from the package root:
#   source("vignettes/precompile.R")
#
# combining-sources queries live web services (and Hydro-Quebec keeps only
# ~10 days of data), so it is knitted locally and the resulting .Rmd and
# figures are committed. CRAN and CI then build it without network access.
# Requires a local HYDAT database for the tidyhydat section
# (tidyhydat::download_hydat()).

pkgload::load_all(".")

withr::with_envvar(c(TZ = "Etc/GMT+8"), {
  withr::with_dir("vignettes", {
    knitr::knit("combining-sources.Rmd.orig", "combining-sources.Rmd")
  })
})
