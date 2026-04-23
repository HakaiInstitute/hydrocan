#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  register_hydrocan_adapter(hydrocan_adapter_hydroquebec())
}
