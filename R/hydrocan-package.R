#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  register_hydrocan_adapter(hydrocan_adapter_hydroquebec())
}
