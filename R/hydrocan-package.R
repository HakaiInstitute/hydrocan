#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  register_hydrocan_adapter(hydrocan_adapter_hydroquebec())
  register_hydrocan_adapter(hydrocan_adapter_cehq())
  register_hydrocan_adapter(hydrocan_adapter_hakai_erddap())
  register_hydrocan_adapter(hydrocan_adapter_bc_aquarius())
}
