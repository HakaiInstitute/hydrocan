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


# Suppress R CMD check notes for dplyr column names used in mutate/select
utils::globalVariables(c(
  "timestamp", "end_timestamp", "value", "approval_level",
  "parameter", "unit", "provider_name", "quality_code", "qf_desc"
))
