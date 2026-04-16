#' Calculate Specific Discharge
#'
#' Normalizes streamflow by watershed drainage area, enabling
#' comparison of discharge across watersheds of different sizes.
#'
#' @param flow Numeric. Streamflow in m^2/s.
#' @param drainage_area Numeric. Watershed drainage area in km^2.
#'
#' @return A numeric vector of specific discharge values (m^3/s/km^2).
#' @export
#'
#' @examples
#' specific_discharge(flow = 10, drainage_area = 500)
specific_discharge <- function(flow, drainage_area) {
  flow / drainage_area
}
