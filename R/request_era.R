#' @importFrom ecmwfr wf_archetype
NULL

#' Request ERA5 data for TROLL
#'
#' Description.
#'
#' @param variables char. Description.
#' @param year int. Description.
#' @param months int. Description.
#' @param days int. Description.
#' @param hours int. Description.
#' @param prefix  char. Description.
#' @param xutm int. Description.
#' @param yutm int. Description.
#' @param format char. Description.
#'
#' @return ERA request for `ecmwfr` as a list of attributes.
#'
#' @export
#'
request_era <- function(variables = c(
                          "u10", "v10", "d2m", "t2m",
                          "ssr", "sp", "tp"
                        ),
                        year = 2004,
                        months = 1:12,
                        days = 1:31,
                        hours = 0:23,
                        prefix = "ERA5land_Paracou",
                        xutm = 5.267241344232334, # paracou
                        yutm = -52.92436802555797, # paracou
                        format = "netcdf") {
  warning("Need to add arguments check")

  varlong <- c(
    "10m_u_component_of_wind",
    "10m_v_component_of_wind",
    "2m_dewpoint_temperature",
    "2m_temperature",
    "surface_net_solar_radiation",
    "surface_pressure",
    "total_precipitation"
  )
  names(varlong) <- c(
    "u10", "v10", "d2m", "t2m",
    "ssr", "sp", "tp"
  )
  variables <- varlong[variables]

  dynamic_request <- wf_archetype(
    request <- list( # nolint
      "dataset_short_name" = "reanalysis-era5-land",
      "format" = format,
      "variable" = unname(variables),
      "year" = as.character(year),
      "month" = sprintf("%02d", months[1]),
      "day" = sprintf("%02d", 1:31),
      "time" = sprintf("%02d:00", 0:23),
      "target" = paste0(prefix, "_", year, "_", months[1], ".nc"),
      "area" = c(xutm, yutm, xutm, yutm)
    ),
    dynamic_fields = c("month", "target")
  )

  requests <- lapply(sprintf("%02d", months), function(m) {
    dynamic_request(
      month = m,
      target = paste0(prefix, "_", year, "_", m, ".nc")
    )
  })

  return(requests)
}
