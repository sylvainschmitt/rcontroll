#' @importFrom ecmwfr wf_archetype
NULL

#' Request ERA5 data for TROLL
#'
#' Description.
#'
#' @param variables char. Requested variable among "u10", "v10", "d2m", "t2m",
#'   "ssr", "sp", and "tp".
#' @param years int. Requested years between 1950 and today.
#' @param months int. Requested months between 1 and 12.
#' @param days int. Requested months between 1 and 31.
#' @param hours int. Requested hours between 0 and 23.
#' @param prefix  char. Prefix used to name the file.
#' @param xutm int. Requested site longitude in UTM.
#' @param yutm int. Requested site latitude in UTM.
#' @param format char. Requested format, default netcdf.
#'
#' @return ERA request for `ecmwfr` as a list of attributes.
#'
#' @export
#'
request_era <- function(variables = c(
                          "u10", "v10", "d2m", "t2m",
                          "ssr", "sp", "tp"
                        ),
                        years = 2004:2005,
                        months = 1:12,
                        days = 1:31,
                        hours = 0:23,
                        prefix = "ERA5land_Paracou",
                        xutm = 5.267241344232334, # paracou
                        yutm = -52.92436802555797, # paracou
                        format = "netcdf") {
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
      "year" = as.character(years[1]),
      "month" = sprintf("%02d", months[1]),
      "day" = sprintf("%02d", 1:31),
      "time" = sprintf("%02d:00", 0:23),
      "target" = paste0(prefix, "_", years[1], "_", months[1], ".nc"),
      "area" = c(xutm, yutm, xutm, yutm)
    ),
    dynamic_fields = c("year", "month", "target")
  )

  years_months <- lapply(
    years,
    function(y) {
      paste0(y, "_", sprintf("%02d", months))
    }
  ) %>%
    unlist()

  requests <- lapply(years_months, function(y_m) {
    y <- strsplit(y_m, "_")[[1]][1]
    m <- strsplit(y_m, "_")[[1]][2]
    dynamic_request(
      year = y,
      month = m,
      target = paste0(prefix, "_", y, "_", m, ".nc")
    )
  })

  return(requests)
}
