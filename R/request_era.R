#' Request ERA5 data for TROLL
#'
#' Description.
#'
#' @param variables
#' @param years
#' @param months
#' @param days
#' @param hours
#' @param target
#' @param area
#' @param format
#' @param request
#'
#' @return ERA request for `ecmwfr` as a list of attributes.
#'
#' @export
#'
request_era <- function(variables = c(
                          "u10", "v10", "d2m", "t2m",
                          "ssr", "sp", "tp"
                        ),
                        years = 2020,
                        months = 1,
                        days = 1:31,
                        hours = 0:23,
                        target = NULL,
                        area = c(5.77, -54.6, 2.11, -51.63), # paracou
                        format = "netcdf") {
  warning("Need to add arguments check")

  if (is.null(target)) {
    yearstamp <- c(min(years), max(years))
    if (yearstamp[1] == yearstamp[2]) {
      yearstamp <- as.character(yearstamp[1])
    } else {
      yearstamp <- paste0(yearstamp[1], ":", yearstamp[2])
    }
    monthstamp <- c(min(months), max(months))
    if (monthstamp[1] == monthstamp[2]) {
      monthstamp <- as.character(monthstamp[1])
    } else {
      monthstamp <- paste0(monthstamp[1], ":", monthstamp[2])
    }

    target <- paste0(
      "ERA5land_",
      yearstamp,
      "_",
      monthstamp,
      ".nc"
    )
  }

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

  request <- list(
    "dataset_short_name" = "reanalysis-era5-land",
    "format" = format,
    "variable" = unname(variables),
    "year" = as.character(years),
    "month" = sprintf("%02d", months),
    "day" = sprintf("%02d", days),
    "time" = sprintf("%02d:00", hours),
    "target" = target,
    "area" = area,
    format = format
  )

  return(request)
}
