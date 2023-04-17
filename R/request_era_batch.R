#' Request ERA5 batch data for TROLL
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
#' @return List of ERA requests for `ecmwfr` as a list of attributes.
#'
#' @export
#'
request_era_batch <- function(variables = c(
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
  requests <- lapply(
    years,
    function(y) {
      lapply(months, function(m) {
        request_era(
          variables = variables, years = y, months = m,
          days = days, hours = hours, target = target,
          area = area, format = format
        )
      })
    }
  )

  return(requests)
}
