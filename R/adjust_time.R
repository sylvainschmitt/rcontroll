#' @importFrom dplyr filter
#' @importFrom lubridate date force_tz
NULL

#' Generate climate dataset
#'
#' Description
#'
#' @param hourly_data data.frame. Hourly or half-hourly data with a 'time'
#'   column in UTC to be adjusted to the given time zone.
#' @param tz char. Time zone. Can be obtained from the coordinates with
#'   [lutz::tz_lookup_coords()].
#'
#' @return The same data frame with adjusted time.
#'
#' @examples
#' adjust_time(data.frame(time = lubridate::as_datetime("2015-01-01 16:30:00")))
#'
#' @export
#'
adjust_time <- function(hourly_data,
                        tz = "America/Cayenne") {
  # checks
  if (!inherits(hourly_data, "data.frame")) {
    stop("hourly_data shoudl inherit from a data frame.")
  } else {
    if (!("time" %in% names(hourly_data))) {
      stop("hourly_data should contain a column named time for adjustement.")
    }
  }
  if (!is.character(tz)) {
    stop("tz should be a string.")
  } else {
    if (!(tz %in% OlsonNames())) {
      stop("tz is not a correct time zone, use OlsonNames() to see available ones.") # nolint
    }
  }


  d0 <- date(hourly_data$time[1])
  t0 <- t1 <- hourly_data$time[1]
  t1 <- force_tz(t1, tz)
  tlag <- t1 - as.POSIXct(t0)
  hourly_data$time <- hourly_data$time - tlag
  hourly_data <- filter(hourly_data, date(time) > (d0 - 1))
  return(hourly_data)
}
