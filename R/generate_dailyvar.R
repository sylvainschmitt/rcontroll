#' @include utils-pipe.R
#' @importFrom dplyr mutate select arrange lag group_by do slice mutate_at funs
#'   ungroup bind_rows rename n recode left_join mutate_all summarise first
#' @importFrom lubridate as_datetime force_tz hms year month hour as_date
#'   days_in_month minute
#' @importFrom utils data
NULL

#' Generate climate dataset
#'
#' Description
#'
#' @param hourly_data data.frame. Half-hourly climatic data formatted for TROLL.
#' @param daytime_start int. Daytime starting hour to compute nigh and day
#'   variables (default 7).
#' @param daytime_end int. Daytime ending hour to compute nigh and day variables
#'   (default 19).
#' @param tz num. Time zone. Can be obtained from the coordinates with
#'   [lutz::tz_lookup_coords()].
#'
#' @return A [data.frame()] with climatic half-hourly variation (temperature,
#'   wind speed, shortwave irradiation and vapour pressure deficit).
#'
#' @details
#'
#' Details.
#'
#' @export
#'
generate_dailyvar <- function(hourly_data,
                              daytime_start = 7,
                              daytime_end = 19,
                              tz = "America/Cayenne") {
  # tidytrick
  temperature <- snet <- vpd <- ws <- NULL
  Temperature <- Rainfall <- time_hour <- DayJulian <- NULL # nolint
  Snet <- VPD <- WS <- time_numeric <- Temp <- NULL # nolint

  # date
  d0 <- date(hourly_data$time[1])
  t0 <- t1 <- hourly_data$time[1]
  t1 <- force_tz(t1, tz)
  tlag <- t1 - as.POSIXct(t0)
  hourly_data$time <- hourly_data$time - tlag
  hourly_data <- filter(hourly_data, date(time) > (d0 - 1))
  rm(t0, t1, tlag)

  # dailyvar
  hourly_data %>%
    rename(Temp = temperature, Snet = snet, VPD = vpd, WS = ws) %>%
    mutate(time_hour = hour(time)) %>%
    filter(time_hour >= daytime_start, time_hour < daytime_end) %>%
    select(-time_hour) %>%
    mutate(time_numeric = hour(time) + minute(time) / 60) %>%
    mutate(DayJulian = date(time) - date(first(time)) + 1) %>%
    mutate(DayJulian = as.numeric(DayJulian)) %>%
    select(DayJulian, time_numeric, Temp, Snet, VPD, WS)
}
