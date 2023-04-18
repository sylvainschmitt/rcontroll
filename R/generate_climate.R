#' @include utils-pipe.R
#' @importFrom dplyr mutate select arrange lag group_by do slice mutate_at funs
#'   ungroup bind_rows rename n recode left_join mutate_all summarise
#' @importFrom lubridate as_datetime force_tz hms year month hour as_date
#'   days_in_month
#' @importFrom zoo na.approx
#' @importFrom utils data
NULL

#' Generate climate dataset
#'
#' Description
#'
#' @param hourly_data data.frame. Description.
#' @param daytime_start int. Daytime starting hour to compute nigh and day
#'   variables (default 7).
#' @param daytime_end int. Daytime ending hour to compute nigh and day variables
#'   (default 19).
#'
#' @return A list with two [data.frame()]: dailyvar and climate.
#'
#' @details
#'
#' Details.
#'
#' @export
#'
generate_climate <- function(hourly_data,
                             daytime_start = 7,
                             daytime_end = 19) {
  warning("Need to add arguments check")

  # tidytrick
  Temperature <- Rainfall <- time_hour <- DayJulian <- NULL # nolint
  Snet <- VPD <- WS <- time_numeric <- NULL # nolint

  # climate
  climate <- hourly_data %>%
    mutate(date = date(time)) %>%
    mutate(time = hour(time)) %>%
    select(date, time, Temperature, Rainfall) %>%
    mutate(Temperature = ifelse(time < daytime_start, NA, Temperature)) %>%
    mutate(Temperature = ifelse(time >= daytime_end, NA, Temperature)) %>%
    group_by(date) %>%
    summarise(
      NightTemperature = mean(Temperature, na.rm = TRUE),
      Rainfall = sum(Rainfall)
    ) %>%
    select(-date)

  # dailyvar
  dailyvar <- data.frame(time = c(
    hourly_data$time,
    hourly_data$time + 30 * 60
  )) %>%
    arrange(time) %>%
    left_join(hourly_data, by = join_by(time)) %>%
    select(-Rainfall) %>%
    mutate_at(c("Temperature", "Snet", "VPD", "WS"),
      na.approx,
      rule = 2
    ) %>%
    mutate(time_hour = hour(time)) %>%
    filter(time_hour >= daytime_start, time_hour < daytime_end) %>%
    select(-time_hour) %>%
    mutate(time_numeric = hour(time) + minute(time) / 60) %>%
    mutate(DayJulian = as.POSIXlt(time)$yday + 1) %>%
    select(DayJulian, time_numeric, Temperature, Snet, VPD, WS)

  return(list(
    dailyvar = dailyvar,
    climate = climate
  ))
}
