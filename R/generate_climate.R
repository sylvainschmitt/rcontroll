#' @include utils-pipe.R
#' @importFrom dplyr mutate select arrange lag group_by do slice mutate_at funs
#'   ungroup bind_rows rename n recode left_join mutate_all summarise
#' @importFrom lubridate as_datetime force_tz hms year month hour as_date
#'   days_in_month
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
#'
#' @return A [data.frame()] with climatic day variation (night temperature and
#'   rainfall).
#'
#' @export
#'
generate_climate <- function(hourly_data,
                             daytime_start = 7,
                             daytime_end = 19) {
  # tidytrick
  temperature <- rainfall <- NULL
  Temperature <- Rainfall <- time_hour <- DayJulian <- NULL # nolint
  Snet <- VPD <- WS <- time_numeric <- NULL # nolint

  # climate
  hourly_data %>%
    mutate(date = date(time)) %>%
    mutate(time = hour(time)) %>%
    select(date, time, temperature, rainfall) %>%
    mutate(temperature = ifelse(time < daytime_start, NA, temperature)) %>%
    mutate(temperature = ifelse(time >= daytime_end, NA, temperature)) %>%
    group_by(date) %>%
    summarise(
      NightTemperature = mean(temperature, na.rm = TRUE),
      Rainfall = sum(rainfall, na.rm = TRUE)
    ) %>%
    select(-date)
}
