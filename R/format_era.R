#' @include utils-pipe.R
#' @include adjust_time.R
#' @include climate-misc.R
#' @importFrom terra rast extract time
#' @importFrom tidyr gather separate spread nest unnest pivot_wider
#' @importFrom dplyr mutate select arrange lag group_by do slice mutate_at funs
#'   ungroup bind_rows rename n recode left_join mutate_all summarise tibble
#' @importFrom lubridate as_datetime force_tz hms year month hour as_date date
#'   days_in_month
#' @importFrom zoo na.spline
NULL

#' format ERA
#'
#' Description
#'
#' @param era5 data.frame. Data from ERA5-Land. See the corresponding vignette
#'   \code{vignette("climate", package = "rcontroll")} to download corresponding
#'   data from Copernicus in R.
#' @param tz num. Time zone. Can be obtained from the coordinates with
#'   [lutz::tz_lookup_coords()].
#'
#' @return A [data.frame()] with formatted ERA5-Land data for TROLL.
#'
#' @details
#'
#' Details on computations.
#'
#' @export
#'
format_era <- function(era5,
                       tz = "America/Cayenne") {
  # checks
  if (!inherits(era5, "data.frame")) {
    stop("era5 shoudl inherit from a data frame.")
  }
  if (!is.character(tz)) {
    stop("tz should be a string.")
  } else {
    if (!(tz %in% OlsonNames())) {
      stop("tz is not a correct time zone, use OlsonNames() to see available ones.") # nolint
    }
  }

  # tidytrick
  . <- snet <- temperature <- vpd <- ws <- rainfall <- NULL
  variable <- value <- u10 <- v10 <- tp <- t2m <- Snet <- NULL # nolint
  d2m <- sp <- Temperature <- VPD <- WS <- Rainfall <- ssr <- NULL # nolint

  # extrapolate
  time_freq <- 0.5 # TROLL v4
  start <- as_datetime(as_date(min(era5$time))) # to start from 00:30
  stop <- max(era5$time) # to stop at 00:00
  time <- seq(start, stop, by = 60 * 60 * time_freq)
  data <- left_join(tibble(time = time),
    era5,
    by = "time"
  ) %>%
    mutate_at(c("u10", "v10", "d2m", "t2m", "sp"),
      zoo::na.spline, .$time,
      na.rm = FALSE
    ) %>%
    mutate_at(c("ssr"),
      zoo::na.approx, .$time,
      na.rm = FALSE
    ) %>%
    mutate(ssr = ifelse(ssr < 0, 0, ssr))

  # tranform
  data <- data %>%
    mutate(ws = sqrt(u10^2 + v10^2)) %>% # formula WS
    mutate(rainfall = tp * 10^3 / 10) %>% # m to mm to cm
    mutate(temperature = t2m - 273.15) %>% # K to C
    mutate(snet = ssr / 3600 * 2) %>% # joul to watt
    mutate(snet = snet - lag(snet)) %>% # instateneous
    mutate(snet = ifelse(snet < 0,
      0, snet
    )) %>%
    mutate(vpd = dewtovpd(t2m, d2m, sp)) %>%
    select(time, temperature, snet, vpd, ws, rainfall) %>%
    adjust_time(tz = tz)

  return(data)
}
