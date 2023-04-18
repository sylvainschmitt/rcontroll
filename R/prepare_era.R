#' @include utils-pipe.R
#' @importFrom terra rast extract time
#' @importFrom tidyr gather separate spread nest unnest pivot_wider
#' @importFrom dplyr mutate select arrange lag group_by do slice mutate_at funs
#'   ungroup bind_rows rename n recode left_join mutate_all summarise
#' @importFrom lubridate as_datetime force_tz hms year month hour as_date date
#'   days_in_month
NULL

#' Prepare ERA
#'
#' Description
#'
#' @param tz num. Time zone. Can be obtained from the coordinates with
#'   [lutz::tz_lookup_coords()].
#' @param era5 str. Path to ERA5 land data in netCDF. See the corresponding
#'   vignette \code{vignette("climate", package = "rcontroll")} to download
#'   corresponding data from Copernicus in R.
#'
#' @return A [data.frame()] with...
#'
#' @details
#'
#' Details on computations.
#'
#' @export
#'
prepare_era <- function(era5,
                        tz = "America/Cayenne") {
  warning("Need to add arguments check")

  # tidytrick
  variable <- value <- u10 <- v10 <- tp <- t2m <- Snet <- NULL # nolint
  d2m <- sp <- Temperature <- VPD <- WS <- Rainfall <- ssr <- NULL # nolint

  # read
  era5_r <- suppressWarnings(rast(era5))
  era5_t <- suppressWarnings(as.data.frame(era5_r)) %>%
    gather("variable", "value") %>%
    mutate(time = rep(as_datetime(terra::time(era5_r)))) %>%
    separate(variable, c("variable", "t"), sep = "_(?=\\d)") %>%
    select(-t) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    arrange(time)


  # variables
  era5_t <- era5_t %>%
    mutate(WS = sqrt(u10^2 + v10^2)) %>% # formula WS
    mutate(Rainfall = tp * 10^3) %>% # m to mm
    mutate(Temperature = t2m - 273.15) %>% # K to C
    mutate(Snet = ssr / 3600) %>% # joul to watt
    mutate(Snet = Snet - lag(Snet)) %>% # instateneous
    mutate(Snet = ifelse(Snet < 0,
      0, Snet
    )) %>%
    mutate(VPD = .dewtovpd(d2m, t2m, sp)) %>%
    select(time, Temperature, Snet, VPD, WS, Rainfall)

  # date
  d0 <- date(era5_t$time[1])
  t0 <- t1 <- era5_t$time[1]
  t1 <- force_tz(t1, tz)
  tlag <- t1 - as.POSIXct(t0)
  era5_t$time <- era5_t$time - tlag
  era5_t <- filter(era5_t, date(time) > (d0 - 1))
  rm(t0, t1, tlag)

  return(era5_t)
}


# Internals #

# .esat
#
# function to compute vapour pressure from temperature & surface pressure
#
# from Jones, H.G. 2014. Plants and microclimate: a quantitative approach to
# environmental plant physiology. 3nd Edition., 2nd Edn. Cambridge University
# Press, Cambridge. 428 p.
#
# @param temperature num. Temperature in celsius degrees.
# @param pressure num.  Atmospheric pressure in Pa, default 1 atmosphere.
#
# @return estimated vapour pressure in Pa
.esat <- function(temperature,
                  pressure = 101325) {
  a <- 611.21
  b <- 18.678 - (temperature / 234.5)
  c <- 257.14
  f <- 1.00072 + 10^-7 * pressure * (0.032 + 5.9 * 10^-6 * temperature^2)
  return(f * a * (exp(b * temperature / (c + temperature))))
}

# .dewtovpd
#
# function to compute VPD from temperature, dewpoint temperature & surface
# pressure
#
# from Jones, H.G. 2014. Plants and microclimate: a quantitative approach to
# environmental plant physiology. 3nd Edition., 2nd Edn. Cambridge University
# Press, Cambridge. 428 p.
#
# @param d2m num. Temperature from dewpoint in K.
# @param t2m num. Temperature in K.
# @param sp num.  Atmospheric pressure in Pa, default 1 atmosphere.
#
# @return estimated vapour pressure in Pa.
.dewtovpd <- function(d2m,
                      t2m,
                      sp = 101325) {
  d2m <- d2m - 273.15
  t2m <- t2m - 273.15
  sp <- sp / 1000
  e <- .esat(d2m, sp) # actual vapor pressure
  esatval <- .esat(t2m) # saturated
  vpd <- esatval - e
  vpd <- vpd / 1000
  return(vpd)
}
