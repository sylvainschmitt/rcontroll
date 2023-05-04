#' rh_to_vpd
#'
#' Compute vapour pressure deficit from relative humidity
#'
#' from Jones, H.G. 2014. Plants and microclimate: a quantitative approach to
#' environmental plant physiology. 3nd Edition., 2nd Edn. Cambridge University
#' Press, Cambridge. 428 p.
#'
#' @param humidity num. Relative humidity in %.
#' @param temperature num. Temperature in celsius degrees.
#' @param pressure num.  Atmospheric pressure in Pa, default 1 atmosphere.
#'
#' @return Estimated vapour pressure in Pa
#'
#' @examples
#' rh_to_vpd(90, 27)
#'
#' @export
rh_to_vpd <- function(humidity,
                      temperature,
                      pressure = 101) {
  esatval <- esat(humidity, pressure)
  e <- (humidity / 100) * esatval
  vpd <- (esatval - e) / 1000
  return(vpd)
}

#' esat
#'
#' Compute vapour pressure from temperature & surface pressure
#'
#' from Jones, H.G. 2014. Plants and microclimate: a quantitative approach to
#' environmental plant physiology. 3nd Edition., 2nd Edn. Cambridge University
#' Press, Cambridge. 428 p.
#'
#' @param temperature num. Temperature in celsius degrees.
#' @param pressure num.  Atmospheric pressure in Pa, default 1 atmosphere.
#'
#' @return Estimated vapour pressure in Pa
#'
#' @examples
#' esat(27)
#'
#' @export
esat <- function(temperature,
                 pressure = 101325) {
  a <- 611.21
  b <- 18.678 - (temperature / 234.5)
  c <- 257.14
  f <- 1.00072 + 10^-7 * pressure * (0.032 + 5.9 * 10^-6 * temperature^2)
  return(f * a * (exp(b * temperature / (c + temperature))))
}


#' dewtovpd
#'
#' function to compute VPD from temperature, dewpoint temperature & surface
#' pressure
#'
#' from Jones, H.G. 2014. Plants and microclimate: a quantitative approach to
#' environmental plant physiology. 3nd Edition., 2nd Edn. Cambridge University
#' Press, Cambridge. 428 p.
#'
#' @param temperature num. Temperature in Kelvin.
#' @param dewpoint num. Temperature in Kelvin.
#' @param pressure num.  Atmospheric pressure in Pa, default 1 atmosphere.
#'
#' @return Estimated vapour pressure in Pa
#'
#' @examples
#' dewtovpd(27 + 273.15, 25 + 273.15)
#'
#' @export
dewtovpd <- function(temperature,
                     dewpoint,
                     pressure = 101325) {
  dewpoint <- dewpoint - 273.15
  temperature <- temperature - 273.15
  pressure <- pressure / 1000
  e <- esat(dewpoint, pressure) # actual vapor pressure
  esatval <- esat(temperature) # saturated
  vpd <- esatval - e
  vpd <- vpd / 1000
  return(vpd)
}
