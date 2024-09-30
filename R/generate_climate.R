#' @include utils-pipe.R
#' @importFrom terra rast extract time
#' @importFrom tidyr gather separate spread nest unnest
#' @importFrom dplyr mutate select arrange lag group_by do slice mutate_at funs
#'   ungroup bind_rows rename n recode left_join mutate_all summarise
#' @importFrom lubridate as_datetime force_tz hms year month hour as_date
#'   days_in_month
#' @importFrom stats decompose ts spline
#' @importFrom utils data
NULL

#' Generate climate dataset
#'
#' `TROLL` forest simulator relies on climate tables with half-hourly variations
#' of a typical day and monthly variations of a typical year which are recycled
#' through simulation days and years. Initially, `TROLL` climate tables were
#' computed from the Nouraflux dataset. Variations in quantities of interests
#' (temperatures, ...) were averaged to the target resolution (half-hour for
#' daily variation or month for monthly variation). The purpose of climate
#' generation functions is to compute equivalent climate tables from the ERA5
#' land reanalysis dataset (Muñoz-Sabater et al. 2021). With these functions,
#' `rcontroll` users only need inventories and associated functional traits to
#' run `TROLL` simulations. See the corresponding vignette
#' \code{vignette("climate", package = "rcontroll")} for further details.
#'
#' @param x num. Longitude in UTM. Can be obtained from the location name with
#'   [nominatimlite::geo_lite_sf()].
#' @param y num. Latitude in UTM. Can be obtained from the location name with
#'   [nominatimlite::geo_lite_sf()].
#' @param tz num. Time zone. Can be obtained from the coordinates with
#'   [lutz::tz_lookup_coords()].
#' @param era5land_hour str. Path to ERA5 land data monthly averaged reanalysis
#'   by hour of day in netCDF. See the corresponding vignette
#'   \code{vignette("climate", package = "rcontroll")} to download corresponding
#'   data from Copernicus in R.
#' @param era5land_month str. Path to ERA5 land data monthly averaged reanalysis
#'   in netCDF. See the corresponding vignette \code{vignette("climate", package
#'   = "rcontroll")} to download corresponding data from Copernicus in R.
#' @param daytime_start int. Daytime starting hour to compute nigh and day
#'   variables (default 7).
#' @param daytime_end int. Daytime ending hour to compute nigh and day variables
#'   (default 19).
#'
#' @return A list with two [data.frame()]: daytimevar and climatedaytime12.
#'
#' @details
#'
#' The `TROLL` forest model simulates tree growth based on ecophysiological
#' processes, with an external climate forcing. Input climatic conditions are
#' provided in the form of climate tables with (i) half-hourly standardised
#' variation of a typical day, and (ii) monthly average values of a typical
#' year, which are currently recycled through simulation. Initially, TROLL
#' climate tables were computed from the Nouraflux dataset (Poncy et al., 1998).
#' The variation in quantities of interest (irradiance, temperature, vapour
#' pressure, rainfall, and wind speed) were averaged to the target resolution
#' (half-hour for daily variation or month for monthly variation).
#'
#' The purpose of the climate generation function is to compute equivalent
#' climate tables from a global climatic reanalysis dataset. With
#' `generate_climate`, `rcontroll` users no longer need to format complex
#' climate input fields, but can generate them from global and carefully
#' documented climate distributions to run `TROLL` simulations. The selected
#' input climate product for this version of `rcontroll` is ERA5-Land
#' (Muñoz-Sabater et al. 2021). The ERA5-Land climate reanalysis has two main
#' advantages over other climate reanalysis products: (1) the data are at a
#' spatial resolution of 9km and have been available at hourly temporal
#' resolution since 1950, and (2) daily or monthly averages are available and
#' their uncertainties are reported.
#'
#' ## Hypotheses
#'
#' The following assumptions are made in the generation of climate data:
#'
#' 1. The temperature at 2m and its derivatives (`d2m`) from ERA5-land
#' corresponds to air temperature measurement used as an input in `TROLL`;
#'
#' 1. We can calculate the  vapour pressure deficit using the Buck equation;
#'
#' 1. The extraction of standardised half-hourly values of an average day and of
#' monthly average values of a year for the climate variables of interest is
#' based on the decomposition of the raw time series into: (i) an overall trend
#' over the study period, (ii) seasonal or daily variation across months or
#' hours depending on the study level, and (iii) the remaining variation;
#'
#' 1. Half-hourly values are not available for ERA5-land data. Spline functions
#' are used to interpolate hourly values for downscaling to half-hourly
#' resolution.
#'
#' ## Quantities of interest
#'
#' ### `TROLL` variables
#'
#' TROLL climate tables summarise temporal variation of quantities of interest.
#' These variations are called **seasonal pattern** and are computed from time
#' series under the additive assumption :
#'
#' \eqn{X(t) = Trend_X(t)+Seasonal_{x,Period}(t  \mod p [mod Period]) +
#' Irregular(t)}
#'
#' With :
#'
#' * \eqn{Trend_X(t)} a moving average covering one period;
#'
#' * \eqn{Seasonal_{x,Period}(t  \mod p [mod Period])} the seasonal pattern
#' contribution at t time modulo the period;
#'
#' * \eqn{Irregular(t)} the remainder part.
#'
#' The de-seasonally average of X is defined as :
#'
#' \eqn{mean(X)=mean(Trend_X(t))}
#'
#' These values of seasonal pattern and de-seasonally average are used to
#' compute the climate table [TROLLv3_climatedaytime12] and
#' [TROLLv3_daytimevar].
#'
#' ### ERA5-Land variables
#'
#' There is a restricted set of variables needed to generate the TROLL climate
#' files:
#'
#' * t2m: Temperature at 2m in K
#'
#' * d2m: Dew point temperature at 2m in K
#'
#' * tp: Cumulative rainfall in m
#'
#' * sp: Atmospheric pressure at the surface in Pa
#'
#' * ssrd: Cumulative Net solar irradiation in J/m2
#'
#' * u10: Zonal wind component at 10m in m/s
#'
#' * v10: Meridional wind component at 10m in m/s
#'
#' ## Calculation of variables
#'
#' The transition from ERA-Land data to `TROLL` data requires several
#' transformations. The `TROLL` climate files correspond to seasonal components,
#' either daily from 7am to 7pm, or monthly. The extraction of these seasonal
#' components is possible by analysing the time series of the data. An additive
#' decomposition of the variables allows one to obtain the pattern of interest
#' at the original resolution (hourly or monthly). Interpolation of the pattern
#' using spline functions of the periodic type, ensuring the boundary conditions
#' (i.e. value at 0 am is the same as 12 pm), except for the `ssrd` which is not
#' a continuous periodic function and which requires natural type spline
#' interpolation. An evaluation of the quality of the pattern extraction is
#' possible by measuring the standard deviation of the error to the original
#' time series. This error can be calculated for each unit of the pattern (for
#' each hour for example).
#'
#' ### Wind speed
#'
#' The wind speed is the norm of the vector generated by the u10 and v10
#' components of the wind. We can therefore deduce that the wind speed
#' corresponds to:
#'
#' \eqn{WindSpeed= \sqrt{u10^2+v10^2}}
#'
#' ### Vapour pressure deficit
#'
#' The calculation of the vapour pressure deficit can be done according to three
#' variables (`t2m`, `d2m` and `sp`) using the formula of (Buck 1981):
#'
#' \eqn{VPD =e_{sat}(d2m,sp)-e_{obst}(2m,sp)}
#'
#' \eqn{e_*(t2m|d2m,sp)=611.21xf(t2m|d2m,sp)x(1)}
#'
#' \eqn{(1)=18.678-(t2m|d2m-273.15)/234.5x(t2m|d2m-273.15)/
#' (240.97+t2m|d2m-273.15)}
#'
#' \eqn{f(t2m|d2m,sp)=1.0007+10^{-7}xspx0.032+5.9x10^{-6}x(t2m|d2m-273.15)^2}
#'
#' ### Instantaneous irradiance
#'
#' The measurement of irradiance in the ERA5 data corresponds to the
#' accumulation either at hourly or daily intervals. The instantaneous
#' measurement can be obtained by calculating the variation of this
#' accumulation:
#'
#' \eqn{\Delta ssrd(t)= ssrd(t)-ssrd(t-1)}
#'
#' ## Conclusion
#'
#' In conclusion, despite some discrepancies between climate input generated
#' from local meteorological station data and the `generate_climate` function
#' that should be investigated further, the `generate_climate` function allows
#' `rcontroll` users to easily obtain relevant climate data for their study. The
#' discrepancies may be partly due to the unconventional situation of the
#' Nouraflux station, which should not be considered as the true climate.
#'
#' **Warning**: As `TROLL` is under constant development, some of the variables
#' presented here may not be used in the current version (v 3.1.7) and may be
#' left over from previous versions or may be intended for future versions.
#' Furthermore, this supplementary information corresponds to the version 3.1.7
#' of `TROLL` and the climate variables used by the model may change as new
#' versions of `TROLL` are released. We plan to include future major
#' developments of `TROLL` in `rcontroll` to keep the advances of the model
#' accessible to the community, including the development of the
#' `generate_climate` function. We thus invite the reader to check the
#' corresponding updated vignette on GitHub
#' (https://sylvainschmitt.github.io/rcontroll/articles/climate.html) according
#' to the version of `TROLL` they are using in `rcontroll` (check with
#' [TROLL.version()]).
#'
#' ## References
#'
#' Buck, Arden L. (1981) New equations for computing vapor pressure and
#' enhancement factor. Journal of Applied Meteorology and Climatology, 1981,
#' vol. 20, no 12, p. 1527-1532.
#'
#' Muñoz-Sabater, J., Dutra, E., Agustí-Panareda, A., Albergel, C., Arduini, G.,
#' Balsamo, G., … Thépaut, J. N. (2021). ERA5-Land: A state-of-the-art global
#' reanalysis dataset for land applications. Earth System Science Data, 13(9),
#' 4349–4383. https://doi.org/10.5194/essd-13-4349-2021
#'
#' Poncy, O., Riéra, B., Larpin, D., Belbenoit, P., Jullien, M., Hoff, M., &
#' Charles-Dominique, P. (1998). The permanent field research station “Les
#' Nouragues” in the tropical rainforest of French Guiana: current projects and
#' preliminary results on tree diversity, structure, and dynamics. Forest
#' Biodiversity in North, Central and South America, and the Caribbean: Research
#' and Monitoring., 385–410.
#'
#' @export
#'
generate_climate <- function(x, y, tz,
                             era5land_hour,
                             era5land_month,
                             daytime_start = 7,
                             daytime_end = 19) {
  # tidytrick
  . <- DayTimeVapourPressureDeficitVPDbasic <- NULL # nolint
  DaytimeMeanIrradiance <- NULL # nolint
  DaytimeMeanTemperature <- DaytimeMeanVapourPressureDeficit <- NULL # nolint
  MeanIrradiance <- MeanIrradiance_Daytime <- NULL # nolint
  NightTemperature <- Rainfall <- NULL # nolint
  SaturatedVapourPressure <- Temperature <- Temperature_Daytime <- NULL # nolint
  Temperature_Night <- Timeperiod <- VaporPressureDeficit <- NULL # nolint
  VaporPressureDeficit_Daytime <- VapourPressure <- NULL # nolint
  VapourPressureDeficitVPDbasic_Daytime <- WindSpeed <- d2m <- NULL # nolint
  data <- daytimevalue <- NULL
  ddeg <- endtime <- monthlyvalue <- psat <- random <- seasonal <- NULL
  sp <- sp_trans <- NULL
  ssrd <- ssrd_trans <- starttime <- t2m <- tdeg <- NULL
  timestep <- tp <- NULL # nolint
  tp_trans <- NULL
  trend <- u10 <- v10 <- value <- NULL
  vardaytime_T <- vardaytime_light <- NULL # nolint
  vardaytime_vpd <- NULL
  variable <- vp <- vpd <- windspeed <- M <- NULL # nolint

  # hourly
  era5_hr_r <- suppressWarnings(rast(era5land_hour))
  era5_hr <- suppressWarnings(extract(era5_hr_r, cbind(x, y))) %>%
    gather("variable", "value") %>%
    mutate(date = as_datetime(terra::time(era5_hr_r))) %>%
    separate(variable, c("variable", "t"), sep = "_(?=\\d)") %>%
    select(-t) %>%
    separate(variable, c("variable", "expver"), sep = "_expver=") %>%
    group_by(date, variable) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    spread(variable, value) %>%
    arrange(date)
  rm(era5_hr_r)
  t0 <- t1 <- era5_hr$date[1]
  t1 <- force_tz(t1, tz)
  tlag <- t1 - as.POSIXct(t0)
  era5_hr$date <- era5_hr$date + hms("48:00:00")
  era5_hr$date <- era5_hr$date - tlag
  rm(t0, t1)
  era5_hr <- era5_hr %>%
    mutate(year = year(date)) %>%
    mutate(month = month(date)) %>%
    mutate(hour = hour(date)) %>%
    arrange(year, month, hour) %>%
    mutate(tdeg = t2m - 273.15) %>% # K to degree celcisus
    mutate(ddeg = d2m - 273.15) %>% # K to degree celcisus
    mutate(ssrd_trans = ssrd / 3600) %>% # joul to watt
    mutate(ssrd_trans = ssrd_trans - lag(ssrd_trans)) %>% # instateneous ssrd
    mutate(ssrd_trans = ifelse(ssrd_trans < 0,
      0, ssrd_trans
    )) %>%
    # negative after midnight to null
    mutate(vpd = .DewtoVPD(
      Tdewpoint = d2m - 273.15,
      Temp = tdeg, Pa = sp / 1000
    ) / 1000) %>%
    # Pa to kPa
    mutate(sp_trans = sp / 1000) # Pa to kPa
  era5_hr <- suppressWarnings(
    era5_hr %>%
      select(tdeg, ddeg, vpd, ssrd_trans, sp_trans) %>%
      gather(variable, value) %>%
      group_by(variable) %>%
      do(decompose(ts(.$value, frequency = 24),
                   type = "additive")[c("seasonal", "trend", "random")] %>%
           as.data.frame()) %>%
      group_by(variable) %>%
      mutate(M = mean(trend, na.rm = TRUE)) %>%
      slice(1:24) %>%
      mutate(timestep = 1:24) %>%
      mutate(value = seasonal + M) %>%
      select(-seasonal, -trend, -random, -M) %>%
      ungroup() %>%
      spread(variable, value)
  )
  tlag <- as.numeric(tlag) - 1
  daytimevar <- suppressWarnings(
    lapply(as.list(select(era5_hr, tdeg, ddeg, vpd, sp_trans)), function(x) {
      spline(seq(0, 23), x, xout = seq(0, 23.5, 0.5), method = "periodic") %>%
        as.data.frame()
    }) %>%
      bind_rows(.id = "variable") %>%
      spread(variable, y) %>%
      left_join(
        spline(seq((daytime_start - 1), (daytime_end + 1)),
          era5_hr$ssrd_trans[
            (daytime_start - 1 + tlag):(daytime_end + 1 + tlag)
          ],
          xout = seq((daytime_start - 1), daytime_end - 0.5, 0.5),
          method = "natural"
        ) %>%
          as.data.frame() %>%
          rename(ssrd_trans = y),
        by = "x"
      )
  ) %>%
    rename(starttime = x) %>%
    mutate(endtime = starttime + 0.5)
  rm(era5_hr, tlag)

  # monthly
  era5_mt_r <- suppressWarnings(rast(era5land_month))
  era5_mt <- suppressWarnings(extract(era5_mt_r, cbind(x, y))) %>%
    gather("variable", "value") %>%
    mutate(date = as_date(terra::time(era5_mt_r))) %>%
    separate(variable, c("variable", "t"), sep = "_(?=\\d)") %>%
    select(-t) %>%
    separate(variable, c("variable", "expver"), sep = "_expver=") %>%
    group_by(date, variable) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    spread(variable, value) %>%
    arrange(date) %>%
    mutate(month = month(date)) %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    filter(n() == 12) %>%
    ungroup() %>%
    arrange(year, month)
  rm(era5_mt_r)
  era5_mt <- era5_mt %>%
    mutate(tdeg = t2m - 273.15) %>% # K to degree celcisus
    mutate(ssrd_trans = ssrd / 86400) %>% # joul to watt
    mutate(ssrd_trans = ifelse(ssrd_trans < 0,
      0, ssrd_trans
    )) %>%
    # negative after midnight to null
    mutate(vpd = .DewtoVPD(
      Tdewpoint = d2m - 273.15,
      Temp = tdeg,
      Pa = sp / 1000
    ) / 1000) %>% # Pa to kPa
    mutate(tp_trans = tp * 100 * days_in_month(date)) %>%
    # m to mm * nb days in the month
    mutate(windspeed = sqrt(u10^2 + v10^2)) %>%
    mutate(vp = .esat(
      Temp = d2m - 273.15,
      Pa = sp / 1000
    ) / 1000) %>% # Pa to kPa
    mutate(psat = .esat(Temp = t2m - 273.15) / 1000) %>% # Pa to kPa
    mutate(ddeg = d2m - 273.15) %>% # K to degree celcisus
    mutate(sp_trans = sp / 1000) # Pa to kPa
  era5_mt <- suppressWarnings(
    era5_mt %>%
      select(
        tdeg, vpd, ssrd_trans, tp_trans, windspeed,
        vp, psat, ddeg, sp_trans
      ) %>%
      gather(variable, value) %>%
      group_by(variable) %>%
      do(decompose(ts(.$value, frequency = 12),
                   type = "additive")[c("seasonal", "trend", "random")] %>%
           as.data.frame()) %>%
      mutate(value = (seasonal + mean(trend + random, na.rm = TRUE))) %>%
      slice(1:12) %>%
      select(-seasonal, -trend, -random) %>%
      mutate(month = 1:12) %>%
      ungroup() %>%
      spread(variable, value, drop = FALSE) %>%
      rename(
        Temperature = tdeg, Rainfall = tp_trans, WindSpeed = windspeed,
        MeanIrradiance = ssrd_trans,
        SaturatedVapourPressure = psat, VapourPressure = vp,
        VaporPressureDeficit = vpd
      )
  )
  era5_mt_daynight <- suppressWarnings(
    era5_mt %>%
      select(
        month, Temperature, MeanIrradiance, VaporPressureDeficit,
        ddeg, sp_trans
      ) %>%
      gather(variable, monthlyvalue, -month) %>%
      left_join(
        daytimevar %>%
          select(starttime, ssrd_trans, vpd, tdeg, ddeg, sp_trans) %>%
          gather(variable, daytimevalue, -starttime) %>%
          mutate(variable = recode(variable,
            "tdeg" = "Temperature",
            "vpd" = "VaporPressureDeficit",
            "ssrd_trans" = "MeanIrradiance"
          )) %>%
          group_by(variable) %>%
          mutate(daytimevalue = ifelse(is.na(daytimevalue),
            0, daytimevalue
          )) %>%
          mutate(daytimevalue = daytimevalue / mean(daytimevalue)) %>%
          nest(),
        by = "variable"
      ) %>%
      unnest(cols = c(data)) %>%
      mutate(value = monthlyvalue * daytimevalue) %>%
      select(-monthlyvalue, -daytimevalue) %>%
      spread(variable, value) %>%
      mutate(
        VapourPressureDeficitVPDbasic =
          .DewtoVPD(ddeg, Temperature, sp_trans) / 1000
      ) %>%
      select(-ddeg, -sp_trans) %>%
      gather(variable, value, -month, -starttime) %>%
      mutate(Timeperiod = ifelse(starttime %in% daytime_start:daytime_end,
        "Daytime", "Night"
      )) %>%
      mutate(variable = paste0(variable, "_", Timeperiod)) %>%
      group_by(variable, month) %>%
      summarise(value = mean(value), .groups = "drop") %>%
      spread(variable, value) %>%
      select(
        month, Temperature_Daytime, Temperature_Night,
        MeanIrradiance_Daytime, VaporPressureDeficit_Daytime,
        VapourPressureDeficitVPDbasic_Daytime
      ) %>%
      rename(
        DaytimeMeanTemperature = Temperature_Daytime,
        NightTemperature = Temperature_Night,
        DaytimeMeanIrradiance = MeanIrradiance_Daytime,
        DaytimeMeanVapourPressureDeficit = VaporPressureDeficit_Daytime,
        DayTimeVapourPressureDeficitVPDbasic =
          VapourPressureDeficitVPDbasic_Daytime
      )
  )

  climatedaytime12 <- era5_mt %>%
    left_join(era5_mt_daynight, by = "month") %>%
    select(
      Temperature, DaytimeMeanTemperature, NightTemperature, Rainfall,
      WindSpeed, DaytimeMeanIrradiance, MeanIrradiance,
      SaturatedVapourPressure, VapourPressure,
      VaporPressureDeficit, DayTimeVapourPressureDeficitVPDbasic,
      DaytimeMeanVapourPressureDeficit
    )

  daytimevar <- suppressWarnings(daytimevar %>%
    rename(
      vardaytime_light = ssrd_trans, vardaytime_vpd = vpd,
      vardaytime_T = tdeg
    ) %>%
    select(
      starttime, endtime, vardaytime_light,
      vardaytime_vpd, vardaytime_T
    ) %>%
    filter(starttime %in% seq(daytime_start - 0.5, daytime_end - 1, 0.5)) %>%
    mutate_all(funs(ifelse(. < 0, 0, .))) %>%
    mutate_at(
      c("vardaytime_light", "vardaytime_vpd", "vardaytime_T"),
      funs(. / mean(.))
    ))

  return(list(
    daytimevar = daytimevar,
    climatedaytime12 = climatedaytime12
  ))
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
# @param Temp num. Temperature in celsius degrees.
# @param Pa num.  Atmospheric pressure in Pa, default 1 atmosphere.
#
# @return estimated vapour pressure in Pa
.esat <- function(Temp, # nolint
                  Pa = 101325) { # nolint
  a <- 611.21
  b <- 18.678 - (Temp / 234.5)
  c <- 257.14
  f <- 1.00072 + 10^-7 * Pa * (0.032 + 5.9 * 10^-6 * Temp^2)
  return(f * a * (exp(b * Temp / (c + Temp))))
}

# .DewtoVPD
#
# function to compute VPD from temperature, dewpoint temperature & surface
# pressure
#
# from Jones, H.G. 2014. Plants and microclimate: a quantitative approach to
# environmental plant physiology. 3nd Edition., 2nd Edn. Cambridge University
# Press, Cambridge. 428 p.
#
# @param Tdewpoint num. Temperature from dewpoint in celsius degrees.
# @param Temp num. Temperature in celsius degrees.
# @param Pa num.  Atmospheric pressure in Pa, default 1 atmosphere.
#
# @return estimated vapour pressure in Pa
.DewtoVPD <- function(Tdewpoint, # nolint
                      Temp, # nolint
                      Pa = 101325) { # nolint
  e <- .esat(Tdewpoint, Pa) # actual vapor pressure
  esatval <- .esat(Temp) # saturated:
  return((esatval - e)) # in Pa
}
