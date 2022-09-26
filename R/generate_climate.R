#' @include utils-pipe.R
#' @importFrom terra rast extract time
#' @importFrom tidyr gather separate spread nest unnest
#' @importFrom dplyr mutate select arrange lag group_by do slice mutate_at funs ungroup bind_rows rename n recode left_join mutate_all summarise
#' @importFrom lubridate as_datetime force_tz hms year month hour as_date days_in_month
#' @importFrom stats decompose ts spline
#' @importFrom utils data
NULL

#' Generate climate dataset
#'
#' TROLL forest simulator relies on climate tables with half-hourly variations
#' of a typical day and monthly variations of a typical year which are recycled
#' through simulation days and years. Initially, TROLL climate tables were
#' computed from the Nouraflux dataset. Variations in quantities of interests
#' (temperatures, ...) were averaged to the target resolution (half-hour for
#' daily variation or month for monthly variation). The purpose of climate
#' generation functions is to compute equivalent climate tables from the ERA5
#' land reanalysis dataset. With these functions, rcontroll users only need
#' inventories and associated functional traits to run TROLL simulations. See
#' the corresponding vignette for further details.
#'
#' @param x num. Longitude in UTM. Can be obtained from the location name with
#'   the function geo_lite_sf from the package nominatimlite.
#' @param y num. Latitude in UTM. Can be obtained from the location name with
#'   the function geo_lite_sf from the package nominatimlite.
#' @param tz num. Time zone. Can be obtained from the coordinates with the
#'   function tz_lookup_coords from the package lutz.
#' @param era5land_hour str. Path to ERA5 land data monthly averaged reanalysis
#'   by hour of day in netCDF. See the corresponding vignette to download
#'   corresponding data from Copernicus in R.
#' @param era5land_month str. Path to ERA5 land data monthly averaged reanalysis
#'   in netCDF. See the corresponding vignette to download corresponding data
#'   from Copernicus in R.
#' @param daytime_start int. Daytime starting hour to compute nigh and day
#'   variables (default 7).
#' @param daytime_end int. Daytime ending hour to compute nigh and day variables
#'   (default 19).
#'
#' @return A list with two tables: daytimevar and climatedaytime12.
#'
#' @export
#' 
generate_climate <- function(x, y, tz,
                             era5land_hour, 
                             era5land_month,
                             daytime_start = 7, 
                             daytime_end = 19
                             ){
  # tidytrick
  . <- DayTimeVapourPressureDeficitVPDbasic <- DaytimeMeanIrradiance <- NULL
  DaytimeMeanTemperature <- DaytimeMeanVapourPressureDeficit <- NULL
  MeanIrradiance <- MeanIrradiance_Daytime <- NightTemperature <- Rainfall <- NULL
  SaturatedVapourPressure <- Temperature <- Temperature_Daytime <- NULL
  Temperature_Night <- Timeperiod <- VaporPressureDeficit <- NULL
  VaporPressureDeficit_Daytime <- VapourPressure <- NULL
  VapourPressureDeficitVPDbasic_Daytime <- WindSpeed <- d2m <- data <- daytimevalue <- NULL
  ddeg <- endtime <- monthlyvalue <- psat <- random <- seasonal <- sp <- sp_trans <- NULL
  ssrd <- ssrd_trans <- starttime <- t2m <- tdeg <- timestep <- tp <- tp_trans <- NULL
  trend <- u10 <- v10 <- value <- vardaytime_T <- vardaytime_light <- vardaytime_vpd <- NULL
  variable <- vp <- vpd <- windspeed <- NULL
  
  # hourly
  era5_hr_r <- rast(era5land_hour)
  era5_hr <- extract(era5_hr_r, cbind(x,y)) %>% 
    gather("variable", "value") %>% 
    separate(variable, c("variable", "t"), sep = "_(?=\\d)") %>% 
    mutate(date = as_datetime(time(era5_hr_r)[as.numeric(t)])) %>% 
    spread(variable, value) %>% 
    select(-t) %>% 
    arrange(date)
  rm(era5_hr_r)
  t0 <- t1 <- era5_hr$date[1]
  t1 <- force_tz(t1, tz)
  tlag <- t1 - t0
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
    mutate(ssrd_trans = ssrd/3600) %>%  # joul to watt
    mutate(ssrd_trans = ssrd_trans - lag(ssrd_trans)) %>% # instateneous ssrd
    mutate(ssrd_trans = ifelse(ssrd_trans < 0, 0, ssrd_trans)) %>%  # negative after midnight to null
    mutate(vpd = .DewtoVPD(Tdewpoint = d2m - 273.15, Temp = tdeg, Pa = sp/1000)/1000) %>%  # Pa to kPa
    mutate(sp_trans = sp/1000) # Pa to kPa
  era5_hr <- suppressWarnings(
    era5_hr %>% 
      select(tdeg, ddeg, vpd, ssrd_trans, sp_trans) %>% 
      gather(variable, value) %>% 
      group_by(variable) %>% 
      do(decompose(ts(.$value,frequency=24),type = "additive")[c("seasonal", "trend", "random")] %>% 
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
    lapply(as.list(select(era5_hr, tdeg, ddeg, vpd, sp_trans)), function(x)
      spline(seq(0,23), x, xout = seq(0,23.5,0.5), method = "periodic") %>% 
        as.data.frame()) %>%  bind_rows(.id = "variable") %>% 
    spread(variable, y) %>% 
    left_join(
      spline(seq((daytime_start-1),(daytime_end+1)), 
             era5_hr$ssrd_trans[(daytime_start-1+tlag):(daytime_end+1+tlag)],
             xout = seq((daytime_start-1), daytime_end - 0.5,0.5), method = "natural") %>% 
        as.data.frame() %>%  
        rename(ssrd_trans = y), by = "x"
    )) %>% 
    rename(starttime = x) %>% 
    mutate(endtime = starttime + 0.5)
  rm(era5_hr, tlag)

  # monthly
  era5_mt_r <- rast(era5land_month)
  era5_mt <- extract(era5_mt_r, cbind(x,y)) %>% 
    gather("variable", "value") %>% 
    separate(variable, c("variable", "t"), sep = "_(?=\\d)") %>% 
    mutate(date = as_date(terra::time(era5_mt_r)[as.numeric(t)])) %>% 
    spread(variable, value) %>% 
    select(-t) %>% 
    mutate(month = month(date)) %>% 
    mutate(year = year(date)) %>% 
    group_by(year) %>% 
    filter(n() == 12) %>% 
    ungroup() %>% 
    arrange(year, month)
  rm(era5_mt_r)
  era5_mt <- era5_mt %>% 
    mutate(tdeg = t2m - 273.15) %>% # K to degree celcisus
    mutate(ssrd_trans = ssrd/86400) %>%  # joul to watt
    mutate(ssrd_trans = ifelse(ssrd_trans < 0, 0, ssrd_trans)) %>%  # negative after midnight to null
    mutate(vpd = .DewtoVPD(Tdewpoint = d2m - 273.15, Temp = tdeg, Pa = sp/1000)/1000) %>%  # Pa to kPa
    mutate(tp_trans = tp*100*days_in_month(date)) %>%  # m to mm * nb days in the month
    mutate(windspeed = sqrt(u10^2 + v10^2)) %>% 
    mutate(vp = .esat(Temp = d2m - 273.15, Pa = sp/1000)/1000) %>%  # Pa to kPa
    mutate(psat = .esat(Temp = t2m - 273.15)/1000) %>% # Pa to kPa
    mutate(ddeg = d2m - 273.15) %>% # K to degree celcisus
    mutate(sp_trans = sp/1000) # Pa to kPa
  era5_mt <- suppressWarnings(
    era5_mt %>% 
    select(tdeg, vpd, ssrd_trans, tp_trans, windspeed, vp, psat, ddeg, sp_trans) %>% 
    gather(variable, value) %>% 
    group_by(variable) %>% 
    do(decompose(ts(.$value,frequency=12),type = "additive")[c("seasonal", "trend", "random")] %>% 
         as.data.frame()) %>% 
    mutate(value = (seasonal + mean(trend + random, na.rm = T))) %>% 
    slice(1:12) %>% 
    select(-seasonal, -trend, -random) %>% 
    mutate(month = 1:12) %>% 
    ungroup() %>% 
    spread(variable, value, drop = FALSE) %>% 
    rename(Temperature = tdeg, Rainfall = tp_trans, WindSpeed = windspeed, MeanIrradiance = ssrd_trans,
           SaturatedVapourPressure = psat, VapourPressure = vp, VaporPressureDeficit = vpd)
  )
  era5_mt_daynight <- suppressWarnings(
    era5_mt %>% 
      select(month, Temperature, MeanIrradiance, VaporPressureDeficit, ddeg, sp_trans) %>% 
      gather(variable, monthlyvalue, -month) %>% 
      left_join(
        daytimevar %>% 
          select(starttime, ssrd_trans, vpd, tdeg, ddeg, sp_trans) %>% 
          gather(variable, daytimevalue, -starttime) %>% 
          mutate(variable = recode(variable, "tdeg" = "Temperature",
                                   "vpd" = "VaporPressureDeficit",
                                   "ssrd_trans" = "MeanIrradiance")) %>% 
          group_by(variable) %>% 
          mutate(daytimevalue = ifelse(is.na(daytimevalue), 0, daytimevalue)) %>% 
          mutate(daytimevalue = daytimevalue/mean(daytimevalue)) %>% 
          nest(), by = "variable"
      ) %>% 
      unnest(cols = c(data)) %>% 
      mutate(value = monthlyvalue*daytimevalue) %>%
      select(-monthlyvalue, -daytimevalue) %>% 
      spread(variable, value) %>% 
      mutate(VapourPressureDeficitVPDbasic = .DewtoVPD(ddeg, Temperature, sp_trans)/1000) %>%
      select(-ddeg, -sp_trans) %>% 
      gather(variable, value, -month, -starttime) %>% 
      mutate(Timeperiod = ifelse(starttime %in% daytime_start:daytime_end, "Daytime", "Night")) %>% 
      mutate(variable = paste0(variable, "_", Timeperiod)) %>% 
      group_by(variable, month) %>% 
      summarise(value = mean(value)) %>% 
      spread(variable, value) %>% 
      select(month, Temperature_Daytime, Temperature_Night, MeanIrradiance_Daytime, VaporPressureDeficit_Daytime, VapourPressureDeficitVPDbasic_Daytime) %>% 
      rename(DaytimeMeanTemperature = Temperature_Daytime, 
             NightTemperature = Temperature_Night, 
             DaytimeMeanIrradiance = MeanIrradiance_Daytime, 
             DaytimeMeanVapourPressureDeficit = VaporPressureDeficit_Daytime,
             DayTimeVapourPressureDeficitVPDbasic = VapourPressureDeficitVPDbasic_Daytime)
  )
  
  climatedaytime12 <- era5_mt %>% 
    left_join(era5_mt_daynight, by = "month") %>% 
    select(Temperature, DaytimeMeanTemperature, NightTemperature, Rainfall, WindSpeed,
           DaytimeMeanIrradiance, MeanIrradiance,	SaturatedVapourPressure,	VapourPressure,
           VaporPressureDeficit,	DayTimeVapourPressureDeficitVPDbasic,	DaytimeMeanVapourPressureDeficit)
  
  daytimevar <- suppressWarnings(daytimevar %>% 
                                   rename(vardaytime_light = ssrd_trans, vardaytime_vpd = vpd, vardaytime_T = tdeg) %>% 
                                   select(starttime, endtime, vardaytime_light, vardaytime_vpd, vardaytime_T) %>% 
                                   filter(starttime %in% seq(daytime_start - 0.5, daytime_end - 1, 0.5)) %>% 
                                   mutate_all(funs(ifelse(. < 0, 0, .))) %>% 
                                   mutate_at(c("vardaytime_light", "vardaytime_vpd", "vardaytime_T"), funs(./mean(.)))
                                 )
  
  return(list(daytimevar = daytimevar,
              climatedaytime12 = climatedaytime12))
}


# Internals #

# .esat
#
# function to compute vapour pressure from temperature & surface pressure
#
# from Jones, H.G. 2014. Plants and microclimate: a quantitative approach to environmental plant physiology.
# 3nd Edition., 2nd Edn. Cambridge University Press, Cambridge. 428 p.
#
# @param Temp num. Temperature in celsius degrees.
# @param Pa num.  Atmospheric pressure in Pa, default 1 atmosphere.
#
# @return estimated vapour pressure in Pa
#
.esat <- function(Temp, Pa = 101325){
  a <- 611.21
  b <- 18.678 - (Temp/234.5)
  c <- 257.14
  f <- 1.00072 + 10^-7 * Pa * (0.032 + 5.9 * 10^-6 * Temp^2)
  return( f * a * (exp(b * Temp/(c + Temp))))
}

# .DewtoVPD
#
# function to compute VPD from temperature, dewpoint temperature & surface pressure
#
# from Jones, H.G. 2014. Plants and microclimate: a quantitative approach to environmental plant physiology.
# 3nd Edition., 2nd Edn. Cambridge University Press, Cambridge. 428 p.
#
# @param Tdewpoint num. Temperature from dewpoint in celsius degrees.
# @param Temp num. Temperature in celsius degrees.
# @param Pa num.  Atmospheric pressure in Pa, default 1 atmosphere.
#
# @return estimated vapour pressure in Pa
#
.DewtoVPD <- function(Tdewpoint, Temp, Pa = 101325){
  e <- .esat(Tdewpoint, Pa) # actual vapor pressure
  esatval <- .esat(Temp)  # saturated:
  return((esatval - e)) # in Pa
}
