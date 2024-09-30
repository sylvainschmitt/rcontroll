#' `TROLL` daytime variation parameters
#'
#' Daytime variation parameters used by `TROLL`s models.
#'
#' @format A data frame with 24 rows and 5 variables: \describe{
#'   \item{starttime}{starting time} \item{endtime}{ending time}
#'   \item{vardaytime_light}{daily variation in irradiance relative to the mean
#'   value of the day} \item{vardaytime_vpd}{daily variation in vapour pressure
#'   deficit relative to the mean value of the day} \item{vardaytime_T}{daily
#'   variation in temperature relative to the mean value of the day}}
#'
#' @seealso [generate_climate()], [TROLLv3_climatedaytime12]
#'
"TROLLv3_daytimevar"
