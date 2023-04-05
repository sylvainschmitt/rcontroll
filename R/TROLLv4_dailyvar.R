#' TROLL daytime variation parameters
#'
#' Daytime variation parameters used by TROLL models from Paracou in 2014 &
#' 2015.
#'
#' @format A data frame with 24 rows and 5 variables: \describe{
#'   \item{DayJulian}{Julian day since 2014/1/1} \item{time_numeric}{Time in
#'   hour} \item{Temp}{Temperature} \item{Snet}{Absorbed short-wave radiation}
#'   \item{VPD}{Vapour pressure deficit} \item{WS}{Windspeed}}
#' 
"TROLLv4_dailyvar"

# TROLLv4_dailyvar <- readr::read_tsv("inst/extdata/TROLLv4_dailyvar.txt")
# usethis::use_data(TROLLv4_dailyvar, overwrite = T)
