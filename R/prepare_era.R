#' Prepare ERA
#'
#' Description
#'
#' @param era5 str. Path to ERA5 land data in netCDF. See the corresponding
#'   vignette \code{vignette("climate", package = "rcontroll")} to download
#'   corresponding data from Copernicus in R.
#' @param tz num. Time zone. Can be obtained from the coordinates with
#'   [lutz::tz_lookup_coords()].
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
  data <- extract_era(era5)
  data <- format_era(data, tz = tz)
  return(data)
}

#' Prepare ERA batch
#'
#' Description
#'
#' @param era5_files vector. Vector of paths to ERA5 land data in netCDF. See
#'   the corresponding vignette \code{vignette("climate", package =
#'   "rcontroll")} to download corresponding data from Copernicus in R.
#' @param tz num. Time zone. Can be obtained from the coordinates with
#'   [lutz::tz_lookup_coords()].
#' @param cores int. Number of cores for parallelization, if NULL available
#'   cores - 1 (default NULL).
#'
#' @return A [data.frame()] with...
#'
#' @details
#'
#' Details on computations.
#'
#' @export
#'
prepare_era_batch <- function(era5_files,
                              tz = "America/Cayenne",
                              cores = NULL) {
  data <- extract_era_batch(
    era5_files = era5_files,
    cores = cores
  )
  data <- format_era(data, tz = tz)
  return(data)
}
