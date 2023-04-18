#' @include utils-pipe.R
#' @include prepare_era.R
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach %dopar%
#' @importFrom iterators iter
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom dplyr bind_rows
NULL

#' Prepare ERA batch
#'
#' Description
#'
#' @param tz num. Time zone. Can be obtained from the coordinates with
#'   [lutz::tz_lookup_coords()].
#' @param era5_files vector. Vector of paths to ERA5 land data in netCDF. See
#'   the corresponding vignette \code{vignette("climate", package =
#'   "rcontroll")} to download corresponding data from Copernicus in R.
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
                              cores = NULL,
                              tz = "America/Cayenne") {
  warning("Need to add arguments check")

  # cores
  if (is.null(cores)) {
    cores <- detectCores()
    message("Detect cores was not defined, ", cores, " cores will be used.")
  }
  if ((detectCores()) < cores) {
    cores <- detectCores()
    warning(paste(
      "It seems you attributed more cores than your CPU has!
      Automatic reduction to",
      cores, "cores."
    ))
  }

  file <- NULL
  cl <- makeCluster(cores, outfile = "")
  registerDoSNOW(cl)
  pb <- txtProgressBar(max = length(era5_files), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  era5_t <- foreach(
    file = iter(era5_files),
    .export = "prepare_era",
    .options.snow = opts
  ) %dopar% {
    prepare_era(file, tz = tz)
  }
  close(pb)
  stopCluster(cl)
  era5_t <- bind_rows(era5_t)

  return(era5_t)
}
