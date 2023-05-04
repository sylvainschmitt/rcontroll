#' @include utils-pipe.R
#' @importFrom terra rast extract time
#' @importFrom tidyr gather separate spread nest unnest pivot_wider
#' @importFrom dplyr mutate select arrange lag group_by do slice mutate_at funs
#'   ungroup bind_rows rename n recode left_join mutate_all summarise
#' @importFrom lubridate as_datetime force_tz hms year month hour as_date date
#'   days_in_month
#' @importFrom iterators iter
NULL

#' Extract ERA
#'
#' Description
#'
#' @param era5 str. Path to ERA5 land data in netCDF. See the corresponding
#'   vignette \code{vignette("climate", package = "rcontroll")} to download
#'   corresponding data from Copernicus in R.
#'
#' @return A [data.frame()] with ERA5-Land data.
#'
#' @details
#'
#' Details on computations.
#'
#' @export
#'
extract_era <- function(era5) {
  # checks
  if (!is.character(era5)) {
    stop("era5 should be a string.")
  }

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

  return(era5_t)
}

#' Extract ERA batch
#'
#' Description
#'
#' @param era5_files vector. Vector of paths to ERA5 land data in netCDF. See
#'   the corresponding vignette \code{vignette("climate", package =
#'   "rcontroll")} to download corresponding data from Copernicus in R.
#' @param cores int. Number of cores for parallelization, if NULL available
#'   cores - 1 (default NULL).
#'
#' @return A [data.frame()] with ERA5-Land data..
#'
#' @details
#'
#' Details on computations.
#'
#' @export
#'
extract_era_batch <- function(era5_files,
                              cores = NULL) {
  iter <- NULL

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

  # checks
  if (!is.character(era5_files)) {
    stop("era5_files should be a string.")
  }

  file <- NULL
  cl <- makeCluster(cores, outfile = "")
  registerDoSNOW(cl)
  pb <- txtProgressBar(max = length(era5_files), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  era5_t <- foreach(
    file = iter(era5_files),
    .export = "extract_era",
    .options.snow = opts
  ) %dopar% {
    extract_era(file)
  }
  close(pb)
  stopCluster(cl)
  era5_t <- bind_rows(era5_t)

  return(era5_t)
}
