#' @include load_output.R
#' @importFrom readr write_tsv
#' @importFrom sys exec_wait
#' @importFrom utils timestamp
NULL

#' TROLL
#'
#' Run a TROLL simulation
#'
#' @param name char. model name
#' @param path char. path to the simulation (tmp if NULL)
#' @param general df. general parameters
#' @param troll chr. path to troll executable (default in the package)
#' @param species df. species parameters
#' @param climate df. climate parameters
#' @param daily df. daily variation parameters
#' @param overwrite bool. overwrite previous outputs
#' @param verbose bool. display models information
#'
#' @return simulation outputs in the path folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' example
#' }
#'
troll <- function(name = NULL,
                  path = NULL,
                  troll = NULL,
                  # troll = system.file("troll",
                  #                     "TROLL.out",
                  #                     package = "RconTroll",
                  #                     mustWork = TRUE),
                  general = data.frame(),
                  species = data.frame(),
                  climate = data.frame(),
                  daily = data.frame(),
                  overwrite = TRUE,
                  verbose = TRUE) {

  # check all inputs with internal
  if (is.null(name)) {
    name <- paste0(
      "sim_",
      gsub(
        " ", "_",
        timestamp(
          prefix = "",
          suffix = "",
          quiet = T
        )
      )
    )
  }
  if (is.null(path)) {
    path <- "."
  } # to be improved with tmp using zzz.R

  # creating folder
  if (name %in% list.dirs(path, full.names = FALSE)[-1]) {
    if (!overwrite) {
      stop("Outputs already exist, use overwrite = T.")
    }
    path_o <- file.path(path, name)
    unlink(path_o, recursive = TRUE)
  } else {
    path_o <- file.path(path, name)
  }
  dir.create(path_o)

  # save input as files
  general_path <- file.path(path, name, paste0(name, "_input_general.txt"))
  species_path <- file.path(path, name, paste0(name, "_input_species.txt"))
  climate_path <- file.path(path, name, paste0(name, "_input_climate.txt"))
  daily_path <- file.path(path, name, paste0(name, "_input_daily.txt"))
  write_tsv(general, file = general_path)
  write_tsv(species, file = species_path)
  write_tsv(climate, file = climate_path)
  write_tsv(daily, file = daily_path)

  # command
  troll <- "/home/sylvain/Documents/ECOFOG/rcontroll/inst/troll/TROLL.out"
  output <- file.path(path, name, name)
  command <- paste0(
    troll,
    " -i", general_path,
    " -s", species_path,
    " -m", climate_path,
    " -d", daily_path,
    " -o", output
  )
  if (verbose) {
    message(command)
  }

  # run
  log <- system(command, intern = TRUE)
  write(log, file.path(path, name, paste0(name, "_log.txt")))

  # outputs
  sim <- loadOutput(name, file.path(path, name))
  # remove or not tmp file depending on pars
  return(sim)
}
