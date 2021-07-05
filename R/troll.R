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
#' @param troll_path chr. path to troll executable (default in the package)
#' @param troll_exe chr. name of troll executable (TROLL_abc.out,
#'   TROLL_full.out, TROLL_full_forest.out, TROLL_reduced.out,
#'   TROLL_reduced_forest.out)
#' @param global df. global parameters
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
                  troll_path = NULL,
                  troll_exe = "TROLL_full.out",
                  global = data.frame(),
                  species = data.frame(),
                  climate = data.frame(),
                  daily = data.frame(),
                  overwrite = TRUE,
                  verbose = TRUE) {

  # check all inputs

  # model name
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

  # model path
  tmp <- FALSE
  if (is.null(path)) {
    path <- getOption("rcontroll.tmp")
    tmp <- TRUE
  }
  if (name %in% list.dirs(path, full.names = FALSE)[-1]) {
    if (!overwrite) {
      stop("Outputs already exist, use overwrite = TRUE.")
    }
    path_o <- file.path(path, name)
    unlink(path_o, recursive = TRUE)
  } else {
    path_o <- file.path(path, name)
  }
  dir.create(path_o)

  # troll exe
  if (is.null(troll_path)) {
    os <- .Platform$OS.type
    troll_path <- system.file("troll", os, package = "rcontroll")
  }
  troll <- file.path(troll_path, troll_exe)

  # save input as files
  global_path <- file.path(path, name, paste0(name, "_input_global.txt"))
  species_path <- file.path(path, name, paste0(name, "_input_species.txt"))
  climate_path <- file.path(path, name, paste0(name, "_input_climate.txt"))
  daily_path <- file.path(path, name, paste0(name, "_input_daily.txt"))
  write_tsv(global, file = global_path)
  write_tsv(species, file = species_path)
  write_tsv(climate, file = climate_path)
  write_tsv(daily, file = daily_path)

  # command
  command <- paste0(
    troll,
    " -i", global_path,
    " -s", species_path,
    " -m", climate_path,
    " -d", daily_path,
    " -o", file.path(path, name, name)
  )
  if (verbose) {
    message(command)
  }

  # run
  log <- system(command, intern = TRUE)
  write(log, file.path(path, name, paste0(name, "_log.txt")))

  # outputs
  lapply(list(
    "100yearsofsolitude",
    "cica",
    "dbh",
    "death", # not using it for the moment
    "death1",
    "death2",
    "death3",
    "deathrate", # not using it for the moment
    "final_pattern", # not using it for the moment
    "info",
    "leafdens1",
    "leafdens2",
    "leafdens3",
    "NDDfield",
    "par",
    "site1",
    "ppfd0", # not using it for the moment
    "site2",
    "site3",
    "site4",
    "site5",
    "site6",
    "sp_par",
    "state",
    "vertd" # not using it for the moment
  ), function(x) {
    unlink(file.path(path, name, paste0(name, "_0_", x, ".txt")))
  })
  sim <- load_output(name, file.path(path, name))
  if (tmp) {
    unlink(file.path(path, name), recursive = TRUE, force = TRUE)
  }

  return(sim)
}
