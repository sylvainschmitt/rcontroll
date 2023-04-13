#' @include load_output.R
#' @importFrom readr write_tsv
#' @importFrom sys exec_wait
#' @importFrom utils timestamp capture.output
NULL

#' Run a `TROLL` simulation
#'
#' `troll()` run a `TROLL` simulation. The minimal set of input files required
#' for a `TROLL` run include (i) climate data for the focal location (`climate`
#' and `daily`), (ii) functional traits for the list of species at the focal
#' location (`species`), and (iii) global parameters (`global`), i.e. parameters
#' that do not depend on species identity.
#'
#' @param name char. Model name (if NULL the timestamp will be used).
#' @param path char. Path to save the simulation outputs, the default is null
#'   corresponding to a simulation in memory without saved intermediary files
#'   (based on temporary files from [option.rcontroll]).
#' @param global df. Global parameters (e.g. [TROLLv3_input] or using
#'   [generate_parameters()]).
#' @param species df. Species parameters (e.g. [TROLLv3_species]).
#' @param climate df. Climate parameters (e.g. [TROLLv3_climatedaytime12]).
#' @param daily df. Daily variation parameters (e.g. [TROLLv3_daytimevar]).
#' @param lidar df. Lidar simulation parameters (e.g. using [generate_lidar()]),
#'   if null not computed (default NULL).
#' @param forest df. TROLL with forest input, if null starts from an empty grid
#'   (default NULL) (e.g. using [TROLLv3_output] with [get_forest()]).
#' @param load bool. TROLL outputs are loaded in R memory, if not only the path
#'   and name of the simulations is kept in the resulting [trollsim()] object
#'   but the content can be accessed later using the [load_sim()] method.
#' @param verbose bool. Show TROLL log in the console.
#' @param overwrite bool. Overwrite previous outputs folder and files.
#' @param thin int. Vector of integers corresponding to the iterations to be
#'   kept to reduce output size, default is NULL and corresponds to no thinning.
#'
#' @return A [trollsim()] object.
#'
#' @seealso [stack()]
#'
#' @export
#'
#' @examples
#' \donttest{
#' data("TROLLv3_species")
#' data("TROLLv3_climatedaytime12")
#' data("TROLLv3_daytimevar")
#' troll(
#'   name = "test",
#'   global = generate_parameters(
#'     cols = 100, rows = 100,
#'     iterperyear = 12, nbiter = 12 * 1
#'   ),
#'   species = TROLLv3_species,
#'   climate = TROLLv3_climatedaytime12,
#'   daily = TROLLv3_daytimevar
#' )
#' }
#'
troll <- function(name = NULL,
                  path = NULL,
                  global,
                  species,
                  climate,
                  daily,
                  lidar = NULL,
                  forest = NULL,
                  load = TRUE,
                  verbose = TRUE,
                  overwrite = TRUE,
                  thin = NULL) {
  i <- NULL # nolint
  cl <- makeCluster(1, outfile = "")
  registerDoSNOW(cl)
  sim <- foreach(i = 1, .export = ".troll_child") %dopar% {
    .troll_child(
      name = name,
      path = path,
      global = global,
      species = species,
      climate = climate,
      daily = daily,
      lidar = lidar,
      forest = forest,
      load = load,
      verbose = verbose,
      overwrite = overwrite,
      thin = thin
    )
  }
  stopCluster(cl)
  return(sim[[1]])
}

.troll_child <- function(name = NULL, # nolint
                         path = NULL,
                         global,
                         species,
                         climate,
                         daily,
                         lidar = NULL,
                         forest = NULL,
                         load = TRUE,
                         verbose = TRUE,
                         overwrite = TRUE,
                         thin = NULL) {
  # check all inputs
  if (!all(unlist(lapply(
    list(verbose, load, overwrite),
    class
  )) == "logical")) {
    stop("verbose, load, and overwrite should be logical.")
  }
  if (!all(unlist(lapply(list(name, path), class)) %in% c(
    "character",
    "NULL"
  ))) {
    stop("name and path should be character or null.")
  }
  if (!all(unlist(lapply(
    list(global, species, climate, daily),
    inherits, c("data.frame", "NULL")
  )))) {
    stop("global, species, climate, and daily should be a data frame.")
  }
  if (!(class(forest) %in% c("data.frame", "NULL"))) {
    stop("forest should be a data frame or null.")
  }
  if (!(class(lidar) %in% c("data.frame", "NULL"))) {
    stop("lidar should be a data frame or null.")
  }

  # model name
  if (is.null(name)) {
    name <- paste0(
      "sim_",
      gsub(
        ":", "-",
        gsub(
          " ", "_",
          timestamp(
            prefix = "",
            suffix = "",
            quiet = TRUE
          )
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
  if (tmp && !load) {
    stop("You can not unactivate the load option if you have not defined a path for your files.") # nolint
  }
  if (!is.null(path)) {
    path <- normalizePath(path)
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

  # save input as files
  global_path <- file.path(path, name, paste0(name, "_input_global.txt"))
  species_path <- file.path(path, name, paste0(name, "_input_species.txt"))
  climate_path <- file.path(path, name, paste0(name, "_input_climate.txt"))
  daily_path <- file.path(path, name, paste0(name, "_input_daily.txt"))

  if (!is.null(forest)) {
    forest_path <- file.path(path, name, paste0(name, "_input_forest.txt"))
  }

  if (!is.null(lidar)) {
    lidar_path <- file.path(path, name, paste0(name, "_input_lidar.txt"))
  }

  write_tsv(global, file = global_path)
  write_tsv(species, file = species_path)
  write_tsv(climate, file = climate_path)
  write_tsv(daily, file = daily_path)
  if (!is.null(forest)) {
    write_tsv(forest, file = forest_path)
  }
  if (is.null(forest)) {
    forest_path <- ""
  }
  if (!is.null(lidar)) {
    write_tsv(lidar, file = lidar_path)
  }
  if (is.null(lidar)) {
    lidar_path <- ""
  }

  # run
  log <- capture.output(
    trollCpp(
      global_file = global_path,
      climate_file = climate_path,
      species_file = species_path,
      day_file = daily_path,
      lidar_file = lidar_path,
      forest_file = forest_path,
      output_file = file.path(path, name, name)
    ),
    split = verbose
  )
  write(log, file.path(path, name, paste0(name, "_log.txt")))

  # cleaning outputs
  lapply(list(
    "info",
    "abc_biomass",
    "abc_chm",
    "abc_chmALS",
    "abc_ground",
    "abc_species",
    "abc_species10",
    "abc_traitconservation",
    "abc_traits",
    "abc_traits10",
    "abc_transmittance",
    "abc_transmittanceALS",
    "CHM",
    "death",
    "deathrate",
    "death_snapshots",
    "LAI",
    "ppfd0",
    "sdd",
    "vertd"
  ), function(x) {
    unlink(file.path(path, name, paste0(name, "_0_", x, ".txt")))
  })

  # loading outputs
  sim <- trollsim(name = name, path = file.path(path, name), mem = FALSE)
  if (load) {
    sim <- load_sim(sim)
  }
  if (tmp) {
    unlink(file.path(path, name), recursive = TRUE, force = TRUE)
    sim@path <- character()
  }

  return(sim)
}
