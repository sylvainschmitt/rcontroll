#' @include load_output.R
#' @importFrom readr write_tsv
#' @importFrom sys exec_wait
#' @importFrom utils timestamp capture.output
NULL

#' troll
#'
#' Run a TROLL simulation.
#'
#' @param name char. Model name (if NULL timestamp).
#' @param path char. Path to save the simulations outputs, the default is null
#'   corresponding to a simulation in memory without saved intermediary files.
#' @param global df. Global parameters.
#' @param species df. Species parameters.
#' @param climate df. Climate parameters.
#' @param daily df. Daily variation parameters.
#' @param forest df. TROLL with forest input, if null start from an empty grid
#'   (default NULL).
#' @param verbose bool. Show TROLL outputs in the console.
#' @param overwrite bool. Overwrite previous outputs.
#' @param thin int. Vector of integers corresponding to the iterations to be
#'   kept to reduce outputs size, default is NULL and corresponds to no
#'   thinning.
#'
#' @return A trollsim object.
#'
#' @export
#'
#' @examples
#'
#' data("TROLLv3_species")
#' data("TROLLv3_climatedaytime12")
#' data("TROLLv3_daytimevar")
#' troll(name = "test",
#'       global = generate_parameters(cols = 100, rows = 100,
#'                                    iterperyear = 12, nbiter = 12*1),
#'       species = TROLLv3_species,
#'       climate = TROLLv3_climatedaytime12,
#'       daily = TROLLv3_daytimevar)
#' 
troll <- function(name = NULL,
                  path = NULL,
                  global,
                  species,
                  climate,
                  daily,
                  forest = NULL,
                  verbose = TRUE,
                  overwrite = TRUE,
                  thin = NULL) {
  i <- NULL
  cl <- makeCluster(1, outfile = "")
  registerDoSNOW(cl)
  sim <- foreach(i=1, .export = ".troll_child") %dopar%
    .troll_child(
      name = name,
      path = path,
      global = global,
      species = species,
      climate = climate,
      daily = daily,
      forest = forest,
      verbose = verbose,
      overwrite = overwrite,
      thin = thin)
  stopCluster(cl)
  return(sim[[1]])
}

.troll_child <- function(name = NULL,
                         path = NULL,
                         global,
                         species,
                         climate,
                         daily,
                         forest = NULL,
                         verbose = TRUE,
                         overwrite = TRUE,
                         thin = NULL) {
  
  # check all inputs
  if(!all(unlist(lapply(list(overwrite), class)) == "logical"))
    stop("overwrite should be logical.")
  if(!all(unlist(lapply(list(name, path), class)) %in% c("character", "NULL")))
    stop("name and path should be character or null.")
  if(!all(unlist(lapply(list(global, species, climate, daily), inherits, c("data.frame", "NULL")))))
    stop("global, species, climate, and daily should be a data frame.")
  if(!(class(forest) %in% c("data.frame", "NULL")))
    stop("forest should be a data frame or null.")
  
  # model name
  if (is.null(name)) {
    name <- paste0(
      "sim_",
      gsub(":", "-",
           gsub(
             " ", "_",
             timestamp(
               prefix = "",
               suffix = "",
               quiet = T
             )
           ))
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
  
  # save input as files
  global_path <- file.path(path, name, paste0(name, "_input_global.txt"))
  species_path <- file.path(path, name, paste0(name, "_input_species.txt"))
  climate_path <- file.path(path, name, paste0(name, "_input_climate.txt"))
  daily_path <- file.path(path, name, paste0(name, "_input_daily.txt"))
  if(!is.null(forest))
    forest_path <- file.path(path, name, paste0(name, "_input_forest.txt"))
  write_tsv(global, file = global_path)
  write_tsv(species, file = species_path)
  write_tsv(climate, file = climate_path)
  write_tsv(daily, file = daily_path)
  if(!is.null(forest))
    write_tsv(forest, file = forest_path)
  if(is.null(forest))
    forest_path <- "NULL"
  
  # run
  log <- capture.output(
    trollCpp(global_file = global_path,
             climate_file = climate_path,
             species_file = species_path,
             day_file = daily_path,
             forest_file = forest_path,
             output_file = file.path(path, name, name)
    ),
    split = verbose)
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
    # "CHM",
    "death",
    "deathrate",
    "death_snapshots",
    # "LAI",
    "ppfd0",
    "sdd",
    "vertd"
  ), function(x) {
    unlink(file.path(path, name, paste0(name, "_0_", x, ".txt")))
  })
  
  # loading outputs
  sim <- load_output(name, file.path(path, name), thin = thin)
  if (tmp) {
    unlink(file.path(path, name), recursive = TRUE, force = TRUE)
    sim@path <- character()
  }
  
  return(sim)
}
