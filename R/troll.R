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
#' @param path char. Path to save the simulation outputs, the default is null
#'   corresponding to a simulation in memory without saved intermediary files.
#' @param global df. Global parameters.
#' @param species df. Species parameters.
#' @param climate df. Climate parameters.
#' @param daily df. Daily variation parameters.
#' @param pedology df. Daily variation parameters.
#' @param forest df. TROLL with forest input, if null starts from an empty grid
#'   (default NULL).
#' @param soil df. TROLL with soil input, if null starts from an empty grid
#'   (default NULL).
#' @param lidar df. Lidar simulation parameters, if null no computed (default
#'   NULL).
#' @param verbose bool. Show TROLL outputs in the console.
#' @param load bool. TROLL outputs are loaded in R memory, if not only the path
#'   to the outputs is kept.
#' @param overwrite bool. Overwrite previous outputs.
#' @param thin int. Vector of integers corresponding to the iterations to be
#'   kept to reduce output size, default is NULL and corresponds to no thinning.
#' @param date char. Starting date as YYYY/MM/DD, default NULL will result in
#'   non-dated outputs.
#'
#' @return A trollsim object.
#'
#' @export
#'
#' @examples
#' \donttest{
#' data("TROLLv4_input")
#' data("TROLLv4_species")
#' data("TROLLv4_climate")
#' data("TROLLv4_dailyvar")
#' data("TROLLv4_pedology")
#' sim <- troll(
#'   name = "test",
#'   path = getwd(),
#'   global = generate_parameters(nbiter = 10),
#'   species = TROLLv4_species,
#'   climate = TROLLv4_climate,
#'   daily = TROLLv4_dailyvar,
#'   pedology = TROLLv4_pedology,
#'   load = TRUE,
#'   date = "2004/01/01"
#' )
#' }
#' 
troll <- function(name = NULL,
                  path = NULL,
                  global,
                  species,
                  climate,
                  daily,
                  pedology,
                  forest = NULL,
                  soil = NULL,
                  lidar = NULL,
                  verbose = TRUE,
                  load = TRUE,
                  overwrite = TRUE,
                  thin = NULL,
                  date = NULL) {
  i <- NULL
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
      pedology = pedology,
      forest = forest,
      soil = soil,
      lidar = lidar,
      verbose = verbose,
      load = load,
      overwrite = overwrite,
      thin = thin,
      date = date
    )
  }
  stopCluster(cl)
  return(sim[[1]])
}

.troll_child <- function(name = NULL,
                         path = NULL,
                         global,
                         species,
                         climate,
                         daily,
                         pedology,
                         forest = NULL,
                         soil = NULL,
                         lidar = NULL,
                         verbose = TRUE,
                         load = TRUE,
                         overwrite = TRUE,
                         thin = NULL,
                         date = NULL) {
  # check all inputs
  if (!all(unlist(lapply(list(verbose, load, overwrite), 
                         class)) == "logical")) {
    stop("verbose, load, and overwrite should be logical.")
  }
  if (!all(unlist(lapply(list(name, path), class)) %in% c("character",
                                                          "NULL"))) {
    stop("name and path should be character or null.")
  }
  if (!all(unlist(lapply(list(global, species, climate, daily, pedology),
                         inherits, c("data.frame", "NULL"))))) {
    stop("global, species, climate, and daily should be a data frame.")
  }
  if (!(class(forest) %in% c("data.frame", "NULL"))) {
    stop("forest should be a data frame or null.")
  }
  if (!(class(soil) %in% c("data.frame", "NULL"))) {
    stop("forest should be a data frame or null.")
  }
  if (!(class(lidar) %in% c("data.frame", "NULL"))) {
    stop("lidar should be a data frame or null.")
  }
  if (!(class(date) %in% c("character", "NULL"))) {
    stop("date should be character or null.")
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
  if(tmp && !load) {
    stop("You can not unactivate the load option if you have not defined a path for your files.")
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
  pedology_path <- file.path(path, name, paste0(name, "_input_pedology.txt"))

  if (!is.null(forest)) {
    forest_path <- file.path(path, name, paste0(name, "_input_forest.txt"))
  }
  
  if (!is.null(soil)) {
    soil_path <- file.path(path, name, paste0(name, "_input_soil.txt"))
  }

  if (!is.null(lidar)) {
    lidar_path <- file.path(path, name, paste0(name, "_input_lidar.txt"))
  }

  write_tsv(global, file = global_path)
  write_tsv(species, file = species_path)
  write_tsv(climate, file = climate_path)
  write_tsv(daily, file = daily_path)
  write_tsv(pedology, file = pedology_path)
  if (!is.null(forest)) {
    write_tsv(forest, file = forest_path)
  }
  if (is.null(forest)) {
    forest_path <- ""
  }
  if (!is.null(soil)) {
    write_tsv(soil, file = soil_path, col_names = FALSE)
  }
  if (is.null(soil)) {
    soil_path <- ""
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
      pedology_file = pedology_path,
      forest_file = forest_path,
      soil_file = soil_path,
      lidar_file = lidar_path,
      output_file = file.path(path, name, name)
    ),
    split = verbose
  )
  write(log, file.path(path, name, paste0(name, "_log.txt")))

  # cleaning outputs
  # lapply(list(
  #   "info",
  # ), function(x) {
  #   unlink(file.path(path, name, paste0(name, "_0_", x, ".txt")))
  # })

  # loading outputs
  sim <- trollsim(name = name, path = file.path(path, name), mem = FALSE)
  if(load) {
    sim <- load_sim(sim)
    if(!is.null(date)){
      sim <- date_sim(sim, date)
    }
  }
  if (tmp) {
    unlink(file.path(path, name), recursive = TRUE, force = TRUE)
    sim@path <- character()
  }
  
  return(sim)
}
