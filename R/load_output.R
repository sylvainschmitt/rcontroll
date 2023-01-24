#' @include trollsim.R
#' @importFrom readr read_tsv cols read_file
#' @importFrom dplyr bind_rows n filter
#' @importFrom reshape2 melt dcast
#' @importFrom lidR readLAS LAS
NULL

#' Function to load outputs from a TROLL simulation.
#'
#' @param name char. Name given to the model output.
#' @param path char. Path where the model is saved.
#' @param thin int. Vector of integers corresponding to the iterations to be
#'   kept to reduce output size, default is NULL and corresponds to no
#'   thinning.
#'
#' @return An S4 \linkS4class{trollsim} class object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_output("test", "./")
#' }
#'
load_output <- function(name,
                        path,
                        thin = NULL) {
  # tidyverse
  iter <- NULL

  # Check inputs
  if (!all(unlist(lapply(list(name, path), class)) %in% c("character"))) {
    stop("name and path should be character.")
  }

  # @inputs
  inputs <- lapply(
    list(
      global = "global",
      species = "species",
      climate = "climate",
      daily = "daily"
    ),
    function(x) {
      read_tsv(file.path(path, paste0(name, paste0("_input_", x, ".txt"))),
               col_types = cols()
      )
    }
  )
  lidar_file <- file.path(path, paste0(name, paste0("_input_lidar.txt")))
  inputs$lidar <- data.frame()
  if (file.exists(lidar_file)) {
    inputs$lidar <- read_tsv(lidar_file, col_types = cols())
  }
  forest_file <- file.path(path, paste0(name, paste0("_input_forest.txt")))
  inputs$forest <- data.frame()
  if (file.exists(forest_file)) {
    inputs$forest <- read_tsv(forest_file, col_types = cols())
  }

  # @parameters
  parameters <- inputs$global$value
  names(parameters) <- inputs$global$param

  # @log
  log <- read_file(file.path(path, paste0(name, "_log.txt")))

  # @forest
  initial_pattern <- read_tsv(
    file.path(path, paste0(name, paste0("_0_initial_pattern.txt"))),
    col_types = cols()
  )
  final_pattern <- read_tsv(
    file.path(path, paste0(name, paste0("_0_final_pattern.txt"))),
    col_types = cols()
  )
  if (nrow(initial_pattern) > 0) {
    forest <- bind_rows(initial_pattern, final_pattern)
  } else {
    forest <- final_pattern
  }

  # @ecosystem
  ecosystem <- read_tsv(file.path(path,
                                  paste0(name, "_0_", "sumstats", ".txt")),
    col_types = cols()
  )
  if (!is.null(thin)) {
    ecosystem <- ecosystem %>%
      filter(iter %in% thin)
  }

  # @species
  species_file <- file.path(path,
                            paste0(name, "_0_", "sumstats_species", ".txt"))
  if (file.exists(species_file)) {
    species <- read_tsv(species_file,
      col_types = cols()
    )
    if (!is.null(thin)) {
      species <- species %>%
        filter(iter %in% thin)
    }
  } else {
    species <- data.frame()
  }

  # @las
  las_file <- file.path(path, paste0(name, "_0", "", ".las"))
  if (file.exists(las_file)) {
    file.copy(las_file, paste0(las_file, ".save.las"))
    las <- list(readLAS(file.path(las_file)))
  } else {
    las <- list()
  }
  
  return(
    trollsim(
      name = name,
      path = path,
      parameters = parameters,
      inputs = inputs,
      log = log,
      forest = forest,
      ecosystem = ecosystem,
      species = species,
      las = las
    )
  )
}
