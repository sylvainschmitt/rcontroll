#' @include trollsim.R
#' @importFrom readr read_tsv cols read_file
#' @importFrom dplyr bind_rows n filter left_join starts_with
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
      daily = "daily",
      pedology = "pedology"
    ),
    function(x) {
      read_tsv(file.path(path, paste0(name, paste0("_input_", x, ".txt"))),
               col_types = cols()
      )
    }
  )
  forest_file <- file.path(path, paste0(name, paste0("_input_forest.txt")))
  inputs$forest <- data.frame()
  if (file.exists(forest_file)) {
    inputs$forest <- read_tsv(forest_file, col_types = cols())
  }
  soil_file <- file.path(path, paste0(name, paste0("_input_soil.txt")))
  inputs$soil <- data.frame()
  if (file.exists(soil_file)) {
    inputs$soil <- read_tsv(soil_file, col_types = cols())
  }
  lidar_file <- file.path(path, paste0(name, paste0("_input_lidar.txt")))
  inputs$lidar <- data.frame()
  if (file.exists(lidar_file)) {
    inputs$lidar <- read_tsv(lidar_file, col_types = cols())
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

  # @soil
  soil_properties <- read_tsv(file.path(path,
                                        paste0(name, "_0_", "soilproperties", ".txt")),
                              col_types = cols()
  )
  swc <- read_tsv(file.path(path,
                            paste0(name, "_0_", "final_SWC3D", ".txt")),
                   col_names = c("dcell", 1:nrow(inputs$pedology)),
                   col_types = cols()
  ) %>% melt("dcell", variable.name = "layer", value.name = "swc") %>% 
    mutate(layer = as.numeric(layer)-1)
  soil <- left_join(swc, soil_properties, by = "layer")
  
  # @ecosystem
  ecosystem <- read_tsv(file.path(path,
                                  paste0(name, "_0_", "sumstats", ".txt")),
                        col_types = cols()
  )
  lai <- read_tsv(file.path(path,
                            paste0(name, "_0_", "LAIdynamics", ".txt")),
                  col_types = cols()
  )
  ecosystem_full <- left_join(ecosystem, lai, by = "iter")
  roots <- read_tsv(file.path(path,
                              paste0(name, "_0_", "phi_root", ".txt")),
                    col_types = cols()
  )
  ecosystem_full <- left_join(ecosystem_full, roots, by = "iter")
  water <- read_tsv(file.path(path,
                              paste0(name, "_0_", "water_balance", ".txt")),
                    col_types = cols()
  ) %>% select(-starts_with("SW"))
  if(nrow(water) > 0) {
    ecosystem_full <- left_join(ecosystem_full, water, by = "iter")
  }
  if (!is.null(thin)) {
    ecosystem_full <- ecosystem_full %>%
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
      mem = TRUE,
      parameters = parameters,
      inputs = inputs,
      log = log,
      forest = forest,
      soil = soil,
      ecosystem = ecosystem_full,
      species = species,
      las = las
    )
  )
}
