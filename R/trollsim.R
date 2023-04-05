#' @import methods
#' @importFrom lidR LAS
NULL

#' An S4 class to represent TROLL simulations
#'
#' This is an S4 class to represent TROLL simulations.
#'
#' @slot name char. Simulation name.
#' @slot path char. Path to the simulation.
#' @slot parameters numeric. Parameters of the simulation (general inputs).
#' @slot inputs list. Simulation inputs (species, climate, daily, forest,lidar).
#' @slot log chr. Simulation log.
#' @slot forest df. Simulation initial and final forest.
#' @slot ecosystem df. Ecosystem metrics.
#' @slot species df. Species metrics (with OUTPUT_extended option).
#' @slot las list. List with one simulated point cloud in LAS from lidar
#'   parameters (with lidar option). The LAS format correspond to lidr::LAS.
#'
#' @return An empty S4 \linkS4class{trollsim} class object.
#' 
#' @export
setClass(
  "trollsim",
  representation(
    name = "character",
    path = "character",
    parameters = "numeric",
    inputs = "list",
    log = "character",
    forest = "data.frame",
    ecosystem = "data.frame",
    species = "data.frame",
    las = "list"
  ),
  prototype(
    name = character(),
    path = character(),
    parameters = numeric(),
    inputs = list(),
    log = character(),
    forest = data.frame(),
    ecosystem = data.frame(),
    species = data.frame(),
    las = list()
  )
)

#' An S4 class to represent TROLL simulations
#'
#' This is an S4 class to represent TROLL simulations.
#'
#' @param name char. Simulation name.
#' @param path char. Path to the simulation.
#' @param parameters numeric. Parameters of the simulation (general inputs).
#' @param inputs list. Simulation inputs (species, climate, daily,
#'   forest,lidar).
#' @param log chr. Simulation log.
#' @param forest df. Simulation initial and final forest.
#' @param ecosystem df. Ecosystem metrics.
#' @param species df. Species metrics (with OUTPUT_extended option).
#' @param las list. List with one simulated point cloud in LAS from lidar
#'   parameters (with lidar option). The LAS format correspond to lidr::LAS.
#'   
#' @return An empty S4 \linkS4class{trollsim} class object.
#'
#' @export
#' @rdname trollsim
trollsim <- function(name = character(),
                     path = character(),
                     parameters = numeric(),
                     inputs = list(),
                     log = character(),
                     forest = data.frame(),
                     ecosystem = data.frame(),
                     species = data.frame(),
                     las = list()) {
  return(new("trollsim",
    name = name,
    path = path,
    parameters = parameters,
    inputs = inputs,
    log = log,
    forest = forest,
    ecosystem = ecosystem,
    species = species,
    las = las
  ))
}
