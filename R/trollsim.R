#' @import methods
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
    species = "data.frame"
  ),
  prototype(
    name = character(),
    path = character(),
    parameters = numeric(),
    inputs = list(),
    log = character(),
    forest = data.frame(),
    ecosystem = data.frame(),
    species = data.frame()
  )
)

#' An S4 class to represent TROLL simulations
#'
#' This is an S4 class to represent TROLL simulations.
#'
#' @param name char. Simulation name.
#' @param path char. Path to the simulation.
#' @param parameters numeric. Parameters of the simulation (general inputs).
#' @param inputs list. Simulation inputs (species, climate, daily, forest,lidar).
#' @param log chr. Simulation log.
#' @param forest df. Simulation initial and final forest.
#' @param ecosystem df. Ecosystem metrics.
#' @param species df. Species metrics (with OUTPUT_extended option).
#'
#' @export
#' @rdname trollsim
trollsim <- function(
  name = character(),
  path = character(),
  parameters = numeric(),
  inputs = list(),
  log = character(),
  forest = data.frame(),
  ecosystem = data.frame(),
  species = data.frame()
) {
  return(new("trollsim",
             name = name,
             path = path,
             parameters = parameters,
             inputs = inputs,
             log = log,
             forest = forest,
             ecosystem = ecosystem,
             species = species
  ))
}
