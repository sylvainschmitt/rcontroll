#' @import methods
#' @importFrom lidR LAS
NULL

#' A `TROLL` simulations
#'
#' `trollsim()` is an S4 class to represent a `TROLL` simulation. `trollsim`
#' values can be accessed using object attributes in the form of simple R
#' objects (with `@`). They consist of eight simulation attributes: (1) name,
#' (2) path to saved files, (3) parameters, (4) inputs, (5) log, (6) initial and
#' final state, (7) ecosystem output metrics, and (8) species output metrics.
#' The initial and final states are represented by a table with the spatial
#' position, size and other relevant traits of all trees at the start and end of
#' the simulation. The ecosystem and species metrics are summaries of ecosystem
#' processes and states, such as net primary production and aboveground biomass,
#' and they are documented at species level and aggregated over the entire
#' stand.
#'
#' @slot name char. Simulation name.
#' @slot path char. File path to the simulation.
#' @slot mem bool. Is the simulation in memory, see [load_sim()].
#' @slot parameters numeric. Parameters of the simulation (general inputs).
#' @slot inputs list. Simulation inputs (species, climate, daily, forest, lidar,
#'   see [stack()]).
#' @slot log chr. Simulation log, see [get_log()].
#' @slot forest df. Simulation initial and final forest, see [get_forest()].
#' @slot ecosystem df. Ecosystem metrics.
#' @slot species df. Species metrics (with OUTPUT_extended option, see
#'   [generate_parameters()]).
#' @slot las list. List with simulated point cloud in LAS from lidar parameters
#'   (with lidar option, see [generate_lidar()]). The LAS format correspond to
#'   [lidR::LAS()].
#'
#' @return An empty S4 [trollsim()] class object.
#'
#' @seealso [troll()], [load_output()], [trollstack()]
#'
#' @export
setClass(
  "trollsim",
  representation(
    name = "character",
    path = "character",
    mem = "logical",
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
    mem = logical(),
    parameters = numeric(),
    inputs = list(),
    log = character(),
    forest = data.frame(),
    ecosystem = data.frame(),
    species = data.frame(),
    las = list()
  )
)

#' A `TROLL` simulations
#'
#' `trollsim()` is an S4 class to represent a `TROLL` simulation. `trollsim`
#' values can be accessed using object attributes in the form of simple R
#' objects (with `@`). They consist of eight simulation attributes: (1) name,
#' (2) path to saved files, (3) parameters, (4) inputs, (5) log, (6) initial and
#' final state, (7) ecosystem output metrics, and (8) species output metrics.
#' The initial and final states are represented by a table with the spatial
#' position, size and other relevant traits of all trees at the start and end of
#' the simulation. The ecosystem and species metrics are summaries of ecosystem
#' processes and states, such as net primary production and aboveground biomass,
#' and they are documented at species level and aggregated over the entire
#' stand.
#'
#' @param name char. Simulation name.
#' @param path char. File path to the stack of simulation (parent folder).
#' @param mem bool. Is the simulation in memory, see [load_sim()].
#' @param parameters numeric. Parameters of the simulation (general inputs).
#' @param inputs list. Simulation inputs (species, climate, daily, forest,
#'   lidar, see [troll()]).
#' @param log chr. Simulation log, see [get_log()].
#' @param forest df. Simulation initial and final forest, see [get_forest()].
#' @param ecosystem df. Ecosystem metrics.
#' @param species df. Species metrics (with OUTPUT_extended option, see
#'   [generate_parameters()]).
#' @param las list. List with simulated point cloud in LAS from lidar parameters
#'   (with lidar option, see [generate_lidar()]). The LAS format correspond to
#'   [lidR::LAS()].
#'
#' @return An empty S4 [trollsim()] class object.
#'
#' @seealso [troll()], [load_output()], [trollstack()]
#'
#' @export
#' @rdname trollsim
trollsim <- function(name = character(),
                     path = character(),
                     mem = logical(),
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
    mem = mem,
    parameters = parameters,
    inputs = inputs,
    log = log,
    forest = forest,
    ecosystem = ecosystem,
    species = species,
    las = las
  ))
}
