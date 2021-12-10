#' @import methods
NULL

#' An S4 class to represent TROLL simulations
#'
#' This is an S4 class to represent TROLL simulations.
#'
#' @slot name char. Simulation name.
#' @slot path char. Path to the simulation.
#' @slot parameters numeric. Parameters of the simulation (general inputs).
#' @slot inputs list. Simulation inputs (species, climate, daily, forest).
#' @slot log chr. Simulation log.
#' @slot final_pattern df. Simulation final forest.
#' @slot outputs df. Species and ecosystem metrics (reduced or full).
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
    final_pattern = "data.frame",
    outputs = "data.frame"
  ),
  prototype(
    name = character(),
    path = character(),
    parameters = numeric(),
    inputs = list(),
    log = character(),
    final_pattern = data.frame(),
    outputs = data.frame()
  )
)

#' An S4 class to represent TROLL simulations
#'
#' This is an S4 class to represent TROLL simulations.
#'
#' @param name char. Simulation name.
#' @param path char. Path to the simulation.
#' @param parameters numeric. Parameters of the simulation (general inputs).
#' @param inputs list. Simulation inputs (species, climate, daily, forest).
#' @param log chr. Simulation log.
#' @param final_pattern df. Simulation final forest.
#' @param outputs df. Species and ecosystem metrics (reduced or full).
#'
#' @export
#' @rdname trollsim
trollsim <- function(
  name = character(),
  path = character(),
  parameters = numeric(),
  inputs = list(),
  log = character(),
  final_pattern = data.frame(),
  outputs = data.frame()
) {
  return(new("trollsim",
             name = name,
             path = path,
             parameters = parameters,
             inputs = inputs,
             log = log,
             final_pattern = final_pattern,
             outputs = outputs
  ))
}
