#' @include trollsim.R
#' @import methods
NULL

#' An S4 class to represent TROLL stack
#'
#' This is an S4 class to represent TROLL stack.
#'
#' @export
setClass(
  "trollstack",
  contains = 'trollsim'
)

#' An S4 class to represent TROLL stack
#'
#' This is an S4 class to represent TROLL stack.
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
#' @rdname trollstack
trollstack <- function(
  name = character(),
  path = character(),
  parameters = numeric(),
  inputs = list(),
  log = character(),
  forest = data.frame(),
  ecosystem = data.frame(),
  species = data.frame()
) {
  return(new("trollstack",
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
