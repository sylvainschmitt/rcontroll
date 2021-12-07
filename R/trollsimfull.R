#' @include trollsim.R
#' @import methods
NULL

#' An S4 class to represent TROLL simulations
#'
#' This is an S4 class to represent TROLL simulations with full outputs.
#'
#' @slot species_outputs df. species and ecosystem metrics
#'
#' @export
setClass(
  "trollsimfull",
  contains = 'trollsim',
  representation(
    species_outputs = "data.frame"
  ),
  prototype(
    species_outputs = data.frame()
  )
)

#' An S4 class to represent TROLL simulations
#'
#' This is an S4 class to represent TROLL simulations.
#'
#' @param name char. simulation name
#' @param path char. path to the simulation
#' @param parameters numeric. parameters of the simulation (general inputs, random, forest)
#' @param inputs list. simulation inputs (species, climate, daily, forest)
#' @param log chr. simulation log
#' @param final_pattern df. simulation final forest
#' @param species_outputs df. species and ecosystem metrics
#'
#' @export
#' @rdname trollsimfull
trollsimfull <- function(
  name = character(),
  path = character(),
  parameters = numeric(),
  inputs = list(),
  log = character(),
  final_pattern = data.frame(),
  species_outputs = data.frame()
) {
  return(new("trollsimfull",
             name = name,
             path = path,
             parameters = parameters,
             inputs = inputs,
             log = log,
             final_pattern = final_pattern,
             species_outputs = species_outputs
  ))
}
