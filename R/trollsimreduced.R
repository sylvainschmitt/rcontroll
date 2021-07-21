#' @include trollsim.R
#' @import methods
NULL

#' An S4 class to represent TROLL simulations
#'
#' This is an S4 class to represent TROLL simulations with reduced outputs.
#'
#' @slot reduced_outputs df. ecosystem metrics
#'
#' @export
setClass(
  "trollsimreduced",
  contains = 'trollsim',
  representation(
    reduced_outputs = "data.frame"
  ),
  prototype(
    reduced_outputs = data.frame()
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
#' @param reduced_outputs df. ecosystem metrics
#'
#' @export
#' @rdname trollsimreduced
trollsimreduced <- function(
  name = character(),
  path = character(),
  parameters = numeric(),
  inputs = list(),
  log = character(),
  final_pattern = data.frame(),
  reduced_outputs = data.frame()
) {
  return(new("trollsimreduced",
             name = name,
             path = path,
             parameters = parameters,
             inputs = inputs,
             log = log,
             final_pattern = final_pattern,
             reduced_outputs = reduced_outputs
  ))
}
