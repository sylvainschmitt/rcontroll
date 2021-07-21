#' @include trollsimreduced.R
#' @import methods
NULL

#' An S4 class to represent TROLL simulations
#'
#' This is an S4 class to represent TROLL simulations with abc outputs.
#'
#' @slot abc_outputs list.. ABC metrics
#'
#' @export
setClass(
  "trollsimabc",
  contains = 'trollsimreduced',
  representation(
    abc_outputs = "list"
  ),
  prototype(
    abc_outputs = list()
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
#' @param abc_outputs list. abc metrics
#'
#' @export
#' @rdname trollsimabc
trollsimabc <- function(
  name = character(),
  path = character(),
  parameters = numeric(),
  inputs = list(),
  log = character(),
  final_pattern = data.frame(),
  reduced_outputs = data.frame(),
  abc_outputs = list()
) {
  return(new("trollsimabc",
             name = name,
             path = path,
             parameters = parameters,
             inputs = inputs,
             log = log,
             final_pattern = final_pattern,
             reduced_outputs = reduced_outputs,
             abc_outputs = abc_outputs
  ))
}
