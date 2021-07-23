#' @include trollsimabc.R
#' @import methods
NULL

#' An S4 class to represent TROLL stack
#'
#' This is an S4 class to represent TROLL stack with abc outputs.
#'
#' @export
setClass(
  "trollstackabc",
  contains = 'trollsimabc'
)

#' An S4 class to represent TROLL stack
#'
#' This is an S4 class to represent TROLL stack
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
#' @rdname trollstackabc
trollstackabc <- function(
  name = character(),
  path = character(),
  parameters = numeric(),
  inputs = list(),
  log = character(),
  final_pattern = data.frame(),
  reduced_outputs = data.frame(),
  abc_outputs = list()
) {
  return(new("trollstackabc",
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
