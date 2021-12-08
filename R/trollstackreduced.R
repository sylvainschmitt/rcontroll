#' @include trollsimreduced.R
#' @import methods
NULL

#' An S4 class to represent TROLL stack
#'
#' This is an S4 class to represent TROLL stack with reduced outputs.
#'
#' @export
setClass(
  "trollstackreduced",
  contains = 'trollsimreduced'
)

#' An S4 class to represent TROLL stack
#'
#' This is an S4 class to represent TROLL stack.
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
#' @rdname trollstackreduced
trollstackreduced <- function(
  name = character(),
  path = character(),
  parameters = numeric(),
  inputs = list(),
  log = character(),
  final_pattern = data.frame(),
  reduced_outputs = data.frame()
) {
  return(new("trollstackreduced",
             name = name,
             path = path,
             parameters = parameters,
             inputs = inputs,
             log = log,
             final_pattern = final_pattern,
             reduced_outputs = reduced_outputs
  ))
}
