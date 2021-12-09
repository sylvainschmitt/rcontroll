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
#' This is an S4 class to represent TROLL stack
#'
#' @param name char. simulation name
#' @param path char. path to the simulation
#' @param parameters numeric. parameters of the simulation (general inputs, random, forest)
#' @param inputs list. simulation inputs (species, climate, daily, forest)
#' @param log chr. simulation log
#' @param final_pattern df. simulation final forest
#' @param outputs df. species and ecosystem metrics (reduced or full)
#'
#' @export
#' @rdname trollstack
trollstack <- function(
  name = character(),
  path = character(),
  parameters = numeric(),
  inputs = list(),
  log = character(),
  final_pattern = data.frame(),
  outputs = data.frame()
) {
  return(new("trollstack",
             name = name,
             path = path,
             parameters = parameters,
             inputs = inputs,
             log = log,
             final_pattern = final_pattern,
             outputs = outputs
  ))
}
