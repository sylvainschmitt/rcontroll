#' @import methods
NULL

#' An S4 class to represent TROLL simulations
#'
#' This is an S4 class to represent TROLL simulations.
#'
#' @slot name char. simulation name
#' @slot path char. path to the simulation
#' @slot parameters numeric. parameters of the simulation (general inputs, random, forest)
#' @slot inputs list. simulation inputs (species, climate, daily, forest)
#' @slot log chr. simulation log
#' @slot final_pattern df. simulation final forest
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
    final_pattern = "data.frame"
  ),
  prototype(
    name = character(),
    path = character(),
    parameters = numeric(),
    inputs = list(),
    log = character(),
    final_pattern = data.frame()
  )
)

#' An S4 class to represent TROLL simulations
#'
#' This is an S4 class to represent TROLL simulations.
#'
#' @param name char. simulation name
#' @param path char. path to the simulation
#' @param parameters char. parameters of the simulation (general inputs, random, forest)
#' @param inputs numeric. simulation inputs (species, climate, daily, forest)
#' @param log chr. simulation log
#' @param final_pattern df. simulation final forest
#'
#' @export
#' @rdname trollsim
trollsim <- function(
  name = character(),
  path = character(),
  parameters = numeric(),
  inputs = list(),
  log = character(),
  final_pattern = data.frame()
) {
  return(new("trollsim",
             name = name,
             path = path,
             parameters = parameters,
             inputs = inputs,
             log = log,
             final_pattern = final_pattern
  ))
}
