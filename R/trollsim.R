#' @import methods
NULL

#' An S4 class to represent TROLL simulations
#'
#' This is an S4 class to represent TROLL simulations.
#'
#' @slot name char. model name
#' @slot path char. path to the model
#' @slot inputs list. model inputs (general, species, climate, daily, forest)
#' @slot log chr. model log
#'
#' @export
setClass(
  "trollsim",
  representation(
    name = "character",
    path = "character",
    inputs = "list",
    log = "character"
  ),
  prototype(
    name = character(),
    path = character(),
    inputs = list(),
    log = character()
  )
)

#' An S4 class to represent TROLL simulations
#'
#' This is an S4 class to represent TROLL simulations.
#'
#' @param name char. model name
#' @param path char. path to the model
#' @param inputs list. model inputs (general, species, climate, daily, forest)
#' @param log chr. model log
#'
#' @export
#' @rdname trollsim
trollsim <- function(name = character(),
                     path = character(),
                     inputs = list(),
                     log = character()) {
  return(new("trollsim",
    name = name,
    path = path,
    inputs = inputs,
    log = log
  ))
}
