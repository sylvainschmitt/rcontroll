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
#' @param name char. model name
#' @param path char. path to the model
#' @param reduced_outputs df. species and ecosystem metrics
#' @param inputs list. model inputs (general, species, climate, daily, forest)
#' @param log chr. model log
#'
#' @export
#' @rdname trollsimreduced
trollsimreduced <- function(name = character(),
                            path = character(),
                            reduced_outputs = data.frame(),
                            inputs = list(),
                            log = character()) {
  return(new("trollsimreduced",
             name = name,
             path = path,
             reduced_outputs = reduced_outputs,
             inputs = inputs,
             log = log
  ))
}
