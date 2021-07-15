#' @include trollsimreduced.R
#' @import methods
NULL

#' An S4 class to represent TROLL simulations
#'
#' This is an S4 class to represent TROLL simulations with abc outputs.
#'
#' @slot abc_outputs df. ABC metrics
#'
#' @export
setClass(
  "trollsimabc",
  contains = 'trollsimreduced',
  representation(
    abc_outputs = "data.frame"
  ),
  prototype(
    abc_outputs = data.frame()
  )
)

#' An S4 class to represent TROLL simulations
#'
#' This is an S4 class to represent TROLL simulations.
#'
#' @param name char. model name
#' @param path char. path to the model
#' @param reduced_outputs df. species and ecosystem metrics
#' @param abc_outputs df. species and ecosystem metrics
#' @param inputs list. model inputs (general, species, climate, daily, forest)
#' @param log chr. model log
#'
#' @export
#' @rdname trollsimabc
trollsimabc <- function(name = character(),
                        path = character(),
                        reduced_outputs = data.frame(),
                        abc_outputs = data.frame(),
                        inputs = list(),
                        log = character()) {
  return(new("trollsimabc",
             name = name,
             path = path,
             reduced_outputs = reduced_outputs,
             abc_outputs = abc_outputs,
             inputs = inputs,
             log = log
  ))
}
