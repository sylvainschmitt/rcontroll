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
#' @param name char. model name
#' @param path char. path to the model
#' @param species_outputs df. species and ecosystem metrics
#' @param inputs list. model inputs (general, species, climate, daily, forest)
#' @param log chr. model log
#'
#' @export
#' @rdname trollsimfull
trollsimfull <- function(name = character(),
                         path = character(),
                         species_outputs = data.frame(),
                         inputs = list(),
                         log = character()) {
  return(new("trollsimfull",
             name = name,
             path = path,
             species_outputs = species_outputs,
             inputs = inputs,
             log = log
  ))
}
