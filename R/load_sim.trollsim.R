#' @include trollsim.R
#' @include load_output.R
#' @include load_stack_output.R
#' @import methods
NULL

#' Function to load in memory outputs from a TROLL simulation.
#'
#' @param sim trollsim.
#' @param ... unused argument.
#'
#' @return An S4 \linkS4class{trollsim} class object.
#'
#' @name load_sim
NULL

#' @rdname load_sim
#' @export
setGeneric("load_sim", function(sim, ...) {
  return(standardGeneric("load_sim"))
})

#' @rdname load_sim
#' @export
setMethod("load_sim", "trollsim", function(sim, ...) {
  if (inherits(sim, "trollstack")) {
    load_stack_output(sim@name, sim@path)
  } else {
    load_output(sim@name, sim@path)
  }
})

