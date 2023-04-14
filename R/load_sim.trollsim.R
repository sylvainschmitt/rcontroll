#' @include trollsim.R
#' @include load_output.R
#' @include load_stack.R
#' @import methods
NULL

#' Load outputs from simulation or stack of simulations
#'
#' `load_sim` is a method of [trollsim()] or [trollstack()] with wirtten files
#' not in R memory to load them into R memory taking advantage of
#' [load_output()] and [load_stack()].
#'
#' @param sim trollsim or trollstack.
#' @param ... unused argument.
#'
#' @return An S4 [trollsim()] or [trollstack()]  class object.
#'
#' @seealso [trollsim()], [trollstack()], [load_sim()], [load_stack()]
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
    load_stack(sim@name, sim@path)
  } else {
    load_output(sim@name, sim@path)
  }
})
