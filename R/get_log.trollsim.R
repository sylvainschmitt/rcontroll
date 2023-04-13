#' @include trollsim.R
#' @import methods
NULL

#' Extract simulation log
#'
#' `get_forest()` extract the simulation log `TROLL` outputs.
#'
#' @param sim trollsim or trollstack.
#' @param ... unused argument.
#'
#' @return the log in the console
#'
#' @seealso [trollsim()], [trollstack()], [troll()], [stack()]
#'
#' @examples
#'
#' data("TROLLv3_output")
#' get_log(TROLLv3_output)
#'
#' @name get_log
NULL

#' @rdname get_log
#' @export
setGeneric("get_log", function(sim, ...) {
  return(standardGeneric("get_log"))
})

#' @rdname get_log
#' @export
setMethod("get_log", "trollsim", function(sim, ...) {
  cat(sim@log)
})
