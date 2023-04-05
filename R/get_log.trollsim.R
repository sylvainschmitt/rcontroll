#' @include trollsim.R
#' @import methods
NULL

#' Function to get the log from TROLL outputs.
#'
#' @param sim trollsim.
#' @param ... unused argument.
#'
#' @return the log in the console
#'
#' @examples
#'
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
