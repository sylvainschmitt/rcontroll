#' @import methods
NULL

#' Function to provide summary of TROLL outputs
#'
#' @param object TROLLsim or TROLLsimstack
#' @param ... unused argument
#'
#' @return Print in console
#'
#' @examples
#' NA
#'
#' @name summary.TROLLsim
#' 
NULL

#' @export
#' @rdname summary.TROLLsim
setMethod('summary', 'TROLLsim', function(object, ...) {
  cat("Simulation:", object@name)
  cat(object@info)
})
