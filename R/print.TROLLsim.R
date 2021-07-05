#' @import methods
NULL

#' Function to print or show TROLL outputs.
#'
#' @param x TROLLsim or TROLLsim
#' @param ... unused argument
#'
#' @return Print or show in console
#'
#' @examples
#' NA
#'
#' @name print.TROLLsim
NULL

#' @export
#' @rdname print.TROLLsim
setMethod('print', 'TROLLsim', function(x, ...) {
  cat("Simulation:", x@name)
  cat(x@info)
})
