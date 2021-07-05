#' @import methods
NULL

#' Function to print or show TROLL outputs.
#'
#' @param x trollsim or trollstack
#' @param ... unused argument
#'
#' @return Print or show in console
#'
#' @examples
#' NA
#' @name print.trollsim
NULL

#' @export
#' @rdname print.trollsim
setMethod("print", "trollsim", function(x, ...) {
  cat("Simulation:", x@name)
  cat(x@info)
})
