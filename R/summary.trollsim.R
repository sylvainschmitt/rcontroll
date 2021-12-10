#' @import methods
NULL

#' Function to provide summary of TROLL outputs
#'
#' @param object trollsim  or trollstack.
#' @param ... unused argument.
#'
#' @return Print in console.
#'
#' @examples
#' NA
#' @name summary.trollsim
#'
NULL

#' @export
#' @rdname summary.trollsim
setMethod("summary", "trollsim", function(object, ...) {
  print(object)
})
