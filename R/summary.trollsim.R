#' @import methods
NULL

#' Function to a summary of TROLL outputs.
#'
#' @param object trollsim  or trollstack.
#' @param ... unused argument.
#'
#' @return Print in console.
#'
#' @examples
#'
#' data("TROLLv3_output")
#' summary(TROLLv3_output)
#'
#' @name summary.trollsim
#'
NULL

#' @export
#' @rdname summary.trollsim
setMethod("summary", "trollsim", function(object, ...) {
  print(object)
})
