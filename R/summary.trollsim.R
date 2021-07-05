#' @import methods
NULL

#' Function to provide summary of TROLL outputs
#'
#' @param object trollsim or trollstack
#' @param ... unused argument
#'
#' @return Print in console
#'
#' @examples
#' NA
#' @name summary.trollsim
#'
NULL

#' @export
#' @rdname summary.trollsim
setMethod("summary", "trollsim", function(object, ...) {
  x <- object
  pars <- x@inputs$global$value
  names(pars) <- x@inputs$global$param
  cat("Object of class :", class(x)[1], "\n\n")
  cat("Name :", x@name, "\n\n")
  cat(
    "2D discrete network: horizontal step = ",
    pars["NV"], "m, one tree per 1 m^2 \n"
  )
  cat(
    "Number of sites      : ", pars["cols"],
    "x", pars["rows"], "\n"
  )
  cat("Number of iterations : ", pars["nbiter"], "\n")
  cat("Duration of timestep : ", 1 / pars["iterperyear"] * 365, "days \n")
  cat("Number of Species    : ", nrow(x@inputs$species), "\n\n")
})
