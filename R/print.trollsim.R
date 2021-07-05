#' @import methods
NULL

#' Function to print or show TROLL outputs.
#'
#' @param x trollsim or trollstack
#' @param object trollsim or trollstack
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

#' @export
#' @rdname print.trollsim
setMethod("show", "trollsim", function(object) {
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
