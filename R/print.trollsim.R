#' @import methods
NULL

#' Function to print or show TROLL outputs.
#'
#' @param x trollsim or trollstack
#' @param object trollsim  or trollstack
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
  cat("Object of class :", class(x)[1], "\n")
  cat("Name :", x@name, "\n")
  cat("Path :", x@path, "\n")
  if(is.stack(x))
    cat("Number of simulations :", length(unique(x@inputs$global$simulation)), "\n")
  cat("Forest :", as.logical(x@parameters["forest"]), "\n")
  cat("Random :", as.logical(x@parameters["random"]), "\n\n")
  cat("2D discrete network: horizontal step = ",
      x@parameters["NV"], "m, one tree per 1 m^2 \n"
  )
  cat(
    "Number of sites      : ", x@parameters["cols"],
    "x", x@parameters["rows"], "\n"
  )
  cat("Number of iterations : ", x@parameters["nbiter"], "\n")
  cat("Duration of timestep : ", 1 / x@parameters["iterperyear"] * 365, "days \n")
  cat("Number of Species    : ", nrow(x@inputs$species), "\n\n")
})

#' @export
#' @rdname print.trollsim
setMethod("show", "trollsim", function(object) {
  print(object)
})
