#' Options
#' 
#' RconTROLL package global options
#'  
#' @param path char. working directory path
#' @param src char. path to the TROLL C++ main code (default: package tools folder)
#' @param app char. name of the TROLL executable (default: TROLL.out)
#' @param init char. name of the input file (default: input.txt)
#' @param forest char. name of the virtual forest input file (default: forest.txt)
#' @param species char. path to the species input data (default: package extdata folder)
#' @param missing char. name of missing species file (default: missing.txt)
#' @param  output char. name of the output folder (default: OUTPUT)
#'   
#' @name option.RconTroll
NULL

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.RconTroll <- list(
    RconTroll.name = "test",
    RconTroll.path = "~/Documents/ECOFOG/Results",
    RconTroll.src = system.file("tools", "main.cpp",  
                            package = 'RconTroll'),
    RconTroll.app = "RconTroll.out",
    RconTroll.init = "input.txt",
    RconTroll.forest = "forest.txt",
    RconTroll.species = system.file("extdata", "species.txt",  
                                package = 'RconTroll'),
    RconTroll.missing = "missing.txt"
  )
  toset <- !(names(op.RconTroll) %in% names(op))
  if(any(toset)) options(op.RconTroll[toset])
  
  invisible()
}