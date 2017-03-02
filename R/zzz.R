#' Options
#' 
#' TROLL package global options
#'  
#' @param path char. working directory path
#' @param src char. path to the TROLL C++ main code (default: package tools folder)
#' @param app char. name of the TROLL executable (default: TROLL.out)
#' @param init char. name of the input file (default: input.txt)
#' @param forest char. name of the virtual forest input file (default: forest.txt)
#' @param species char. path to the species input data (default: package extdata folder)
#' @param missing char. name of missing species file (default: missing.txt)
#' @param output char. name of the output folder (default: OUTPUT)
#'   
#' @name option.TROLL
NULL

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.TROLL <- list(
    TROLL.name = "test",  # include in documentation
    TROLL.path = "~/Documents/ECOFOG/Results", # set to current wd
    TROLL.src = system.file("tools", "main.cpp",  
                            package = 'TROLL'), # explain? 
    TROLL.app = "TROLL.out",
    TROLL.init = "input.txt",
    TROLL.forest = "forest.txt", # rename "inventory"
    TROLL.species = system.file("extdata", "species.txt", # several files
                                package = 'TROLL'),
    TROLL.missing = "missing.txt" # ?
  )
  toset <- !(names(op.TROLL) %in% names(op))
  if(any(toset)) options(op.TROLL[toset])
  
  invisible()
}