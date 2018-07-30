#' Options
#' 
#' RconTROLL package global options
#'  
#' @param name char. name of simulations
#' @param src char. path to the TROLL C++ main code (default: package tools folder)
#' @param app char. path to the TROLL executable (default: package tools folder)
#' @param init char. path to the input file template (default: package extdata folder)
#' @param species char. path to the species input data (default: package extdata folder)
#' @param species char. path to the species input data (default: package extdata folder)
#' @param forest char. name of the forest inventory input file (default: forest.txt)
#'   
#' @name option.RconTROLL
NULL

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.RconTROLL <- list(
    RconTROLL.name = "TROLL_test",
    RconTROLL.src = file.path(system.file("tools", package = 'RconTROLL'), "main_v2.3.2.cpp"),
    RconTROLL.app = file.path(system.file("tools", package = 'RconTROLL'), "TROLL.exe"),
    RconTROLL.init = system.file("extdata", "init.txt",  
                                 package = 'RconTROLL'),
    RconTROLL.species = system.file("extdata", "species.txt",  
                                package = 'RconTROLL'),
    RconTROLL.climate = system.file("extdata", "climate.txt",  
                                    package = 'RconTROLL'),
    RconTROLL.forest = system.file("extdata", "forest.txt",  
                                   package = 'RconTROLL')
  )
  toset <- !(names(op.RconTROLL) %in% names(op))
  if(any(toset)) options(op.RconTROLL[toset])
  
  invisible()
}