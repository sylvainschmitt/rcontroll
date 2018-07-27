#' Options
#' 
#' RconTROLL package global options
#'  
#' @param name char. name of simulations
#' @param src char. path to the TROLL C++ main code (default: package extdata folder)
#' @param app char. path to the TROLL executable (default: package extdata folder)
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
    RconTROLL.src = system.file("tools", "main_v2.3.2.cpp",  
                            package = 'RconTROLL'),
    RconTROLL.app = system.file("extdata", "RconTROLL.exe",  
                                 package = 'RconTROLL'),
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