#' @import methods
NULL

#' Function to print TROLL outputs.
#'
#' @param x TROLLoutput
#'
#' @return Print in console
#'
#' @export
#'
#' @examples
#'
#' @name print.TROLLoutput
#' 
setMethod('print', 'TROLLoutput', function(x, ...) {

  cat('Object of class :', class(x)[1],'\n\n')

  cat('Name :', x@name, '\n\n')

  cat('2D discrete network: horizontal step = ', x@info$step, 'm, one tree per 1 m^2 \n')
  cat('Number of sites      : ', x@info$SitesNb[1], 'x', x@info$SitesNb[2], '\n')
  cat('Number of iterations : ', x@info$IterationsNb, '\n')
  cat('Duration of timestep : ', x@info$timestep, 'years \n')
  cat('Number of Species    : ', x@info$SpeciesNb, '\n\n')

  cat('Average computation time : ', x@info$ComputationTime, 'seconds \n\n')
})

setMethod('show', 'TROLLoutput', function(object) {

  cat('Object of class :', class(object)[1],'\n\n')

  cat('Name :', object@name, '\n\n')

  cat('2D discrete network: horizontal step = ', object@info$step, 'm, one tree per 1 m^2 \n')
  cat('Number of sites      : ', object@info$SitesNb[1], 'object', object@info$SitesNb[2], '\n')
  cat('Number of iterations : ', object@info$IterationsNb, '\n')
  cat('Duration of timestep : ', object@info$timestep, 'years \n')
  cat('Number of Species    : ', object@info$SpeciesNb, '\n\n')

  cat('Average computation time : ', object@info$ComputationTime, 'seconds \n\n')
})
