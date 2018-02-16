#' @import methods
NULL

#' Function to print or show TROLL outputs.
#'
#' @param x TROLLsim or TROLLsimstack
#' @param object TROLLsim or TROLLsimstack
#' @param ... unused argument
#'
#' @return Print or show in console
#'
#' @examples
#' NA
#'
#' @name print.TROLLsim
NULL

#' @export
#' @rdname print.TROLLsim
setMethod('print', 'TROLLsim', function(x, ...) {

  cat('Object of class :', class(x)[1],'\n\n')
  cat('Name :', x@name, '\n\n')
  cat('2D discrete network: horizontal step = ', 
      x@info$step, 'm, one tree per 1 m^2 \n')
  cat('Number of sites      : ', x@info$SitesNb[1], 
      'x', x@info$SitesNb[2], '\n')
  cat('Number of iterations : ', x@info$IterationsNb, '\n')
  cat('Duration of timestep : ', x@info$timestep, 'years \n')
  cat('Number of Species    : ', x@info$SpeciesNb, '\n\n')

  cat('Computation time : ', x@info$ComputationTime, 'seconds \n\n')
})

#' @export
#' @rdname print.TROLLsim
setMethod('show', 'TROLLsim', function(object) {

  cat('Object of class :', class(object)[1],'\n\n')

  cat('Name :', object@name, '\n\n')

  cat('2D discrete network: horizontal step = ', object@info$step, 
      'm, one tree per 1 m^2 \n')
  cat('Number of sites      : ', object@info$SitesNb[1], 'object', 
      object@info$SitesNb[2], '\n')
  cat('Number of iterations : ', object@info$IterationsNb, '\n')
  cat('Duration of timestep : ', object@info$timestep, 'years \n')
  cat('Number of Species    : ', object@info$SpeciesNb, '\n\n')

  cat('Average computation time : ', object@info$ComputationTime, 
      'seconds \n\n')
})

#' @export
#' @rdname print.TROLLsim
setMethod('print', 'TROLLsimstack', function(x, ...) {
  
  cat('Object of class :', class(x)[1],'\n\n')
  
  cat('Structured :', x@structured, '\n')
  cat('Aggregated', x@aggregated, '\n\n')
  
  if(isTRUE(x@structured)){
    cat('Number of sites      : ', x@nbcols, 'x', x@nbrows, '\n')
    cat('Number of iterations : ', x@nbiter, '\n')
    cat('Duration of timestep : ', 1/x@iter, 'years \n')}
  
})

#' @export
#' @rdname print.TROLLsim
setMethod('show', 'TROLLsimstack', function(object) {
  
  cat('Object of class :', class(object)[1],'\n\n')
  
  cat('Structured :', object@structured, '\n')
  cat('Aggregated', object@aggregated, '\n\n')
  
  if(isTRUE(object@structured)){
    cat('Number of sites      : ', object@nbcols, 'x', object@nbrows, '\n')
    cat('Number of iterations : ', object@nbiter, '\n')
    cat('Duration of timestep : ', 1/object@iter, 'years \n')}
})