#' @import methods
NULL

#' Function to print TROLL outputs.
#'
#' @param x TROLLsimstack
#'
#' @return Print in console
#'
#' @export
#'
#' @examples
#'
#' @name print.TROLLsimstack
#' 
setMethod('print', 'TROLLsimstack', function(x, ...) {

  cat('Object of class :', class(x)[1],'\n\n')
  
  cat('Structured :', x@structured, '\n')
  cat('Compressed', x@compressed, '\n\n')
  
  if(isTRUE(x@structured)){
  cat('Number of sites      : ', x@nbcols, 'x', x@nbrows, '\n')
  cat('Number of iterations : ', x@nbiter, '\n')
  cat('Duration of timestep : ', 1/x@iter, 'years \n')}
  
})

setMethod('show', 'TROLLsimstack', function(object) {

  cat('Object of class :', class(object)[1],'\n\n')
  
  cat('Structured :', object@structured, '\n')
  cat('Compressed', object@compressed, '\n\n')
  
  if(isTRUE(object@structured)){
    cat('Number of sites      : ', object@nbcols, 'x', object@nbrows, '\n')
    cat('Number of iterations : ', object@nbiter, '\n')
    cat('Duration of timestep : ', 1/object@iter, 'years \n')}
})
