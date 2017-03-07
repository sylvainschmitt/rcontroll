if (!isGeneric("stack")) {
  setGeneric("stack", function(x, ...)
    standardGeneric("stack"))
}

#' Create TROLL simulations stack
#' 
#' Simulation stack is a collection of TROLL simulations created from several or
#' a list off TROLL simulations object.
#' 
#' @param x TROLLsim. Troll simulation object
#' @param ... other TROLL simulations
#'   
#' @return TROLL simulation stack
#' 
#' @examples
#' 
#' @name stack.TROLLsim
#' 
NULL

#' @rdname stack.TROLLsim
#' @export
setMethod("stack", signature(x='TROLLsim'), function(x, ...,layers=NULL) {
  simlist <- list(x, ...)
  stack(simlist)
})

#' @rdname stack.TROLLsim
#' @export
setMethod("stack", signature(x='list'), function(x, bands=NULL) {
  
  # check for proper object classes
  isTROLLsim <- sapply(x, function(i) inherits(i, 'TROLLsim')) 
  if(!all(isTROLLsim)){
    if(sum(isTROLLsim) == 0 ){
      x <- NULL
      stop('Input contains no TROLLsim objects. Stack cannot be created')
    }
    warning('Input contains not only TROLLsim objects. Automatic removal of non-TROLLsim objects')
    x <- x[isTROLLsim]
  }
  
  # use simulation names to identify list items
  names(x) <- sapply(x,function(x){listname<-x@name})  
  
  # check for duplicates
  isunique <- !duplicated(x)
  if(!all(isunique)){
    warning('Input contains duplicates. Automatic removal of duplicates')
    x <- x[isunique]
  }
  
  s <- new("TROLLsimstack")
  s@layers <- x
  #names(s) <- names(x)
  
  # compare to determine whether stack is structured or not
  # a simstack of only one TROLLsim object is structured by default
  if(length(x) == 1 || compareSim(x,stopiffalse = FALSE)){
    s@structured <- TRUE
    s@nbcols <- x[[1]]@par$general$nbcols
    s@nbrows <- x[[1]]@par$general$nbrows
    s@nbiter <- x[[1]]@par$general$nbiter
    s@iter <- x[[1]]@par$general$iter
    s@NV <- x[[1]]@par$general$NV
    s@NH <- x[[1]]@par$general$NH
  }
  
  return(s)
  
})