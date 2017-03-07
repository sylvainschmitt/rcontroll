#' Compare parameters of two or more TROLL simulations
#' 
#' Function to compare whether two or more TROLLsim objects have been executed with the same set of general parameters. Please note: This function does not compare output.
#'
#' @param sim TROLL.sim object
#' @param ... TROLL.sim objects
#' @param rowcols logical. If TRUE, number of rows and columns of simulations are compared 
#' @param duration logical. If TRUE, number of iterations and iterations per year are compared
#' @param dimension logical. If TRUE, cell dimensions (NH, NV) of TRoLL are compared
#' @param species logical. If TRUE, species number is compared
#' @param stopiffalse logical. If TRUE, an error will occur if the objects are not the same
#' @param showwarning logical. If TRUE, a warning will be given if objects are not the same. Only relevant when stopiffalse is not TRUE. 
#'
#' @return logical
#' 
#' @examples
#' 
compareSim <- function(sim, ..., rowcols=TRUE, duration=TRUE, dimension=TRUE, species=FALSE, stopiffalse=TRUE,showwarning=FALSE) {
  
  result <- TRUE
  objects <- c(sim, list(...))
  
  if (!isTRUE(length(objects) > 1)) {
    warning('There should be at least 2 TROLLsim objects to compare')
    return(result)
  }	
  
  # properties for comparison
  
  nbcols1 <- objects[[1]]@par$general$nbcols
  nbrows1 <- objects[[1]]@par$general$nbrows
  nbiter1 <- objects[[1]]@par$general$nbiter
  iter1 <- objects[[1]]@par$general$iter
  NV1 <- objects[[1]]@par$general$NV
  NH1 <- objects[[1]]@par$general$NH
  numesp1 <- objects[[1]]@par$general$numesp
  
  # cycle through objects
  
  for (i in 2:length(objects)) { 
    if (rowcols) {
      if ( !(identical(nbcols1, objects[[i]]@par$general$nbcols)) ) {
        result <- FALSE
        if (stopiffalse) { stop('different number of columns') } 
        if (showwarning) { warning('different number of columns') } 
      }	
      if ( !(identical(nbrows1, objects[[i]]@par$general$nbrows)) ) {
        result <- FALSE
        if (stopiffalse) { stop('different number of rows') }
        if (showwarning) { warning('different number of rows') }
      }
    }
    if (duration) {
      if ( !(identical(nbiter1, objects[[i]]@par$general$nbiter)) ) {
        result <- FALSE
        if (stopiffalse) { stop('different number of iterations') } 
        if (showwarning) { warning('different number of iterations') } 
      }	
      if ( !(identical(iter1, objects[[i]]@par$general$iter)) ) {
        result <- FALSE
        if (stopiffalse) { stop('different number of iterations per year') }
        if (showwarning) { warning('different number or iterations per year') }
      }
    }
    if (dimension) {
      if ( !(identical(NV1, objects[[i]]@par$general$NV)) ) {
        result <- FALSE
        if (stopiffalse) { stop('different vertical cell dimensions') } 
        if (showwarning) { warning('different vertical cell dimensions') } 
      }	
      if ( !(identical(NH1, objects[[i]]@par$general$NH))) {
        result <- FALSE
        if (stopiffalse) { stop('different horizontal cell dimensions') }
        if (showwarning) { warning('different horizontal cell dimensions') }
      }
    }
    if (species) {
      if ( !(identical(numesp1, objects[[i]]@par$general$numesp)) ) {
        result <- FALSE
        if (stopiffalse) { stop('different species number') } 
        if (showwarning) { warning('different species number') } 
      }	
    }
  }
  return(result)
}