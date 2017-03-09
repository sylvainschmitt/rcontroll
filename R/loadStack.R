#' @include loadOutput.R loadOutputOld.R stack.TROLLsim.R
NULL

#' load simulation stack
#' 
#' @param path char. path containing all models folders
#' @param old logical. DEPRECATED, use loadOutputOld for backward compatibility
#'   with all disturbance module
#' @param ignore char. names of folder or files to ignore in path that are not
#'   model folders
#'   
#' @return an S4 \linkS4class{TROLLsimstack} class object
#' 
#' @export
#' 
#' @examples
#' NA
#' 
loadStack <- function(path, 
                      old = FALSE,
                      ignore = NULL){
  
  # Get sim path list
  simpath <- list.files(path, full.names = FALSE, no.. = TRUE)
  if(!is.null(ignore))
    simpath <- simpath[-which(simpath %in% ignore)]

  # Get sim names list
  sim <- sapply(simpath, function(x){
    unlist(strsplit(
      list.files(file.path(path, x), pattern = '_0_agb.txt')
      , '_'))[1]
  })
  
  # Get opening function
  if(!old)
    FUN = loadOutput
  if(old){
    warning('Deprecated option for backward compatibilty with the old disturbance module')
    FUN = loadOutputOld
  }
  
  # Opening stack
  stack <- mapply(function(x,y){
    FUN(name = x, path = file.path(path, y))
  }, x = sim, y = simpath, SIMPLIFY = FALSE)
  stack <- stack(stack)
  
  return(stack)
}