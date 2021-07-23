#' Function to test if TROLL outputs is a stack.
#'
#' @param x object
#'
#' @return logical
#'
#' @examples
#' NA
#' 
is.stack <- function(x) {
  res <- FALSE
  if(inherits(x, "trollsimfull"))
    res <- TRUE
  if(inherits(x, "trollsimreduced"))
    res <- TRUE
  if(inherits(x, "trollsimabc"))
    res <- TRUE
  return(res)
}
