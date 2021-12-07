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
  if(inherits(x, "trollstackfull"))
    res <- TRUE
  if(inherits(x, "trollstackreduced"))
    res <- TRUE
  if(inherits(x, "trollstackabc"))
    res <- TRUE
  return(res)
}
