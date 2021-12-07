#' @include load_output.R
#' @include merge_stack.R
NULL

#' Function to load TROLL stack
#'
#' @param name char. name given to the stack output
#' @param path char. path where the stack is saved
#' @param type char. type of simulation (full, reduce or abc)
#'
#' @return an S4 \linkS4class{trollsim} class object
#'
#' @export
#'
#' @examples
#'
#' NA
load_stack <- function(name,
                        path,
                        type) {
  # Check inputs
  if(!all(unlist(lapply(list(name, path, type), class)) %in% c("character")))
    stop("name, path, and type should be character.")

  simulations <- list.files(path = file.path(path, name))
  stack_res <- lapply(simulations, function(sim)  load_output(sim, 
                                                              file.path(path, name, sim), 
                                                              type))
  names(stack_res) <- simulations
  stack_res[[1]]@name <- name
  stack_res[[1]]@path <- path_o
  stack_res <- .merge_stack(stack_res)
  
  return(stack_res)
}
