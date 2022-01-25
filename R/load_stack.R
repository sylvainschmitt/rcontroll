#' @include load_output.R
NULL

#' Function to load a stack of outputs from TROLL simulations.
#'
#' @param name char. Name given to the stack output.
#' @param path char. Path where the stack is saved.
#' @param thin int. Vector of integers corresponding to the iterations to be
#'   kept to reduce outputs size, default is NULL and corresponds to no
#'   thinning.
#'   
#' @return An S4 \linkS4class{trollsim} class object.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  load_stack("test", "./")
#' }
#' 
load_stack <- function(name,
                        path,
                       thin = NULL) {
  # Check inputs
  if(!all(unlist(lapply(list(name, path), class)) %in% c("character")))
    stop("name and path should be character.")

  simulations <- list.files(path = file.path(path, name))
  stack_res <- lapply(simulations, function(sim)  load_output(sim, 
                                                              file.path(path, name, sim), 
                                                              thin = thin))
  names(stack_res) <- simulations
  stack_res <- trollstack(
    name = name,
    path = path,
    parameters = stack_res[[1]]@parameters,
    inputs = list(
      global = lapply(stack_res, slot, "inputs") %>% 
        lapply(`[[`, "global") %>% 
        bind_rows(.id = "simulation"),
      species = lapply(stack_res, slot, "inputs") %>% 
        lapply(`[[`, "species") %>% 
        bind_rows(.id = "simulation"),
      climate = lapply(stack_res, slot, "inputs") %>% 
        lapply(`[[`, "climate") %>% 
        bind_rows(.id = "simulation"),
      daily = lapply(stack_res, slot, "inputs") %>% 
        lapply(`[[`, "daily") %>% 
        bind_rows(.id = "simulation"),
      forest = lapply(stack_res, slot, "inputs") %>% 
        lapply(`[[`, "forest") %>% 
        bind_rows(.id = "simulation")
    ),
    log = paste(lapply(stack_res, slot, "log")),
    forest = lapply(stack_res, slot, "forest") %>% 
      bind_rows(.id = "simulation"),
    ecosystem = lapply(stack_res, slot, "ecosystem") %>% 
      bind_rows(.id = "simulation"),
    species = lapply(stack_res, slot, "outputs") %>% 
      bind_rows(.id = "species")
  )
  
  return(stack_res)
}
