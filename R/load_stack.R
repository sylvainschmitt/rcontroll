#' @include load_output.R
NULL

#' Function to load TROLL stack
#'
#' @param name char. name given to the stack output
#' @param path char. path where the stack is saved
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
  if(!all(unlist(lapply(list(name, path), class)) %in% c("character")))
    stop("name and path should be character.")

  simulations <- list.files(path = file.path(path, name))
  stack_res <- lapply(simulations, function(sim)  load_output(sim, 
                                                              file.path(path, name, sim), 
                                                              type))
  names(stack_res) <- simulations
  stack_res <- trollstack(
    name = name,
    path = path_o,
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
    final_pattern = lapply(stack_res, slot, "final_pattern") %>% 
      bind_rows(.id = "simulation"),
    outputs = lapply(stack_res, slot, "outputs") %>% 
      bind_rows(.id = "simulation")
  )
  
  return(stack_res)
}
