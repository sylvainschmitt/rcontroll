#' @importFrom dplyr bind_rows

.merge_stack <- function(stack_res){
  if(inherits(stack_res[[1]], "trollsimfull"))
    res <- .merge_stack_full(stack_res)
  if(inherits(stack_res[[1]], "trollsimreduced"))
    res <- .merge_stack_reduced(stack_res)
  if(inherits(stack_res[[1]], "trollsimabc"))
    res <- .merge_stack_abc(stack_res)
  return(res)
}

.merge_stack_full <- function(stack_res){
  trollstackfull(
    name = stack_res[[1]]@name,
    path = stack_res[[1]]@path,
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
    species_outputs = lapply(stack_res, slot, "species_outputs") %>% 
      bind_rows(.id = "simulation")
  )
}

.merge_stack_reduced <- function(stack_res){
  trollstackreduced(
    name = stack_res[[1]]@name,
    path = stack_res[[1]]@path,
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
    reduced_outputs = lapply(stack_res, slot, "reduced_outputs") %>% 
      bind_rows(.id = "simulation")
  )
}

.merge_stack_abc <- function(stack_res){
  stop("Not implemented yet.")
}

