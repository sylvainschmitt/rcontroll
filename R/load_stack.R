#' @include load_output.R
NULL

#' Load outputs from a stack of simulations
#'
#' `load_stack` load outputs from a stack of `TROLL` simulation files using
#' `TROLL` stack of simulation `name` and `path`.
#'
#' @param name char. Name given to the stack output.
#' @param path char. Path where the stack is saved.
#' @param thin int. Vector of integers corresponding to the iterations to be
#'   kept to reduce output size, default is NULL and corresponds to no thinning.
#'
#' @return An S4 [trollstack()] class object.
#'
#' @seealso [trollsim()], [trollstack()], [load_sim()], [load_stack()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_stack("test", "./")
#' }
#'
load_stack <- function(name,
                       path,
                       thin = NULL) {
  # Check inputs
  if (!all(unlist(lapply(list(name, path), class)) %in% c("character"))) {
    stop("name and path should be character.")
  }

  simulations <- list.files(path = file.path(path))
  stack_res <- lapply(simulations, function(sim) {
    load_output(sim,
      file.path(path, sim),
      thin = thin
    )
  })
  names(stack_res) <- simulations
  stack_res <- trollstack(
    name = name,
    path = path,
    mem = TRUE,
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
        bind_rows(.id = "simulation"),
      lidar = lapply(stack_res, slot, "inputs") %>%
        lapply(`[[`, "lidar") %>%
        bind_rows(.id = "simulation")
    ),
    log = paste(lapply(stack_res, slot, "log")),
    forest = lapply(stack_res, slot, "forest") %>%
      bind_rows(.id = "simulation"),
    ecosystem = lapply(stack_res, slot, "ecosystem") %>%
      bind_rows(.id = "simulation"),
    species = lapply(stack_res, slot, "species") %>%
      bind_rows(.id = "simulation"),
    las = lapply(stack_res, slot, "las")
  )

  if (nrow(stack_res@inputs$lidar) == 0) {
    stack_res@las <- list()
  }
  if (nrow(stack_res@inputs$lidar) > 0) {
    stack_res@las <- lapply(stack_res@las, `[[`, 1)
  }

  return(stack_res)
}
