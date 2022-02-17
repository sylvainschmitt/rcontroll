#' @include troll.R
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach %dopar%
#' @importFrom iterators icount
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom dplyr bind_rows
#' @importFrom tidyr unnest
NULL

#' Stack
#'
#' Run a TROLL stack.
#' 
#' @param name char. Stack name (if NULL timestamp).
#' @param simulations char. Simulation names.
#' @param path char. Path to save the simulation outputs, the default is null
#'   corresponding to a simulation in memory without saved intermediary files.
#' @param global df. Global parameters.
#' @param species df. Species parameters.
#' @param climate df. Climate parameters.
#' @param daily df. Daily variation parameters.
#' @param forest df. TROLL with forest input, if null starts from an empty grid
#'   (default NULL).
#' @param verbose bool. Show TROLL outputs in the console.
#' @param cores int. Number of cores for parallelization, if NULL available
#'   cores - 1 (default NULL).
#' @param overwrite bool. Overwrite previous outputs.
#' @param thin int. Vector of integers corresponding to the iterations to be
#'   kept to reduce output size, default is NULL and corresponds to no
#'   thinning.
#'
#' @return A trollstack object. 
#'
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' data("TROLLv3_species")
#' data("TROLLv3_climatedaytime12")
#' data("TROLLv3_daytimevar")
#' data("TROLLv3_output")
#' TROLLv3_input_stack <- generate_parameters(cols = 100, rows = 100,
#'                                            iterperyear = 12, nbiter = 12*1) %>% 
#'                                            mutate(simulation = list(c("seed50000", "seed500"))) %>% 
#'                                            unnest(simulation)
#' TROLLv3_input_stack[62,2] <- 500 # Cseedrain
#' stack(name = "teststack", 
#'       simulations = c("seed50000", "seed500"),
#'       global = TROLLv3_input_stack,
#'       species = TROLLv3_species,
#'       climate = TROLLv3_climatedaytime12,
#'       daily = TROLLv3_daytimevar,
#'       verbose = F,
#'       cores = 2,
#'       thin = c(1,5,10))
#' }
#' 
stack <- function(name = NULL,
                  simulations,
                  path = NULL,
                  global,
                  species,
                  climate,
                  daily,
                  forest = NULL,
                  verbose = TRUE,
                  cores = NULL,
                  overwrite = TRUE,
                  thin = NULL) {
  # cores
  if(is.null(cores)){
    cores <- detectCores() - 1
    message("Detect cores was not defined, ", cores, " cores will be used.")
  }
  if ((detectCores() - 1) < cores) {
    cores <- detectCores()-1
    warning(paste("It seems you attributed more cores than your CPU has! Automatic reduction to",
                  cores, "cores."))
  }
  
  # stack name
  if (is.null(name)) {
    name <- paste0(
      "stack_",
      gsub(
        " ", "_",
        timestamp(
          prefix = "",
          suffix = "",
          quiet = T
        )
      )
    )
  }

  # stack path
  tmp <- FALSE
  if (is.null(path)) {
    path <- getOption("rcontroll.tmp")
    tmp <- TRUE
  }
  if (name %in% list.dirs(path, full.names = FALSE)[-1]) {
    if (!overwrite) {
      stop("Outputs already exist, use overwrite = TRUE.")
    }
    path_o <- file.path(path, name)
    unlink(path_o, recursive = TRUE)
  } else {
    path_o <- file.path(path, name)
  }
  dir.create(path_o)

  # sim names
  sim_names <- paste0('sim_', simulations)
  
  # inputs
  global <- .prep_input(global, simulations)
  species <- .prep_input(species, simulations)
  climate <- .prep_input(climate, simulations)
  daily <- .prep_input(daily, simulations)
  if(!is.null(forest)) {
    global <- .prep_input(global, simulations)
  } else {
    forest <- lapply(simulations, function(x) forest)
    names(forest) <- simulations
  }
  if(tmp) {
    sim_path <- lapply(simulations, function(x) NULL)
    names(sim_path) <- simulations
  } else {
    sim_path <- lapply(simulations, function(x) path_o)
    names(sim_path) <- simulations
  }
  
  # stack
  i <- NULL
  cl <- makeCluster(cores, outfile = "")
  registerDoSNOW(cl)
  pb <- txtProgressBar(max = length(simulations), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  stack_res <- foreach(i=1:length(simulations), .export = ".troll_child",
                       .options.snow = opts) %dopar% {
                     sim <- simulations[i]
                     .troll_child(
                       name = sim,
                       path = sim_path[[sim]],
                       global = global[[sim]],
                       species = species[[sim]],
                       climate = climate[[sim]],
                       daily = daily[[sim]],
                       forest = forest[[sim]],
                       verbose = verbose,
                       overwrite = overwrite,
                       thin = thin
                     )
                   }
  close(pb)
  stopCluster(cl)
  names(stack_res) <- simulations
  stack_res <- trollstack(
    name = stack_res[[1]]@name,
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
    forest = lapply(stack_res, slot, "forest") %>% 
      bind_rows(.id = "simulation"),
    ecosystem = lapply(stack_res, slot, "ecosystem") %>% 
      bind_rows(.id = "simulation"),
    species = lapply(stack_res, slot, "species") %>% 
      bind_rows(.id = "simulation")
  )
  
  # unlink stack path with tmp
  if(tmp)
    unlink(path_o)
  
  return(stack_res)
}

.prep_input <- function(input, simulations){
  simulation <- NULL
  
  if("simulation" %in% colnames(input)){
    if(!all(as.character(unique(input$simulation)) %in% simulations))
      stop("Simulations names in your inputs don't match indicated simulations names.")
    input <- split(input, input$simulation)
    input <- lapply(input, select, -simulation)
  } else {
    input <- lapply(simulations, function(x) input)
    names(input) <- simulations
  }
  return(input)
}
