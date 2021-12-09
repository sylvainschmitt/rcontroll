#' @include troll.R
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach %dopar%
#' @importFrom iterators icount
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom dplyr bind_rows
NULL

#' Stack
#'
#' Run a TROLL stack
#'
#' @param name char. stack name (if NULL timestamp)
#' @param simulations char. simulations names
#' @param path char. path to the stack (tmp if NULL)
#' @param global df. global parameters, add a simulation column for repeated
#'   simulations
#' @param species df. species parameters, add a simulation column for repeated
#'   simulations
#' @param climate df. climate parameters, add a simulation column for repeated
#'   simulations
#' @param daily df. daily variation parameters, add a simulation column for
#'   repeated simulations
#' @param forest df. TROLL with forest input, if null start from an empty grid,
#'   add a simulation column for repeated simulations (default NULL)
#' @param verbose bool. show TROLL outputs in the console
#' @param cores int. number of cores for parallelization, if NULL available
#'   cores - 1 (default NULL)
#' @param overwrite bool. overwrite previous outputs
#' @param thin int. vector of integers corresponding to iterations to be kept
#'   (default NULL)
#'
#' @return trollstack
#'
#' @export
#'
#' @examples
#' \dontrun{
#' example
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
  # for tests
  # data("TROLLv3_input")
  # data("TROLLv3_species")
  # data("TROLLv3_climatedaytime365")
  # data("TROLLv3_daytimevar")
  # TROLLv3_input$value[5] <- 10 # iterations
  # global <- TROLLv3_input %>% 
  #   mutate(simulation = list(1:2)) %>% 
  #   unnest(simulation)
  # global[62,2] <- 500 # Cseedrain
  # species <- TROLLv3_species
  # climate <- TROLLv3_climatedaytime12
  # daily <- TROLLv3_daytimevar
  # name <- NULL
  # simulations <- as.character(1:2)
  # full = T
  # abc = F
  # random = T
  # forest = NULL
  # cores = NULL
  # overwrite = T
  # path <- getwd()

  # check all inputs
  
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
  cl <- makeCluster(cores, outfile = "")
  registerDoSNOW(cl)
  pb <- txtProgressBar(max = length(simulations), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  stack_res <- foreach(i=1:length(simulations),
                   .packages = c("rcontroll"),.options.snow = opts) %dopar% {
                     sim <- simulations[i]
                     troll(
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
    final_pattern = lapply(stack_res, slot, "final_pattern") %>% 
      bind_rows(.id = "simulation"),
    outputs = lapply(stack_res, slot, "outputs") %>% 
      bind_rows(.id = "simulation")
  )
  
  # unlink stack path with tmp
  if(tmp)
    unlink(path_o)
  
  return(stack_res)
}

.prep_input <- function(input, simulations){
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
