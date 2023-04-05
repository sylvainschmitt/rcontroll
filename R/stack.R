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
#' @param pedology df. Daily variation parameters.
#' @param forest df. TROLL with forest input, if null starts from an empty grid
#'   (default NULL).
#' @param soil df. TROLL with soil input, if null starts from an empty grid
#'   (default NULL).
#' @param lidar df. Lidar simulation parameters, if null no computed (default
#'   NULL).
#' @param verbose bool. Show TROLL outputs in the console.
#' @param load bool. TROLL outputs are loaded in R memory, if not only the path
#'   to the outputs is kept.
#' @param cores int. Number of cores for parallelization, if NULL available
#'   cores - 1 (default NULL).
#' @param overwrite bool. Overwrite previous outputs.
#' @param thin int. Vector of integers corresponding to the iterations to be
#'   kept to reduce output size, default is NULL and corresponds to no
#'   thinning.
#' @param date char. Starting date as YYYY/MM/DD, default NULL will result in
#'   non-dated outputs.
#'
#' @return A trollstack object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data("TROLLv4_input")
#' data("TROLLv4_species")
#' data("TROLLv4_climate")
#' data("TROLLv4_dailyvar")
#' data("TROLLv4_pedology")
#' TROLLv4_input_stack <- generate_parameters(nbiter = 10) %>%
#'   mutate(simulation = list(c("seed50000", "seed500"))) %>%
#'   unnest(simulation)
#' TROLLv4_input_stack[65, 2] <- 500 # Cseedrain
#' stack(
#'   name = "teststack",
#'   simulations = c("seed50000", "seed500"),
#'   path = getwd(),
#'   global = TROLLv4_input_stack,
#'   species = TROLLv4_species,
#'   climate = TROLLv4_climate,
#'   daily = TROLLv4_dailyvar,
#'   pedology = TROLLv4_pedology,
#'   load = TRUE,
#'   date = "2004/01/01",
#'   cores = 2
#' )
#' }
#'
stack <- function(name = NULL,
                  simulations,
                  path = NULL,
                  global,
                  species,
                  climate,
                  daily,
                  pedology,
                  forest = NULL,
                  soil = NULL,
                  lidar = NULL,
                  verbose = TRUE,
                  load = TRUE,
                  cores = NULL,
                  overwrite = TRUE,
                  thin = NULL,
                  date = NULL) {
  # cores
  if (is.null(cores)) {
    cores <- detectCores()
    message("Detect cores was not defined, ", cores, " cores will be used.")
  }
  if ((detectCores()) < cores) {
    cores <- detectCores()
    warning(paste(
      "It seems you attributed more cores than your CPU has! 
      Automatic reduction to",
      cores, "cores."
    ))
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
          quiet = TRUE
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
  if(tmp && !load) {
    stop("You can not unactivate the load option if you have not defined a path for your files.")
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

  # inputs
  global <- .prep_input(global, simulations)
  species <- .prep_input(species, simulations)
  climate <- .prep_input(climate, simulations)
  daily <- .prep_input(daily, simulations)
  pedology <- .prep_input(pedology, simulations)
  if (!is.null(forest)) {
    forest <- .prep_input(forest, simulations)
  } else {
    forest <- lapply(simulations, function(x) forest)
    names(forest) <- simulations
  }
  if (!is.null(soil)) {
    soil <- .prep_input(soil, simulations)
  } else {
    soil <- lapply(simulations, function(x) soil)
    names(soil) <- simulations
  }
  if (!is.null(lidar)) {
    lidar <- .prep_input(lidar, simulations)
  } else {
    lidar <- lapply(simulations, function(x) lidar)
    names(lidar) <- simulations
  }
  if (tmp) {
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
  stack_res <- foreach(
    i = 1:length(simulations), .export = ".troll_child",
    .options.snow = opts
  ) %dopar% {
    sim <- simulations[i]
    .troll_child(
      name = sim,
      path = sim_path[[sim]],
      global = global[[sim]],
      species = species[[sim]],
      climate = climate[[sim]],
      daily = daily[[sim]],
      pedology = pedology[[sim]],
      forest = forest[[sim]],
      soil = soil[[sim]],
      lidar = lidar[[sim]],
      verbose = verbose,
      load = load,
      overwrite = overwrite,
      thin = thin,
      date = date
    )
  }
  close(pb)
  stopCluster(cl)
  stop("We should use load_stack to avoid repetition.")
  names(stack_res) <- simulations
  stack_res <- trollstack(
    name = stack_res[[1]]@name,
    path = path_o,
    mem = FALSE
  )
  if(load){
    stack_res <- load_sim(stack)
  }
  return(stack_res)
}

.prep_input <- function(input, simulations) {
  simulation <- NULL

  if ("simulation" %in% colnames(input)) {
    if (!all(as.character(unique(input$simulation)) %in% simulations)) {
      stop("Simulations names in your inputs don't
           match indicated simulations names.")
    }
    input <- split(input, input$simulation)
    input <- lapply(input, select, -simulation)
  } else {
    input <- lapply(simulations, function(x) input)
    names(input) <- simulations
  }
  return(input)
}
