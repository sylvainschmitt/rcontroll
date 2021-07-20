#' @include load_output.R
#' @importFrom readr write_tsv
#' @importFrom sys exec_wait
#' @importFrom utils timestamp
NULL

#' TROLL
#'
#' Run a TROLL simulation
#'
#' @param name char. model name
#' @param path char. path to the simulation (tmp if NULL)
#' @param full bool. TROLL with full outputs, if not reduced outputs (default
#'   TRUE)
#' @param abc bool. TROLL with abc outputs, forcing reduced outputs (default
#'   FALSE)d
#' @param global df. global parameters
#' @param species df. species parameters
#' @param climate df. climate parameters
#' @param daily df. daily variation parameters
#' @param overwrite bool. overwrite previous outputs
#'
#' @return simulation outputs in the path folder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' example
#' }
#' 
troll <- function(name = NULL,
                  path = NULL,
                  full = TRUE,
                  abc = FALSE,
                  global = data.frame(),
                  species = data.frame(),
                  climate = data.frame(),
                  daily = data.frame(),
                  overwrite = TRUE) {
  # for tests
  # data("TROLLv3_input")
  # data("TROLLv3_species")
  # data("TROLLv3_climatedaytime365")
  # data("TROLLv3_daytimevar")
  # TROLLv3_input$value[5] <- 10 # iterations
  # global <- TROLLv3_input
  # species <- TROLLv3_species 
  # climate <- TROLLv3_climatedaytime12
  # daily <- TROLLv3_daytimevar
  # full = F
  # abc = T
  # path <- getwd()

  # check all inputs

  # model name
  if (is.null(name)) {
    name <- paste0(
      "sim_",
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

  # model path
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

  # troll exe
  os <- .Platform$OS.type
  troll_path <- system.file("troll", os, package = "rcontroll")
  ext <- switch(os, 
                "unix" = ".out", 
                "win" = ".exe")
  
  
  if(full & abc)
    stop("ABC and full outputs are not compatible.")
  
  type <- NULL
  if(full & !abc){
    message("Simualtion with full outputs.")
    type <- "full"
    troll <- file.path(troll_path, paste0("TROLL_full", ext))
  }
  if(!full & !abc){
    message("Simualtion with reduced outputs.")
    type <- "reduced"
    troll <- file.path(troll_path, paste0("TROLL_reduced", ext))
  }
  if(!full & abc){
    message("Simualtion with ABC outputs.")
    type <- "abc"
    troll <- file.path(troll_path, paste0("TROLL_abc", ext))
  }
  if(is.null(type))
    stop("TROLL executable was not found.")

  # save input as files
  global_path <- file.path(path, name, paste0(name, "_input_global.txt"))
  species_path <- file.path(path, name, paste0(name, "_input_species.txt"))
  climate_path <- file.path(path, name, paste0(name, "_input_climate.txt"))
  daily_path <- file.path(path, name, paste0(name, "_input_daily.txt"))
  write_tsv(global, file = global_path)
  write_tsv(species, file = species_path)
  write_tsv(climate, file = climate_path)
  write_tsv(daily, file = daily_path)

  # command
  command <- paste0(
    troll,
    " -i", global_path,
    " -s", species_path,
    " -m", climate_path,
    " -d", daily_path,
    " -o", file.path(path, name, name)
  )
  message(command)

  # run
  log <- system(command, intern = TRUE)
  write(log, file.path(path, name, paste0(name, "_log.txt")))

  # cleaning outputs
  lapply(list(
    "100yearsofsolitude",
    "abc_biomass",
    "chm_potential",
    "abc_LAI",
    "abc_PAIfield",
    "abc_PAIfieldALS",
    "abc_species",
    "abc_species10",
    "abc_traitconservation",
    "abc_traits",
    "abc_traits10",
    "chm",
    "cica",
    "dbh",
    "death", # not using it for the moment
    "death1",
    "death2",
    "death3",
    "deathrate", # not using it for the moment
    "final_pattern", # not using it for the moment
    "info",
    "leafdens1",
    "leafdens2",
    "leafdens3",
    "NDDfield",
    "par",
    "site1",
    "ppfd0", # not using it for the moment
    "site2",
    "site3",
    "site4",
    "site5",
    "site6",
    "sp_par",
    "state",
    "vertd" # not using it for the moment
  ), function(x) {
    unlink(file.path(path, name, paste0(name, "_0_", x, ".txt")))
  })
  
  # loading outputs
  sim <- load_output(name, file.path(path, name), type)
  if (tmp) {
    unlink(file.path(path, name), recursive = TRUE, force = TRUE)
  } # set path to null

  return(sim)
}
