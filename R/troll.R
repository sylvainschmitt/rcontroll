#' @include load_output.R
#' @importFrom readr write_tsv
#' @importFrom sys exec_wait
#' @importFrom utils timestamp
NULL

#' TROLL
#'
#' Run a TROLL simulation
#'
#' @param name char. model name (if NULL timestamp)
#' @param path char. path to the simulation (tmp if NULL)
#' @param full bool. TROLL with full outputs, if not reduced outputs (default
#'   TRUE)
#' @param abc bool. TROLL with abc outputs, forcing reduced outputs (default
#'   FALSE)
#' @param random bool. TROLL with random outputs, if not the seed is fixed
#'   (default TRUE)
#' @param global df. global parameters
#' @param species df. species parameters
#' @param climate df. climate parameters
#' @param daily df. daily variation parameters
#' @param forest df. TROLL with forest input, if null start from an empty grid
#'   (default NULL)
#' @param overwrite bool. overwrite previous outputs
#'
#' @return trollsim
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
                  random = TRUE,
                  global,
                  species,
                  climate,
                  daily,
                  forest = NULL,
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
  if(!all(unlist(lapply(list(full, random, abc, overwrite), class)) == "logical"))
    stop("full, random, abc, and overwrite should be logical.")
  if(!all(unlist(lapply(list(name, path), class)) %in% c("character", "NULL")))
    stop("name and path should be character or null.")
  if(!all(unlist(lapply(list(global, species, climate, daily), inherits, c("data.frame", "NULL")))))
    stop("global, species, climate, and daily should be a data frame.")
  if(!(class(forest) %in% c("data.frame", "NULL")))
    stop("forest should be a data frame or null.")
  
  if(!is.null(forest))
    stop("forest not implemented yet!")
  
  # model name
  if (is.null(name)) {
    name <- paste0(
      "sim_",
      gsub(":", "-",
      gsub(
        " ", "_",
        timestamp(
          prefix = "",
          suffix = "",
          quiet = T
        )
      ))
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
  
  # type
  if(full & abc)
    stop("ABC and full outputs are not compatible.")
  if(full & !abc)
    type <- "full"
  if(!full & !abc)
    type <- "reduced"
  if(!full & abc)
    type <- "abc"
  
  # DEV #
  if(!full | abc | !random)
    stop("This is the Rcpp branch only including standard full random non abc TROLL.")
  
  # save input as files
  global_path <- file.path(path, name, paste0(name, "_input_global.txt"))
  species_path <- file.path(path, name, paste0(name, "_input_species.txt"))
  climate_path <- file.path(path, name, paste0(name, "_input_climate.txt"))
  daily_path <- file.path(path, name, paste0(name, "_input_daily.txt"))
  if(!is.null(forest))
    forest_path <- file.path(path, name, paste0(name, "_input_forest.txt"))
  write_tsv(global, file = global_path)
  write_tsv(species, file = species_path)
  write_tsv(climate, file = climate_path)
  write_tsv(daily, file = daily_path)
  if(!is.null(forest))
    write_tsv(forest, file = forest_path)
  if(!random)
    cat("nonrandom", file = file.path(path, name, paste0(name, "_nonrandom.txt")))
  
  # run
  log <- capture.output(
    trollCpp(global_file = global_path, 
             climate_file = climate_path, 
             species_file = species_path, 
             day_file = daily_path, 
             output_file = file.path(path, name, name)
    ))
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
    "info",
    "leafdens1",
    "leafdens2",
    "leafdens3",
    "NDDfield",
    "litterfall",
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
    sim@path <- character()
  }
  
  return(sim)
}
