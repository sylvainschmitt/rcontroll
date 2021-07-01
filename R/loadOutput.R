#' @include TROLLsim.R
#' @importFrom readr read_tsv cols read_file
NULL

#' Function to load TROLL output
#'
#' @param name char. Name given to the model output
#' @param path char. Path where the model is saved
#'
#' @return an S4 \linkS4class{TROLLsim} class object
#'
#' @export
#'
#' @examples
#' 
#' NA
#'
loadOutput <- function(name = NULL, 
                       path = NULL){

  #### Inputs ####
  general <- read_tsv(file.path(path, paste0(name, '_input_general.txt')), col_types = cols())
  species <- read_tsv(file.path(path, paste0(name, '_input_species.txt')), col_types = cols())
  climate <- read_tsv(file.path(path, paste0(name, '_input_climate.txt')), col_types = cols())
  daily <- read_tsv(file.path(path, paste0(name, '_input_daily.txt')), col_types = cols())
  soil <- read_tsv(file.path(path, paste0(name, '_input_soil.txt')), col_types = cols())
  
  #### Model  ####
  info <- read_file(file.path(path, paste0(name, '_0_info.txt')))
  par <- NULL
  log <- read_file(file.path(path, paste0(name, '_log.txt')))

  #### AGB/BA  ####
  agb <- read_tsv(file.path(path, paste0(name, '_0_agb.txt')),
                    col_names = c(species$s_name, "total"), col_types = cols())
  ba <- read_tsv(file.path(path, paste0(name, '_0_ba.txt')),
                    col_names = c("iter", species$s_name, "total"), col_types = cols())
  ba10 <- read_tsv(file.path(path, paste0(name, '_0_ba10.txt')),
                    col_names = c("iter", species$s_name, "total"), col_types = cols())

  #### Abundances  ####
  abund <- read_tsv(file.path(path, paste0(name, '_0_abund.txt')),
                   col_names = c("iter", species$s_name, "total"), col_types = cols())
  abu10 <- read_tsv(file.path(path, paste0(name, '_0_abu10.txt')),
                     col_names = c("iter", species$s_name, "total"), col_types = cols())
  abu30 <- read_tsv(file.path(path, paste0(name, '_0_abu30.txt')),
                    col_names = c("iter", species$s_name, "total"), col_types = cols())
  
  #### GPP/NPP ####
  gpp <- read_tsv(file.path(path, paste0(name, '_0_gpp.txt')),
                    col_names = c("iter", species$s_name, "total"), col_types = cols())
  npp <- read_tsv(file.path(path, paste0(name, '_0_npp.txt')),
                    col_names = c("iter", species$s_name, "total"), col_types = cols())
  
  #### Respiration  ####
  rday <- read_tsv(file.path(path, paste0(name, '_0_Rday.txt')),
                   col_names = c("iter", species$s_name, "total"), col_types = cols())
  rnight <- read_tsv(file.path(path, paste0(name, '_0_Rnight.txt')),
                   col_names = c("iter", species$s_name, "total"), col_types = cols())
  rstem <- read_tsv(file.path(path, paste0(name, '_0_Rnight.txt')),
                     col_names = c("iter", species$s_name, "total"), col_types = cols())
  
  #### Litterfall ####
  litterfall <- read_tsv(file.path(path, paste0(name, '_0_litterfall.txt')),
                         col_types = cols()) # broken
  
  #### PPFD0 ####
  ppfd0 <- read_tsv(file.path(path, paste0(name, '_0_ppfd0.txt')),
                    col_names = c("iter", "measure", "mean", "var"), col_types = cols())
  
  #### Death ####
  death <- read_tsv(file.path(path, paste0(name, '_0_death.txt')), 
                    col_names = paste0("X", 1:6), col_types = cols()) # col to be defined
  death1 <- data.frame() # not understood
  death2 <- data.frame() # not understood
  death3 <- data.frame() # not understood
  deathrate <- read_tsv(file.path(path, paste0(name, '_0_deathrate.txt')),
                        col_names = paste0("X", 1:4), col_types = cols()) # col to be defined
  
  #### Final pattern ####
  final_pattern <- read_tsv(file.path(path, paste0(name, '_0_final_pattern.txt')), col_types = cols())
  # to spatial?
  
  #### Vertd ####
  vertd <- read_tsv(file.path(path, paste0(name, '_0_vertd.txt')),
                    col_names = c('height', 'vertd'), col_types = cols())

  #### ABC  ####
  # not with full outputs
  
  #### Not implemented  ####
  nddfield <- NULL
  cica <- NULL
  dbh <- NULL
  hundredyears <- NULL
  leafdens1 <- NULL
  leafdens2 <- NULL
  leafdens3 <- NULL
  site1 <- NULL
  site2 <- NULL
  site3 <- NULL
  site4 <- NULL
  site5 <- NULL
  sp_par <- NULL
  state <- NULL
  
  #### Output ####
  x <- TROLLsim(
    name = name,
    path = path,
    abundances = list(abund  = abund,
                      abu10 = abu10,
                      abu30 = abu30),
    agb = agb,
    ba = list(ba = ba, ba10 = ba10),
    death = list(death = death,
                 death1 = death1,
                 death2 = death2,
                 death3 = death3,
                 deathrate = deathrate),
    final_pattern = final_pattern,
    gpp = gpp,
    info = info,
    litterfall = litterfall,
    npp = npp,
    inputs = list(general = general,
                  species = species,
                  climate = climate,
                  daily = daily,
                  soil = soil),
    ppfd0 = ppfd0,
    R = list(rday = rday, rnight = rnight, rstem = rstem),
    vertd = vertd)

  return(x)
}
