#' @include trollsim.R trollsimfull.R trollsimreduced.R trollsimabc.R
#' @importFrom readr read_tsv cols read_file
#' @importFrom dplyr bind_rows n
#' @importFrom reshape2 melt dcast
NULL

#' Function to load TROLL output
#'
#' @param name char. name given to the model output
#' @param path char. path where the model is saved
#' @param type char. type of simulation (full, reduce or abc)
#'
#' @return an S4 \linkS4class{trollsim} class object
#'
#' @export
#'
#' @examples
#'
#' NA
load_output <- function(name = NULL,
                        path = NULL,
                        type = NULL) {
  # Check inputs

  #inputs and log
  inputs <- lapply(
    list(
      global = "global",
      species = "species",
      climate = "climate",
      daily = "daily"
    ),
    function(x) {
      read_tsv(file.path(path, paste0(name, paste0("_input_", x, ".txt"))),
        col_types = cols()
      )
    }
  )
  inputs$forest <- data.frame()
  log <- read_file(file.path(path, paste0(name, "_log.txt")))


  sim <- switch(type,
                
                "full" =   trollsimfull(
                  name = name,
                  path = path,
                  species_outputs = .load_species_outputs(name, path, inputs = inputs),
                  inputs = inputs,
                  log = log
                ),
                
                "reduced" =   trollsimreduced(
                  name = name,
                  path = path,
                  reduced_outputs = .load_reduced_outputs(name, path),
                  inputs = inputs,
                  log = log
                ),
                
                "abc" =   trollsimabc(
                  name = name,
                  path = path,
                  reduced_outputs = .load_reduced_outputs(name, path),
                  abc_outputs = .load_abc_outputs(name, path),
                  inputs = inputs,
                  log = log
                )
                
  )
  
    
  return(sim)
}

# Internals

.load_species_outputs <- function(name = NULL,
                                  path = NULL,
                                  inputs = NULL){
  species_outputs1 <- lapply(list(
    ba = "ba",
    ba10 = "ba10",
    abund = "abund",
    abu10 = "abu10",
    abu30 = "abu30",
    gpp = "gpp",
    npp = "npp",
    rday = "Rday",
    rnight = "Rnight",
    rstem = "Rstem"
  ), function(x) {
    read_tsv(file.path(path, paste0(name, "_0_", x, ".txt")),
             col_names = c("iter", inputs$species$s_name, "total"),
             col_types = cols()
    ) %>%
      melt("iter", variable.name = "species")
  }) %>%
    bind_rows(.id = "variable")
  
  species_outputs2 <- lapply(list(
    agb = "agb",
    litterfall = "litterfall"
  ), function(x) {
    read_tsv(file.path(path, paste0(name, "_0_", x, ".txt")),
             col_names = c(inputs$species$s_name, "total"),
             col_types = cols()
    ) %>%
      mutate(iter = 0:(n() - 1)) %>%
      melt("iter", variable.name = "species")
  }) %>%
    bind_rows(.id = "variable")
  
  species_outputs <- bind_rows(
    species_outputs1,
    species_outputs2
  ) %>%
    dcast(iter + species ~ variable)
  
  return(species_outputs)
}

.load_reduced_outputs <- function(name = NULL,
                                  path = NULL){
  warning("Colnames to be defined.")
  reduced_outputs <- read_tsv(file.path(path, paste0(name, "_0_", "outputs", ".txt")),
                              col_names = F,
                              col_types = cols())
  return(reduced_outputs)
}

.load_abc_outputs <- function(name = NULL,
                              path = NULL){
  warning("Colnames to be defined and outputs to be consolidated.")
  abc_outputs <- lapply(c("chm", "chmALS", "chmpotential", "ground",
                          "transmittance", "transmittanceALS"),
                        function(x)
                          read_tsv(
                            file.path(path, paste0(name, "_0_abc_", "chm", ".txt"),
                                      col_names = F,
                                      col_types = cols()
                            )))
  return(abc_outputs)
}
