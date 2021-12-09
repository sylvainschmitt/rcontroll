#' @include trollsim.R
#' @importFrom readr read_tsv cols read_file
#' @importFrom dplyr bind_rows n
#' @importFrom reshape2 melt dcast
NULL

#' Function to load TROLL output
#'
#' @param name char. name given to the model output
#' @param path char. path where the model is saved
#' @param full bool. TROLL with full outputs, if not reduced outputs (default
#'   TRUE)
#'
#' @return an S4 \linkS4class{trollsim} class object
#'
#' @export
#'
#' @examples
#'
#' NA
load_output <- function(name,
                        path,
                        full) {
  # Check inputs
  if(!all(unlist(lapply(list(name, path), class)) %in% c("character")))
    stop("name and path should be character.")
  if(!all(unlist(lapply(list(full), class)) %in% c("logical")))
    stop("full should be logical.")

  # troll sim
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
  forest <- file.path(path, paste0(name, paste0("_input_forest.txt")))
  forestop <- file.exists(forest)
  inputs$forest <- NULL
  if(forestop)
    inputs$forest <- read_tsv(forest, col_types = cols())
  random <- !file.exists(file.path(path, paste0(name, paste0("_nonrandom.txt"))))
  parameters <- inputs$global$value
  names(parameters) <- inputs$global$param
  parameters['forest'] <- as.numeric(forestop)
  parameters['random'] <- as.numeric(random)
  log <- read_file(file.path(path, paste0(name, "_log.txt")))
  final_pattern <- read_tsv(file.path(path, paste0(name, paste0("_0_final_pattern.txt"))),
                            col_types = cols())
  
  if(full)
    return(
      trollsim(
        name = name,
        path = path,
        parameters = parameters,
        inputs = inputs,
        log = log,
        final_pattern = final_pattern,
        outputs = .load_species_outputs(name, path, inputs = inputs)
      )
    )
  else
    return(
      trollsim(
        name = name,
        path = path,
        parameters = parameters,
        inputs = inputs,
        log = log,
        final_pattern = final_pattern,
        outputs = .load_reduced_outputs(name, path)
      )
    )
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
    agb = "agb"
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
  reduced_outputs <- read_tsv(file.path(path, paste0(name, "_0_", "outputs", ".txt")),
                              col_names = c("iter", "N", "N10", "N30", "BA10", "NPP", "GPP", "AGB"),
                              col_types = cols())
  return(reduced_outputs)
}
