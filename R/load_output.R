#' @include trollsim.R
#' @importFrom readr read_tsv cols read_file
#' @importFrom dplyr bind_rows n
#' @importFrom reshape2 melt dcast
NULL

#' Function to load TROLL output
#'
#' @param name char. Name given to the model output
#' @param path char. Path where the model is saved
#'
#' @return an S4 \linkS4class{trollsim} class object
#'
#' @export
#'
#' @examples
#'
#' NA
load_output <- function(name = NULL,
                        path = NULL) {

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

  #species outputs
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

  return(
    trollsim(
      name = name,
      path = path,
      species_outputs = species_outputs,
      inputs = inputs,
      log = log
    )
  )
}
