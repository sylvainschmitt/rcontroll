#' TROLL output
#'
#' TROLL outputs from a 100-year simulation on a 100x100 grid with the other
#' default parameters and using TROLLv3_species, TROLLv3_climatedaytime12, and
#' TROLLv3_daytimevar for use in tests and examples. Ecosystem level output has
#' been thinned and species output has been removed to save disk space.
#'
#' @format A trollsim object.
#'
"TROLLv3_output"

# data("TROLLv3_species")
# data("TROLLv3_climatedaytime12")
# data("TROLLv3_daytimevar")
# TROLLv3_output <- troll(name = "test",
#                         path = "/home/sylvain/Documents/ECOFOG/",
#                         global = generate_parameters(
#                           rows = 100,
#                           cols = 100,
#                           iterperyear = 12,
#                           nbiter = 12*100),
#                         species = TROLLv3_species,
#                         climate = TROLLv3_climatedaytime12,
#                         daily = TROLLv3_daytimevar,
#                         verbose = T)
# thin <- c(0, round(seq(1, 100, length.out = 50))*12-1)
# TROLLv3_output@ecosystem <- TROLLv3_output@ecosystem %>%
#   dplyr::filter(iter %in% thin)
# TROLLv3_output@species <- data.frame()
# forest <- TROLLv3_output@forest
# autoplot(TROLLv3_output, "temporal")
# usethis::use_data(TROLLv3_output, overwrite = T)
