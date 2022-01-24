#' TROLL output
#'
#' TROLL outputs from a 500 years simulations with default parameters using
#' TROLLv3_species,  TROLLv3_climatedaytime12, and TROLLv3_daytimevar to be used
#' in tests and examples.
#'
#' @format A trollsim object.
#'   
"TROLLv3_output"

# data("TROLLv3_species")
# data("TROLLv3_climatedaytime12")
# data("TROLLv3_daytimevar")
# TROLLv3_output <- troll(name = "test",
#                         path = "/home/sylvain/Documents/ECOFOG/",
#                         global = generate_parameters(iterperyear = 12,
#                                                      nbiter = 12*500),
#                         species = TROLLv3_species,
#                         climate = TROLLv3_climatedaytime12,
#                         daily = TROLLv3_daytimevar,
#                         verbose = T)
# save(TROLLv3_output, file = "/home/sylvain/Documents/ECOFOG/TROLLv3_output.Rdata")
# load("/home/sylvain/Documents/ECOFOG/TROLLv3_output.Rdata")
# usethis::use_data(TROLLv3_output, overwrite = T)
