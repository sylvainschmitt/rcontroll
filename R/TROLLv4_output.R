#' `TROLL` output
#'
#' `TROLL` outputs from a 100-year simulation on a 100x100 grid with the other
#' default parameters and using TROLLv3_species, TROLLv3_climatedaytime12, and
#' TROLLv3_daytimevar for use in tests and examples. Ecosystem level output has
#' been thinned and species output has been removed to save disk space.
#'
#' @format A [trollsim()] object.
#'
#' @seealso [TROLLv3_species()], [TROLLv3_climatedaytime12()],
#'   [TROLLv3_daytimevar()], [troll()]
#'
"TROLLv4_output"

# nolint start
# months <- round(seq(1, 365-30, length.out = 12))-1
# thin <- unlist(lapply(1:100-1, function(y) months+y*364))
# thin <- c(0, round(seq(1, 100, length.out = 50))*365-1)
# TROLLv4_output <- load_output("test",
#                               path = "/home/sschmitt/Documents/alt/troll/historical_100y/",
#                               thin = thin)
# TROLLv4_output@ecosystem <- TROLLv4_output@ecosystem %>%
#   dplyr::filter(iter %in% thin)
# TROLLv4_output@species <- data.frame()
# usethis::use_data(TROLLv4_output, overwrite = T)
# nolint end
