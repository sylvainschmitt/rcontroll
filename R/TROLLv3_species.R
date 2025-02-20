#' `TROLL` species parameters
#'
#' Functional traits used by `TROLL` model for 70 species in Central Africa
#' gathered by Nicholas Russo (Harvard).
#'
#' @format A data frame with 45 rows and 12 variables: \describe{
#'   \item{s_name}{Species name genus_species} \item{s_LMA}{leaf mass per area}
#'   \item{s_Nmass}{leaf nitrogen mass} \item{s_Pmass}{leaf phosphorus mass}
#'   \item{s_wsg}{wood specific gravity} \item{s_dbhmax}{maximum diameter}
#'   \item{s_hmax}{maximum height} \item{s_ah}{height-diameter allometry
#'   coefficient} \item{s_CD_a}{crown depth allometry intercept} 
#'   \item{s_CD_b}{crown depth allometry slope} \item{s_CR_a}{crown radius 
#'   allometry intercept} \item{s_CR_b}{crown radius allometry slope} }
#'
#' @seealso [troll()], [stack()]
#' 
"TROLLv3_species"
# 
# library(tidyverse)
# TROLLv3_species <- read_tsv("https://raw.githubusercontent.com/sylvainschmitt/rcontroll/refs/heads/dev/inst/extdata/TROLLv3_species.txt") %>%
#   mutate(s_CD_a = 0,
#          s_CD_b = 0.2,
#          s_CR_a = 2.13,
#          s_CR_b = 0.63)
# usethis::use_data(TROLLv3_species, overwrite = T) # nolint

