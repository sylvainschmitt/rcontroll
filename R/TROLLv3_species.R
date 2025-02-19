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

# library(tidyverse) # nolint
# TROLLv3_species <- read_csv("inst/extdata/TROLL_75sp_18Feb2025.csv") %>% # nolint
#   select(-`...1`, -sample_size) %>% # nolint
#   na.omit() %>% # nolint
#   rename_at(c("CD_a", "CD_b", "CR_a", "CR_b"), ~ paste0("s_", .)) %>% # nolint
#   mutate(s_regionalfreq = 1/n()) %>% # nolint
#   mutate(s_name = gsub(" ", "_", s_name))
# usethis::use_data(TROLLv3_species, overwrite = T) # nolint
