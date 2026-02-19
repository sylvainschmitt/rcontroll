#' `TROLL` species parameters
#'
#' Functional traits used by `TROLL` model for 45 species in French Guiana.
#'
#' @format A data frame with 45 rows and 12 variables: \describe{
#'   \item{s_name}{Species name genus_species} \item{s_LMA}{leaf mass per area}
#'   \item{s_Nmass}{leaf nitrogen mass} \item{s_Pmass}{leaf phosphorus mass}
#'   \item{s_wsg}{wood specific gravity} \item{s_dbhmax}{maximum diameter}
#'   \item{s_hmax}{maximum height} \item{s_ah}{height-diameter allometry
#'   coefficient} \item{s_regionalfreq}{regional frequency} \item{s_tlp}{Turgor
#'   loss point} \item{s_drymass}{leaf dry mass} \item{s_seedmass}{seed mass} } 
#'
#' @seealso [troll()], [stack()]
#'
"TROLLv3_species"

# TROLLv3_species <- readr::read_tsv("inst/extdata/input_species_bruno.txt") # nolint
# TROLLv3_species$s_seedmass=10
# usethis::use_data(TROLLv3_species, overwrite = T) # nolint
