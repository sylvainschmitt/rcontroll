#' TROLL species parameters
#'
#' Functional traits used by TROLL model for 109 species in French Guiana.
#'
#' @format A data frame with 109 rows and 11 variables: \describe{
#'   \item{s_name}{Species name genus_species} \item{s_LMA}{leaf mass per area}
#'   \item{s_Nmass}{leaf nitrogen mass} \item{s_Pmass}{leaf phosphorus mass}
#'   \item{s_wsg}{wood specific gravity} \item{s_dbhmax}{maximum diameter}
#'   \item{s_hmax}{maximum height} \item{s_ah}{height-diameter allometry
#'   coefficient} \item{s_regionalfreq}{regional frequency} \item{s_tlp}{Turgor
#'   loss point} \item{s_leafarea}{leaf area}}
#'
"TROLLv4_species"

# TROLLv4_species <- readr::read_tsv("inst/extdata/TROLLv4_species.txt")
# usethis::use_data(TROLLv4_species, overwrite = T)
