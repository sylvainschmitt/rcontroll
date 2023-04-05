#' TROLL pedology parameters
#'
#' Pedology parameters used by TROLL model for 5 soil layers in French Guiana.
#'
#' @format A data frame with 5 rows and 8 variables: \describe{
#'   \item{layer_thickness}{layer thickness in m}
#'   \item{proportion_Silt}{proportion of silt in percent}
#'   \item{proportion_Clay}{proportion of clay in percent}
#'   \item{proportion_Sand}{proportion of sand in percent} \item{SOC}{soil
#'   organic content} \item{DBD}{soil dry bulk density} \item{pH}{soil pH}
#'   \item{CEC}{soil cation exchange capacity}}
#' 
"TROLLv4_pedology"

# TROLLv4_pedology <- readr::read_tsv("inst/extdata/TROLLv4_pedology.txt")
# usethis::use_data(TROLLv4_pedology, overwrite = T)
