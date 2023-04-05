#' @include trollsim.R
#' @import methods
#' @importFrom dplyr select
#' @importFrom reshape2 dcast
NULL

#' Function to get a forest inventory from TROLL outputs.
#'
#' @param sim trollsim.
#' @param ... unused argument.
#'
#' @return data.frame
#'
#' @examples
#'
#' data("TROLLv3_output")
#' head(get_forest(TROLLv3_output))
#'
#' @name get_soil
NULL

#' @rdname get_soil
#' @export
setGeneric("get_soil", function(sim, ...) {
  return(standardGeneric("get_soil"))
})

#' @rdname get_soil
#' @export
setMethod("get_soil", "trollsim", function(sim, ...) {
  dcell <- layer <- swc <- NULL
  select(sim@soil, dcell, layer, swc) %>% 
    dcast(dcell ~ layer, value.var = "swc") %>%
    as.data.frame()
})
