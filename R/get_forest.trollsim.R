#' @include trollsim.R
#' @import methods
#' @importFrom dplyr filter select
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
#' @name get_forest
NULL

#' @rdname get_forest
#' @export
setGeneric('get_forest', function(sim, ...) {return(standardGeneric('get_forest'))})

#' @rdname get_forest
#' @export
setMethod("get_forest", "trollsim", function(sim, ...) {
  iter <- from_Data <- sp_lab <- site <- dbh_previous <- AGB <- NULL
  filter(sim@forest, 
         iter == max(sim@forest$iter)) %>% 
    select(-iter, -from_Data, -sp_lab, -site, -dbh_previous, -AGB) %>% 
    as.data.frame()
})
