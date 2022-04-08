#' @include trollsim.R
#' @import methods
#' @importFrom dplyr filter select rename left_join
#' @importFrom tibble rownames_to_column
NULL

#' Function to updae parameters from a TROLL outputs for a next simlation.
#'
#' @param sim trollsim.
#' @param ... parameters to update and their values..
#'
#' @return data.frame
#'
#' @examples
#' 
#' data("TROLLv3_output")
#' head(get_forest(TROLLv3_output))
#' 
#' @name update_parameters
NULL

#' @rdname update_parameters
#' @export
setGeneric('update_parameters', function(sim, ...) {return(standardGeneric('update_parameters'))})

#' @rdname update_parameters
#' @export
setMethod("update_parameters", "trollsim", function(sim, ...) {
  V1 <- description <- newvalue <- oldvalue <- param <- value <- NULL
  sim@inputs$global %>% 
    rename(oldvalue = value) %>% 
    left_join(list(...) %>% 
                as.data.frame() %>% 
                t() %>% 
                as.data.frame() %>% 
                rownames_to_column("param") %>% 
                rename(newvalue = V1), by = "param") %>% 
    mutate(value = ifelse(is.na(newvalue), oldvalue, newvalue)) %>% 
    select(param, value, description)
})
