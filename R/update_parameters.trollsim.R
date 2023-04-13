#' @include trollsim.R
#' @import methods
#' @importFrom dplyr filter select rename left_join
#' @importFrom tibble rownames_to_column
NULL

#' Update global parameters
#'
#' `update_parameters()` update the global parameters used in the `TROLL`
#' simulation from a TROLL outputs for a next simulation. All parameters have a
#' default value used in French Guiana simulations.
#'
#' @param sim trollsim.
#' @param ... parameters to update and their values (see [generate_parameters()]
#'   for a complete list).
#'
#' @return a [data.frame()]
#'
#' @seealso [troll()], [stack()], [generate_parameters()]
#'
#' @examples
#'
#' data("TROLLv3_output")
#' head(update_parameters(TROLLv3_output, iters = 10))
#'
#' @name update_parameters
NULL

#' @rdname update_parameters
#' @export
setGeneric("update_parameters", function(sim, ...) {
  return(standardGeneric("update_parameters"))
})

#' @rdname update_parameters
#' @export
setMethod("update_parameters", "trollsim", function(sim, ...) {
  V1 <- description <- newvalue <- oldvalue <- param <- value <- NULL # nolint
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
