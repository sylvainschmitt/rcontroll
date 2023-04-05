#' @include trollsim.R
#' @import methods
NULL

#' Function to date outputs from a TROLL simulation based on a starting date.
#'
#' @param sim trollsim.
#' @param date char. date as a string YYYY/MM/DD.
#' @param ... unused argument.
#'
#' @return An S4 \linkS4class{trollsim} class object.
#'
#' @name date_sim
NULL

#' @rdname date_sim
#' @export
setGeneric("date_sim", function(sim, date, ...) {
  return(standardGeneric("date_sim"))
})

#' @rdname date_sim
#' @export
setMethod("date_sim", "trollsim", function(sim, ...) {
  sim@ecosystem$date <- sim@ecosystem$iter + as_date(date)
  sim@species$date <- sim@species$iter + as_date(date)
  return(sim)
})
