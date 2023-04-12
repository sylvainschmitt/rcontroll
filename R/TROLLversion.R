#' @include zzz.R
NULL

#' Function to print TROLL version.
#'
#' @return Print TROLL version in console.
#'
#' @examples
#'
#' TROLL.version()
#'
#' @export
#'
TROLL.version <- function() { # nolint
  getOption("rcontroll.troll")
}
