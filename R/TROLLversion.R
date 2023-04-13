#' @include zzz.R
NULL

#' `TROLL` version
#'
#' `TROLL.version()` prints `TROLL` version.
#'
#' @return `TROLL` version in console.
#'
#' @examples
#'
#' TROLL.version()
#'
#' @seealso [option.rcontroll]
#'
#' @export
TROLL.version <- function() { # nolint
  getOption("rcontroll.troll")
}
