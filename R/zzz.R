#' Options
#'
#' `rcontroll` package global options including temporary files location and
#' `TROLL version.
#'
#' @param rcontroll.tmp char. Path to temporary files folder used by [troll()]
#'   and [stack()].
#' @param rcontroll.troll char. `TROLL`version number accessible with
#'   [TROLL.version()].
#'
#' @name option.rcontroll
NULL

.onLoad <- function(libname, pkgname) {
  tmp_dir <- file.path(tempdir(), "rcontroll")
  dir.create(tmp_dir)
  options(list(
    rcontroll.tmp = tmp_dir,
    rcontroll.troll = "TROLL version 4.0.1"
  ))
  invisible()
}

.onUnload <- function(libpath) {
  unlink(getOption("rcontroll.tmp"), force = TRUE, recursive = TRUE)
}
