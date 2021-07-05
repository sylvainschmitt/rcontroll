#' Options
#'
#' rcontroll package global options
#'
#' @param rcontroll.tmp char. path to temporary files folder
#'
#' @name option.rcontroll
NULL

.onLoad <- function(libname, pkgname) {
  tmp_dir <- file.path(tempdir(), "rcontroll")
  dir.create(tmp_dir)
  options(list(rcontroll.tmp = tmp_dir))
  invisible()
}

.onUnload <- function(libpath) {
  unlink(getOption("rcontroll.tmp"), force = TRUE, recursive = TRUE)
}
