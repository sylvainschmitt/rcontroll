#' @include trollsim.R
#' @import methods
#' @importFrom lidR rasterize_canopy LAS p2r
#' @importFrom terra focal
NULL

#' Extract canopy height model
#'
#' `get_chm()` extract the canopy height model from `TROLL` outputs with lidar
#' option.
#'
#' @param sim trollsim.
#' @param method char. method to extract the canopy height model from the point
#'   cloud in las, either 'filled' (replacing NA by 0) or 'smoothed' (local
#'   means, default value).
#' @param ... unused argument.
#'
#' @return [data.frame()]
#'
#' @seealso [trollsim()], [troll()], [stack()]
#'
#' @name get_chm
NULL

#' @rdname get_chm
#' @export
setGeneric("get_chm", function(sim, method = "smoothed", ...) {
  return(standardGeneric("get_chm"))
})

#' @rdname get_chm
#' @export
setMethod("get_chm", "trollsim", function(sim, method = "smoothed", ...) {
  # check las existence
  if (length(sim@las) == 0) {
    stop("The TROLL outputs does not contain a las from lidar simulation.")
  }

  # check method
  if (!(method %in% c("filled", "smoothed"))) {
    stop("method should be filled, or smoothed")
  }

  chm <- lapply(sim@las, rasterize_canopy, res = 1, algorithm = p2r())

  fill_na <- function(x, i = 5) {
    if (is.na(x)[i]) {
      return(mean(x, na.rm = TRUE))
    } else {
      return(x[i])
    }
  }
  w <- matrix(1, 3, 3)

  if (method == "filled") {
    chm <- lapply(chm, focal, w, fun = fill_na)
  }
  if (method == "smoothed") {
    chm <- lapply(chm, focal, w, fun = mean, na.rm = TRUE)
  }

  # changes names
  chm <- lapply(chm, function(x) {
    names(x) <- "canopy_height"
    return(x)
  })

  if (inherits(sim, "trollsim")) {
    names(chm) <- sim@name
  }
  if (inherits(sim, "trollstack")) {
    names(chm) <- unique(sim@inputs$global$simulation)
  }

  return(chm)
})
