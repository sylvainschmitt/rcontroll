#' An S4 class to represent TROLL outputs
#'
#' This is an S4 class to represent TROLL outputs in stacked format (stack of TROLLsim objects)
#'
#' @slot structured logical. indicates whether stacked simulations have the same dimensions or not
#' @slot aggregated logical. indicates whether stacked simulations are saved in compressed format (mean, sd)
#' @slot layers list. TROLLsim objects
#' @slot nbcols int. number of columns
#' @slot  nbrows int. number of rows
#' @slot nbiter int. total nb of timesteps
#' @slot iter int. number of iteration per year
#' @slot NV int. vertical nb of cells (nb per m)
#' @slot NH int. horizontal nb of cells (nb per m)
#'
#' @export
setClass('TROLLsimstack',
         representation(
           structured = 'logical',
           aggregated = 'logical',
           layers = 'list',
           nbcols = 'integer',
           nbrows = 'integer',
           nbiter = 'integer',
           iter = 'integer',
           NV = 'integer',
           NH = 'integer'
         ),
         prototype(
           structured = FALSE,
           aggregated = FALSE,
           layers = list(),
           nbcols = integer(),
           nbrows = integer(),
           nbiter = integer(),
           iter = integer(),
           NV = integer(),
           NH = integer()
         )
)

# functions to initialise class
# ToDo: move to their own file
# ToDo: subclasses? structured/compressed

