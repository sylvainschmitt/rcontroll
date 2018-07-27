#' RconTROLL
#'
#' Function and code to manage and use indivdual-based and spatially-explicit
#' forest growth model TROLL.
#'
#' @section Modelling main  functions: \describe{
#'   \item{\code{\link{init}}}{Create the parameter file for the TROLL program}
#'   \item{\code{\link{run}}}{Launch a simulation}
#'   \item{\code{\link{loadOutput}}}{Load TROLL output}
#'   \item{\code{\link{loadStack}}}{Load simulation stack}
#'   \item{\code{\link{compareSim}}}{Compare whether two or more TROLLsim
#'   objects have been executed with the same set of general parameters. Please
#'   note: This function does not compare output} }
#'
#' @section Model main methods: \describe{
#'   \item{\code{\link{print,TROLLsim-method}}}{Print TROLL outputs}
#'   \item{\code{\link{summary,TROLLsim-method}}}{Provide a summary of TROLL
#'   outputs} \item{\code{\link{plot.TROLLsim}}}{Plot TROLL simulations}
#'   \item{\code{\link{stack,TROLLsim-method}}}{Create TROLL simulations stack}
#'   \item{\code{\link{aggregate,TROLLsimstack-method}}}{Splits the data into
#'   subsets, computes summary statistics for each, and returns the result in a
#'   convenient form} }
#'
#' @section Model classes: \describe{ \item{\code{\linkS4class{TROLLsim}}}{S4
#'   class with TROLL outptus for a single simulation.}
#'   \item{\code{\linkS4class{TROLLsimstack}}}{S4 class with TROLL outptus for
#'   a stack of simulations.} }
#'
#' @section Modelling additional  functions: \describe{\item{ToDo}{work in progress}}
#'
#' @docType package
#' @name RconTROLL
NULL
#> NULL