#' rcontroll
#' 
#' rcontroll integrates the individual-based and spatially-explicit TROLL model
#' to simulate forest ecosystem and species dynamics forward in time. rcontroll
#' provides user-friendly functions to set up and analyse simulations with
#' varying community compositions, ecological parameters, and climate
#' conditions.
#'
#' @section Section Description: \describe{The rcontroll package relies on a few
#'   functions in R to generate and provide inputs, prepare and run the
#'   simulations, and analyze the simulations through tables, figures, and maps
#'   that are easily connected to the rest of the R ecosystem. The whole
#'   workflow can even be run for one or several simulations using a unique
#'   function (troll and stack). Pre-simulations functions include global
#'   parameters definition (generate_parameters) and species and weather data
#'   input, with default values for French Guiana included in the package.
#'   Simulations are run alone (troll) or as a stack (stack) and stocked in
#'   corresponding classes. The post-simulation outputs are stored in the
#'   corresponding objects and can be accessed using object attributes (with @
#'   in R) in the form of simple R objects, mainly data frames, or summarized
#'   and plotted with the print, summary and autoplot methods. Simulations can
#'   be saved using a user-defined path when run and later loaded as a simple
#'   simulation (load_output) or a stack of simulations (load_stack).}
#'
#' @docType package
#' @name rcontroll
NULL
# > NULL
