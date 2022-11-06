#' rcontroll
#'
#' TROLL is coded in C++ to efficiently simulate typically hundreds of thousands
#' of individuals over hundreds of years. The rcontroll package is a wrapper of
#' TROLL that includes functions in R that generate inputs and prepare the run,
#' run simulations, and analyse the outputs through tables, figures, and maps
#' taking advantage of the R package ecosystem.
#'
#' @section Pre-simulations: \describe{ Pre-simulation functions include global
#'   parameters definition (generate_parameters function) and climate data
#'   generation (generate_climate function). rcontroll also includes default
#'   data for species and climate inputs for a typical French Guiana rainforest.
#'   The purpose of the generate_climate function with the help of the
#'   corresponding vignette is to calculate TROLL climate inputs from ERA5-Land
#'   (Munoz-Sabater et al. 2021), a global climatic reanalysis dataset (see
#'   Supplementary Information SI1). Therefore, rcontroll users only need
#'   species-specific data to run TROLL simulations anywhere in the tropics. }
#'
#' @section Simulations: \describe{ Simulations are run one at a time using the
#'   troll function or as stacked runs, using the stack function. The outputs of
#'   the two commands are stored in  trollsim or trollstack classes,
#'   respectively; they can be accessed using object attributes (with @ in R) in
#'   the form of simple R objects. trollsim or trollstack objects consist of 7
#'   attributes: (1) simulation name, (2) path to saved files, (3) simulation
#'   parameters, (4) simulation log, (5) initial and final state of the forest,
#'   (6) ecosystem metrics, and (7) species metrics.  The initial and final
#'   states of the simulated forest are represented by a table that records the
#'   spatial position, size and other relevant traits of all trees at the
#'   beginning and end of the simulation. The ecosystem and species metrics are
#'   summaries of ecosystem processes and states , such as  net primary
#'   productivity and aboveground biomass, and provided both at species-level
#'   and aggregated over the entire stand. Simulations can be saved using a
#'   user-defined path when run and later loaded as a simple simulation
#'   (load_output function) or a stack of simulations (load_stack function). }
#'
#' @section Post-simulations: \describe{ rcontroll also includes
#'   post-simulations functions. Simulation outputs can be retrieved simply from
#'   trollsim or trollstack objects and summarised or plotted in the R
#'   environnement with the print, summary and autoplot functions. In addition,
#'   a function is available to visualise TROLL simulations as an animated
#'   figure (autogif function, Fig. 1) for research, demonstration or
#'   educational purposes. }
#'
#' @section TROLL: \describe{version 3.1.3}
#'
#' @docType package
#' @name rcontroll
#'   
NULL
# > NULL
