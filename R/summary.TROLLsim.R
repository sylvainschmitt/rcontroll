#' @import methods
NULL

#' Function to provide summary of TROLL outputs
#'
#' @param x TROLLsim
#'
#' @return Print in console
#'
#' @export
#'
#' @examples
#'
#' @name summary.TROLLsim
#' 


setMethod('summary', 'TROLLsim', function(object, ...) {
  
  # cat('Object of class :', class(x)[1],'\n\n')
  # 
  # cat('Name :', x@name, '\n\n')
  # 
  # cat('2D discrete network: horizontal step = ', object@info$step, 'm, one tree per 1 m^2 \n')
  # cat('Number of sites      : ', object@info$SitesNb[1], 'object', object@info$SitesNb[2], '\n')
  # cat('Number of iterations : ', object@info$IterationsNb, '\n')
  # cat('Duration of timestep : ', object@info$timestep, 'years \n')
  # cat('Number of Species    : ', object@info$SpeciesNb, '\n\n')
  # 
  # cat('Average computation time : ', object@info$ComputationTime, 'seconds \n\n')
})

#### Needs to include

# launched from zero or not
# average biomass per hectare
# biomass change (avg, sd) over last 100 iterations (if possible)
# nb of seedlings in seedbank
# 5 most dominant species, 5 least dominant species (with live individual)
# nb of stems per hectare (total, 10, 30)
# Simpson index
# etc. (everything from Isabelle)
# maximum realized tree height

#
