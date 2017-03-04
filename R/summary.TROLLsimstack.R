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

setMethod('summary', 'TROLLsimstack', function(object, ...) {
# 
# cat('Object of class :', class(x)[1],'\n\n')
# 
# cat('Name :', x@name, '\n\n')

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
})
