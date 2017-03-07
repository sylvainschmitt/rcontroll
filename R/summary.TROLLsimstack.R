#' @import methods
NULL

#' Function to provide summary of TROLL outputs
#'
#' @param x TROLLsimstack
#'
#' @return Print in console
#'
#' @export
#'
#' @examples
#'
#' @name summary.TROLLsimstack
#' 
setMethod('summary', 'TROLLsimstack', function(object, ...) {
  
  ##### params ####
  nbiter <- object@nbiter
  iter <- object@iter
  
  ##### summary ####
  
  cat('Object of class :', class(object)[1],'\n\n')
  cat('Structured :', object@structured, '\n')
  cat('Compressed', object@compressed, '\n\n')

  if(isTRUE(object@structured)){  
  cat('\n','*************************************************')
  cat('\n','**** General outputs (multirun means and sd) ****')
  cat('\n','*************************************************','\n\n')
  
  cat('Number of trees (stems/ha):\t\t\t'
    , mean(sapply(object@layers, function(x) return(x@abundances$abund$Total[nbiter])))
    , '\t'
    , sd(sapply(object@layers, function(x) return(x@abundances$abund$Total[nbiter])))
    , '\n')
  cat('Number of trees with dbh > 10 cm (stems/ha):\t'
    , mean(sapply(object@layers, function(x) return(x@abundances$abu10$Total[nbiter])))
    , '  \t'
    , sd(sapply(object@layers, function(x) return(x@abundances$abu10$Total[nbiter])))
    , '\n')
  cat('Number of trees with dbh > 30 cm (stems/ha):\t'
    , mean(sapply(object@layers, function(x) return(x@abundances$abu30$Total[nbiter])))
    , '    \t'
    , sd(sapply(object@layers, function(x) return(x@abundances$abu30$Total[nbiter])))
    , '\n')
  cat('Aboveground biomass (t/ha): \t\t\t'
    , mean(sapply(object@layers, function(x) return(x@agb$Total[nbiter]/1000)))
    , '\t'
    , sd(sapply(object@layers, function(x) return(x@agb$Total[nbiter]/1000)))
    ,'\n')
  } else {
    cat('TROLLsimstack is not structured. No meaningful summary statistics calculated.')
  }
})
