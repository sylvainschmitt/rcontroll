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

  ##### params ####
  nbiter <- object@par$general$nbiter
  iter <- object@par$general$iter
  SpeciesNb <- as.integer(object@info$SpeciesNb)
  age <- round(nbiter / iter, 1) # in years
  surf <- prod(object@info$step * object@info$SitesNb) / 10000 # in ha
  
  ##### data ####
  sortabund <- t(sort(data.frame(object@abundances$relabu10[nbiter,1:SpeciesNb]),decreasing=T))
  
  ##### summary ####
  
  cat('Object of class :', class(object)[1],'\n')
  cat('Name :', object@name, '\n')
  
  cat('\n','*************************')
  cat('\n','**** General outputs ****')
  cat('\n','*************************','\n\n')
  
  cat('Total number of trees (stems/ha):\t\t',object@abundances$abund$Total[nbiter],'\n')
  cat('Number of trees with dbh > 10 cm (stems/ha):\t',object@abundances$abu10$Total[nbiter],'\n')
  cat('Number of trees with dbh > 30 cm (stems/ha):\t',object@abundances$abu30$Total[nbiter],'\n')
  cat('Aboveground biomass (t/ha):\t\t\t', object@agb$Total[nbiter]/1000, '\n')
  if(nbiter>12){
    cat('Aboveground biomass relative change (%):\t', 
      100*mean((object@agb$Total[(nbiter-11):nbiter]-object@agb$Total[(nbiter-12):(nbiter-1)])/object@agb$Total[(nbiter-11):nbiter]), 
      '\n')
    cat('(avg over last 12 iterations)\n')
    }

  cat('\n','*****************************')    
  cat('\n','**** Forest architecture ****')
  cat('\n','*****************************','\n\n')  
  
  cat('Maximum realized tree height (m):\t\t', max(object@final_pattern$height), '\n')
  cat('Mean canopy height (m):\t\t\t\t', 'not yet implemented', '\n')

  cat('\n','**********************')  
  cat('\n','**** Biodiversity ****')
  cat('\n','**********************','\n\n')

  if(SpeciesNb>=5) {
    cat('5 most dominant species (of ', SpeciesNb, '):\t\t ',rownames(sortabund)[1],' (',round(sortabund[1],1),' %)\n',sep="")
    for(i in 2:5){cat('\t\t\t\t\t\t ',rownames(sortabund)[i],' (',round(sortabund[i],1),' %)\n',sep="")}
  }
  cat('Simpson diversity index (D):\t\t\t',1/sum((object@abundances$relabu10[nbiter,]/100)^2), '\n')
  cat('Mean wood density:\t\t\t\t',sum(object@abundances$relabu10[nbiter,]/100*object@sp_par$wsg),'\n\n')
  cat('BEWARE: Biodiversity statistics are calculated for trees with dbh > 10 cm!', '\n') 
  
})

#### Needs to include

# biomass change (avg, sd) over last 100 iterations (if possible)
# nb of seedlings in seedbank
# Simpson index
#
