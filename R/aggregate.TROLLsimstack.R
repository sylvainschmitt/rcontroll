#' aggregate
#' 
#' Splits the data into subsets, computes summary statistics for each, and
#' returns the result in a convenient form.
#' 
#' @param x TROLLsimstack. TROLLsimstack to aggregate
#' @param add logical. should the results be added to the existing object
#' @param force logical. should we force the aggregation (can cause error)
#' @param ... unused argument
#'   
#' @return aggregated TROLL simstack
#' 
#' @export
#' 
#' @examples
#' 
#' @name aggregate.TROLLsimstack
#' 
setMethod("aggregate", signature(x='TROLLsimstack'), function(x, add = FALSE, force = FALSE, ...) {
  # check correspondance between all parameters
  if(!force){
    ref <- x@layers[[1]]
    if(!all(unlist(lapply(x@layers, function(sim)
      all(mapply(.check_list, sim@par, ref@par), 
          .check_list(sim@sp_par, ref@sp_par))
    ))))
      stop('All simulations of the stack should have the same parameters !')
  }
  
  # aggregate
  y <- list(
    mean = .aggregate_fun(x, mean),
    # var = .aggregate_fun(x, var),
    min = .aggregate_fun(x, min),
    max = .aggregate_fun(x, max)
  )
  
  if(!add)
    y <- stack(y) # aggregate class
  if(add)
    x@layers <- c(x@layers, y)
  
  y@aggregated <- TRUE
  return(y)
})

.check_list <- function(x, y){ # Check correspondance btw two lists
  all(unlist(mapply(function(a, b){
    a == b
  },a = x, b= y)))
}

.aggregate_fun <- function(x, FUN){ # Aggregate a simulation stack following a function
  y <- TROLLsim(
    name = as.character(match.call())[3],
    abundances = list(
      abund = .aggregate_simstack_df(x, FUN, 'abundances', list = 'abund'),
      abu10 = .aggregate_simstack_df(x, FUN, 'abundances', list = 'abu10'),
      abu30 = .aggregate_simstack_df(x, FUN, 'abundances', list = 'abu30'),
      relabdund = .aggregate_simstack_df(x, FUN, 'abundances', list = 'relabdund'),
      relabu10 = .aggregate_simstack_df(x, FUN, 'abundances', list = 'relabu10'),
      relabu30 = .aggregate_simstack_df(x, FUN, 'abundances', list = 'relabu30')
    ),
    agb = .aggregate_simstack_df(x, FUN, 'agb'),
    ba = list(
      ba = .aggregate_simstack_df(x, FUN, 'ba', list = 'ba'),
      ba10 = .aggregate_simstack_df(x, FUN, 'ba', list = 'ba10')
    ),
    # death = list(
    #   death = .aggregate_simstack_df(x, FUN, 'death', list = 'death'),
    #   death1 = .aggregate_simstack_df(x, FUN, 'death', ref = 1, list = 'death1'),
    #   death2 = .aggregate_simstack_df(x, FUN, 'death', ref = 1, list = 'death2'),
    #   death3 = .aggregate_simstack_df(x, FUN, 'death', ref = 1, list = 'death3'),
    #   deathrate = .aggregate_simstack_df(x, FUN, 'death', list = 'deathrate')
    # ),
    gpp = .aggregate_simstack_df(x, FUN, 'gpp'),
    info = list(
      step = x@layers[[1]]@info$step,
      SitesNb = x@layers[[1]]@info$SitesNb,
      IterationsNb = x@layers[[1]]@info$IterationsNb,
      timestep = x@layers[[1]]@info$timestep,
      SpeciesNb = x@layers[[1]]@info$SpeciesNb,
      ComputationTime = FUN(as.numeric(sapply(x@layers, function(y){y@info$ComputationTime})))
    ),
    litterfall = .aggregate_simstack_df(x, FUN, 'litterfall'),
    npp = .aggregate_simstack_df(x, FUN, 'npp'),
    par = x@layers[[1]]@par,
    paramspace = x@layers[[1]]@paramspace,
    ppfd0 = .aggregate_simstack_df(x, FUN, 'ppfd0', 1:2),
    R = list(
      Rday = .aggregate_simstack_df(x, FUN, 'R', list = 'Rday'),
      Rnight = .aggregate_simstack_df(x, FUN, 'R', list = 'Rnight')
    ),
    sp_par = x@layers[[1]]@sp_par,
    vertd = .aggregate_simstack_df(x, FUN, 'vertd', 1)
  )
}

.aggregate_simstack_df <- function(x, FUN, slot, ref = NULL, list = NULL){
  if(is.null(list))
    y <- slot(x@layers[[1]], slot)
  if(!is.null(list))
    y <- slot(x@layers[[1]], slot)[[list]]
  if(is.null(ref))
    cols <- 1:dim(y)[2]
  if(!is.null(ref))
    cols <- (1:dim(y)[2])[-ref]
  if(is.null(list))
    y[cols] <- sapply(cols, function(col){
      apply(
        sapply(x@layers, function(y){
          slot(y, slot)[,col]
        }), 1, FUN)
    })
  if(!is.null(list))
    y[cols] <- sapply(cols, function(col){
      apply(
        sapply(x@layers, function(y){
          slot(y, slot)[[list]][,col]
        }), 1, FUN)
    })
  if(is.null(list))
    names(y) <- names(slot(x@layers[[1]], slot))
  if(!is.null(list))
    names(y) <- names(slot(x@layers[[1]], slot)[[list]])
  return(y)
}