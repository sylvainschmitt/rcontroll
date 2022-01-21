#' @include trollsim.R
#' @include trollstack.R
#' @import methods
#' @import ggplot2
#' @importFrom dplyr filter mutate select
#' @importFrom reshape2 melt
#' @importFrom viridis scale_color_viridis
NULL

#' Plot TROLL outputs
#'
#' This is a method to plot TROLL simulations or stacks, including either
#' temporal trajectories of whole ecosystem or species metrics, or the final
#' pattern observed in the forest community. Metrics includes abundances of
#' individuals above 1cm (abund & N), above 10cm (abu10 & N10), and above 30cm
#' (abu30 & N30), aboveground biomass (agb & AGB), basal area of individuals
#' above 1cm (ba, BA), and above 10cm (ba10, BA10), gross primary production
#' (gpp, GPP), net primary production (npp, NPP), and respiration of day (rday),
#' night (rnight) and stem (rstem).
#'
#' @param object TROLL simulation or stack.
#' @param what char. What to plot: "forest", "ecosystem", or "species".
#' @param variables char. Wich variable(s) to plot (ecosystem or species).
#' @param iter char. Which iteration(s) to plot.
#' @param species char. Which species to plot when using the species table.
#'
#' @return ggplot2 object
#'
#' @examples
#' \dontrun{
#' autoplot(sim)
#' }
#' 
#' @name autoplot.troll
#'   
NULL

#' @export
#' @rdname autoplot.troll
setMethod("autoplot", "trollsim", function(
  object, 
  what = "ecosystem",
  variables = NULL, 
  iter = NULL,
  species = NULL
) {
  # dplyr
  spnames <- Niter <- value <- variable <- dbh <- s_name <- NULL
  
  # check parameters
  if(!(what %in% c("forest", "ecosystem", "species")))
    stop("what should be forest, ecosystem, or species")
  
  # forest
  if(what == "forest"){
    if(!is.null(variables))
      warning("variables parameter is unused when plotting forest.")
    if(!is.null(species))
      warning("species parameter is unused when plotting forest.")
    if(length(unique(object@forest$iter)) == 0)
      stop("The forest table is empty, please use extended outputs with global parameter _OUTPUT_extended.")
    if(all(!(iter %in% unique(object@forest$iter))))
      stop("iteration not available in the forest table, please check.")
    Niter <- iter
    g <- ggplot(filter(object@forest, iter %in% Niter),
           aes(col, row, size = dbh, col = s_name)) +
      geom_point() +
      theme_bw() +
      scale_size_continuous("DBH (m)", range = c(0.1, 3)) +
      scale_color_viridis(guide = "none", discrete = T) +
      coord_equal() +
      xlab("X") + ylab("Y")
  }
  
  # ecosystem
  if(what == "ecosystem"){
    if(is.null(variables))
    variables <- names(object@ecosystem)
    absent <- variables[!(variables %in% names(object@ecosystem))]
    if(length(absent) > 0)
      warning(paste("The following variables are not present in the ecosystem table: ", paste(absent, sep =", ")))
    if(!is.null(species))
      warning("species parameter is unused when plotting ecosystem.")
    ecosystem <- object@ecosystem
    if(!is.null(iter)){
      if(!(iter %in% unique(object@ecosystem$iter)))
        stop("iteration not available in the ecosystem table, please check.")
      Niter <- iter
      ecosystem <- filter(ecosystem, iter %in% Niter)
    }
    g <- ecosystem %>%
      mutate(iter = as.numeric(iter / object@parameters["iterperyear"])) %>% 
      melt("iter") %>%
      filter(variable %in% variables) %>% 
      ggplot(aes(x =iter, y = value)) +
      geom_line() +
      facet_wrap(~variable, scales = "free_y") +
      theme_bw() +
      xlab("Time (year)")
  }
  
  # species
  if(what == "species"){
    if(nrow(object@species) == 0)
      stop("The species table is empty, please use extended outputs with global parameter _OUTPUT_extended.")
    if(is.null(variables))
      variables <- names(object@ecosystem)
    absent <- variables[!(variables %in% names(object@ecosystem))]
    if(length(absent) > 0)
      warning(paste("The following variables are not present in the ecosystem table: ", paste(absent, sep =", ")))
    if(is.null(species))
      species <- object@species$species
    absent <- species[!(species %in% object@species$species)]
    if(length(absent) > 0)
      warning(paste("The following species are not present in the species table: ", paste(absent, sep =", ")))
    sptab <- object@species
    if(!is.null(iter)){
      if(!(iter %in% unique(object@ecosystem$iter)))
        stop("iteration not available in the ecosystem table, please check.")
      Niter <- iter
      sptab <- filter(sptab, iter %in% Niter)
    }
    spnames <- species
    g <- sptab %>%
      mutate(iter = as.numeric(iter / object@parameters["iterperyear"])) %>% 
      melt(c("iter", "species")) %>%
      filter(variable %in% variables) %>% 
      filter(species %in% spnames) %>% 
      ggplot(aes(x =iter, y = value,color = species)) +
      geom_line() +
      facet_wrap(~variable, scales = "free_y") +
      theme_bw() +
      xlab("Time (year)")
  }
  
  return(g)
})

#' @export
#' @name autoplot.troll
setMethod("autoplot", "trollstack", function(
  object, 
  what = "ecosystem",
  variables = NULL, 
  iter = NULL,
  species = NULL
) {
  # dplyr
  spnames <- Niter <- value <- variable <- dbh <- s_name <- NULL
  
  # check parameters
  if(!(what %in% c("forest", "ecosystem", "species")))
    stop("what should be forest, ecosystem, or species")
  
  # forest
  if(what == "forest"){
    if(!is.null(variables))
      warning("variables parameter is unused when plotting forest.")
    if(!is.null(species))
      warning("species parameter is unused when plotting forest.")
    if(length(unique(object@forest$iter)) == 0)
      stop("The forest table is empty, please use extended outputs with global parameter _OUTPUT_extended.")
    if(all(!(iter %in% unique(object@forest$iter))))
      stop("iteration not available in the forest table, please check.")
    Niter <- iter
    g <- ggplot(filter(object@forest, iter %in% Niter),
                aes(col, row, size = dbh, col = s_name)) +
      geom_point() +
      theme_bw() +
      scale_size_continuous("DBH (m)", range = c(0.1, 3)) +
      scale_color_viridis(guide = "none", discrete = T) +
      coord_equal() +
      xlab("X") + ylab("Y") +
      facet_wrap(~ simulation)
  }
  
  # ecosystem
  if(what == "ecosystem"){
    if(is.null(variables))
      variables <- names(object@ecosystem)
    absent <- variables[!(variables %in% names(object@ecosystem))]
    if(length(absent) > 0)
      warning(paste("The following variables are not present in the ecosystem table: ", paste(absent, sep =", ")))
    if(!is.null(species))
      warning("species parameter is unused when plotting ecosystem.")
    ecosystem <- object@ecosystem
    if(!is.null(iter)){
      if(!(iter %in% unique(object@ecosystem$iter)))
        stop("iteration not available in the ecosystem table, please check.")
      Niter <- iter
      ecosystem <- filter(ecosystem, iter %in% Niter)
    }
    g <- ecosystem %>%
      mutate(iter = as.numeric(iter / object@parameters["iterperyear"])) %>% 
      melt(c("iter", "simulation")) %>%
      filter(variable %in% variables) %>% 
      ggplot(aes(x =iter, y = value, col = simulation)) +
      geom_line() +
      facet_wrap(~variable, scales = "free_y") +
      theme_bw() +
      xlab("Time (year)")
  }
  
  # species
  if(what == "species"){
    if(nrow(object@species) == 0)
      stop("The species table is empty, please use extended outputs with global parameter _OUTPUT_extended.")
    if(is.null(variables))
      variables <- names(object@ecosystem)
    absent <- variables[!(variables %in% names(object@ecosystem))]
    if(length(absent) > 0)
      warning(paste("The following variables are not present in the ecosystem table: ", paste(absent, sep =", ")))
    if(is.null(species))
      species <- object@species$species
    absent <- species[!(species %in% object@species$species)]
    if(length(absent) > 0)
      warning(paste("The following species are not present in the species table: ", paste(absent, sep =", ")))
    sptab <- object@species
    if(!is.null(iter)){
      if(!(iter %in% unique(object@ecosystem$iter)))
        stop("iteration not available in the ecosystem table, please check.")
      Niter <- iter
      sptab <- filter(sptab, iter %in% Niter)
    }
    spnames <- species
    if(inherits(object, "trollstack"))
      melt_vars <- c("iter", "species", "simulation")
    else
      melt_vars <- c("iter", "species")
    g <- sptab %>%
      mutate(iter = as.numeric(iter / object@parameters["iterperyear"])) %>% 
      melt(melt_vars) %>%
      filter(variable %in% variables) %>% 
      filter(species %in% spnames) %>% 
      ggplot(aes(x =iter, y = value, color = species, linetype = simulation)) +
      geom_line() +
      facet_wrap(~variable, scales = "free_y") +
      theme_bw() +
      xlab("Time (year)")
  }
  
  return(g)
})

